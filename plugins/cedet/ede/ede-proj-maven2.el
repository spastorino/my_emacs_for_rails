;; Copyright (C) 2008, 2009 Eric M. Ludlam

;; Author: Eric M. Ludlam <eric@siege-engine.com>
;; Joakim Verona <joakim@verona.se>
;; X-RCS: $Id: ede-proj-maven2.el,v 1.5 2009/03/19 19:37:55 joakimv Exp $

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;

;; Notes: 

;; - Maven2 is very different from Maven1, so this file wont be
;; useful for Maven1 projects.

;; - Currently the maven2 ede support enables you to open a file in a
;;  maven2 project and build the project with "mvn install".

;; Introduction:

;; Projects that build with Maven2 have a pom.xml file that defines
;; the project.  Maven projects are used for Java projects, and have
;; many differences from Make projects. (Ant is another Java build
;; tool that is more like a Make project.)  With Maven2 you describe a
;; projects dependencies explicitly, and other things like which files
;; to compile is normaly deducted implicitly.

;; Maven2 projects can consist of any file types, but mostly java, xml
;; files(with different file name extensions), and Java property
;; files.

;; Maven2 + Emacs is a good combination, because team members can
;; choose to use Eclipse + Maven2, or Emacs + Maven2, and both can use
;; the workflow their comfortable with.


;; Implementation discussion:

;; Its not totaly clear how ede:s model map on maven2 projects, OTOH,
;; the basic useful functionality is simple:
;; - Identify the project root; the directory where the pom.xml file is
;; - Find source: <proj root>/src/<main|test>/<java|whatever/...
;; - Execute "mvn install" in a project root, to build the project


;;TODO

;; 
;; BUG: (this bug-description is somewhat based on a missunderstanding
;; of EDE, FIXME) in projects with a root pom and modules with poms
;; residing in child directories of the root pom, "compile" sometimes
;; build the root project rather than the child project. This is not
;; what we want for maven.  maybe its a feature for makefile projects?

;;to reproduce:
;; - use a hierarchical maven project:
;;root/pom.xml
;;root/A/pom.xml
;;root/B/pom.xml
;; - open root/A/pom.xml, and build with ede. it will build the right project
;; - open root/pom.xml and build
;; - open root/B/pom.xml and ede build. it will build root/pom.xml instead!

;;project-compile-project seems to get the wrong directory to build
;;because ede-compile-project seems to travel up through a project
;;hierarchy and build the root project, rather than the current one.

;;should  ede-parent-project be made generic?

;; In the end more things are desired from proper emacs maven support.

;; - Create new maven2 projects with maven2 from archetypes

;; - Handle maven modules.

;; - make cedet know about the maven project source and class paths.

;; - handle maven build profiles.

;; - ede targets == maven goals ?
;; if so, maven projects will have a "install" goal to start with.
;; all files below the maven src dir, will belong to this target.
;; (this will be true also if we add more "targets" like mavens, "compile" goal.)
;; all src files will belong to all maven targets.

;; - an auxilary project file, like ede-simple could be an useful option

(require 'ede)
(require 'cedet-files)

;;; Code:

;; Because there is one root project file, and no sub project files,
;; we need a special root-finding function.

;;;###autoloa d
(defun ede-maven2-project-root (&optional dir)
  "Get the root directory for DIR."
  (when (not dir) (setq dir default-directory))
  (let ((ans nil))
    (while (and dir (not ans))
      (when (file-exists-p (expand-file-name "pom.xml" dir))
	(setq ans dir))
      (setq dir (ede-up-directory dir)))
    ans))

(defvar ede-maven2-project-list nil
  "List of projects created by option `ede-proj-maven2'.")

(defun ede-maven2-file-existing (dir)
  "Find a maven project in the list of maven projects.
DIR is the directory to search from."
  ;;TODO this is cloned from ede-emacs-file-existing, should be refactorable
  (let ((projs ede-maven2-project-list)
	(ans nil))
    (while (and projs (not ans))
      (let ((root (ede-project-root-directory (car projs))))
	(when (string-match (concat "^" (regexp-quote root)) dir)
	  (setq ans (car projs))))
      (setq projs (cdr projs)))
    ans))


;;;###autoloa d
(defun ede-maven2-load (dir &optional rootproj)
  "Return a Maven Project object if there is a match.
Return nil if there isn't one.
Argument DIR is the directory it is created for.
ROOTPROJ is nil, since there is only one project."
  (or (ede-maven2-file-existing dir)
      ;; Doesn't already exist, so lets make one.
       (let ((this
             (ede-maven2-project "Maven"
                                 :name "maven dir" ; make fancy name from dir here.
                                 :directory dir
                                 :file
				 (expand-file-name "pom.xml" dir)
				 )))
         ;; (message "adding %s to global proj list" this)
         (ede-add-project-to-global-list this)
         ;;TODO the above seems to be done somewhere else, maybe ede-load-project-file
         ;; this seems to lead to multiple copies of project objects in ede-projects
         this
        )
      )
)


(defclass ede-maven2-target-java (ede-target)
  ()
  "EDE Maven Project target for Java code.
All directories need at least one target.")


(defclass ede-maven2-target-misc (ede-target)
  ()
  "EDE Maven Project target for Misc files.
All directories need at least one target.")

;;;###autoloa d
(defclass ede-maven2-project (ede-project)
  ((file-header-line :initform ";; EDE Maven2 project wrapper")
   )
  "Project Type for Maven2 based Java projects."
  :method-invocation-order :depth-first)


(defmethod initialize-instance ((this ede-maven2-project)
				&rest fields)
  "Make sure the :targets is setup."
  (call-next-method)
  (unless (slot-boundp this 'targets)
    (oset this :targets nil))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;the 3 compile methods below currently do much the same thing.
;;  - 1st one tries to find the "root project" and compile it
;;  - 2nd 2 compiles the child project the current file is a member of
;;maven error messages are recognized by emacs23

(defmethod project-compile-project ((obj ede-maven2-project) &optional command)
  "Compile the entire current project OBJ.
Argument COMMAND is the command to use when compiling."
  ;; we need to be in the proj root dir for this to work
  (let ((default-directory (ede-project-root-directory obj)))
    (compile "mvn install")))


(defmethod project-compile-target ((obj ede-maven2-target-java) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (let* ((default-directory (ede-maven2-project-root (oref obj :path))))
    (compile "mvn install")))

(defmethod project-compile-target ((obj ede-maven2-target-misc) &optional command)
  "Compile the current target OBJ.
Argument COMMAND is the command to use for compiling the target."
  (let* ((default-directory (ede-maven2-project-root (oref obj :path))))
    (compile "mvn install")))

;;; File Stuff
;;
 (defmethod ede-project-root-directory ((this ede-maven2-project)
 				       &optional file)
   "Return the root for THIS Maven project with file."
;;   (file-name-directory (oref this file))
   (oref this :directory)
   )
   

 (defmethod ede-project-root ((this ede-maven2-project))
   "Return my root."
   this)

 (defmethod ede-find-subproject-for-directory ((proj ede-maven2-project)
 					      dir)
   "Return PROJ, for handling all subdirs below DIR."
   proj)

;;; TARGET MANAGEMENT
;;

;;im not sure maven targets should look like this at all.
;;

(defun ede-maven2-find-matching-target (class dir targets)
  "Find a target that is a CLASS and is in DIR in the list of TARGETS."
  (let ((match nil))
    (dolist (T targets)
      (when (and (object-of-class-p T class)
		 (string= (oref T :path) dir))
	(setq match T)
      ))
    match))

(defmethod ede-find-target ((proj ede-maven2-project) buffer)
  "Find an EDE target in PROJ for BUFFER.
If one doesn't exist, create a new one for this directory."
  (let* ((ext (file-name-extension (buffer-file-name buffer)))
	 (cls (cond ((not ext)
		     'ede-maven2-target-misc)
		    ((string-match "java" ext)
		     'ede-maven2-target-java)
		    (t 'ede-maven2-target-misc)))
	 (targets (oref proj targets))
	 (dir default-directory)
	 (ans (ede-maven2-find-matching-target cls dir targets))
	 )
    (when (not ans)
      (setq ans (make-instance 
		 cls 
		 :name (file-name-nondirectory
			(directory-file-name dir))
		 :path dir
		 :source nil))
      (object-add-to-list proj :targets ans)
      )
    ans))

;;; UTILITIES SUPPORT.
;;

;;;###autoloa d
(add-to-list 'ede-project-class-files
	     (ede-project-autoload "maven2"
	      :name "MAVEN2"
	      :file 'ede-proj-maven2
	      :proj-file "pom.xml"
	      :proj-root 'ede-maven2-project-root
	      :load-type 'ede-maven2-load
	      :class-sym 'ede-maven2-project
	      :new-p nil)
	     t)

(provide 'ede-proj-maven2)

;;; ede-proj-maven2.el ends here

