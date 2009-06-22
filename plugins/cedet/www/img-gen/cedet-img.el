;;; cedet-img.el --- Create images for the CEDET Web site
;;
;; Copyright (C) 2009 Eric M. Ludlam
;;
;; Author: Eric M. Ludlam <eric@siege-engine.com>
;;
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
;; Scripts that use Emacs features to create images for the CEDET website.
;;
;; To use, start Emacs 23 or later with no default color settings:
;;
;; emacs -q
;;
;; Then load this file, and run some of the commands.  Use a
;; screen-cap tool to then get the images.
;;

;;; Code:
(setq scalable-fonts-allowed t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(line-number-mode -1)
(setq default-indicate-buffer-boundaries t)
(setq frame-title-format "emacs@siege-engine.com")

(defvar cig-dir (let* ((cdir (file-name-directory (or (locate-library "cedet.el")
						      "~/cedet/common/cedet.el")
						  ))
		       (rdir (file-name-directory (directory-file-name cdir)))
		       )
		  rdir)
  "The path the to CEDET.")

(when (not (featurep 'cedet))
  (load-file (expand-file-name "common/cedet.el" cig-dir))
  (semantic-load-enable-code-helpers)
  )

(defface cig-logo-face
  '((t :family "Arial"
       :height 960
       :weight bold
       :width condensed
       :foreground "#338833"
       ))
  "Face used for the CEDET Logo.")
	   

(defun cig-logo ()
  "Create a CEDET Logo buffer."
  (interactive)
  (switch-to-buffer
   (get-buffer-create "Collection of Emacs Development Environment Tools"))
  (erase-buffer)
  (insert "-*-CEDET-*-")
  (let ((ol (make-overlay (point-min) (point-max))))
    (overlay-put ol 'face 'cig-logo-face)
    )
  (goto-char (point-max))
  (insert "  \n")
  )
  
(defun cig-smart-completion ()
  "Show some smart completion examples."
  (interactive)
  (switch-to-buffer
   (find-file-other-window (expand-file-name "semantic/tests/testtemplates.cpp" cig-dir)))
  (goto-char (point-min))
  (re-search-forward "-> // -2-")
  (goto-char (+ 2 (match-beginning 0)))
  (let ((semantic-complete-inline-analyzer-displayor-class
	 'semantic-displayor-tooltip)
	(ctxt (semantic-analyze-current-context))
	)
    (semantic-complete-inline-analyzer ctxt)
    ))

(require 'srecode-insert)
(setq srecode-insert-with-fields-p t)

(defun cig-srecode ()
  "Show template insertion."
  (interactive)
  (switch-to-buffer
   (find-file-other-window "/tmp/demo.hpp"))
  (erase-buffer)
  (srecode-load-tables-for-mode major-mode)
  (srecode-minor-mode 1)
  (srecode-insert "file:empty")
  (require 'srecode-fields)
  (let ()
    (srecode-insert "declaration:class"))
  )

(defun cig-symref ()
  "Show a symref buffer."
  (interactive)
  (switch-to-buffer
   (find-file-other-window "~/src/global-5.7.3/libutil/checkalloc.c"))
  (semantic-go-to-tag
   (car
    (semantic-find-tags-by-name "check_malloc" (current-buffer))))
  (forward-line 2)
  (semantic-symref)
  )
  
(defun cig-cogre ()
  "Show a UML class."
  (interactive)
  (switch-to-buffer
   (find-file-other-window (expand-file-name "cogre/cogre.el" cig-dir)))
  (semantic-go-to-tag
   (car
    (semantic-find-tags-by-name "cogre-node" (current-buffer))))
  (require 'cogre)
  (require 'cogre-uml)
  (cogre-uml-enable-unicode)
  (cogre-uml-quick-class "cogre-node")
  )

(provide 'cedet-img)
;;; cedet-img.el ends here
