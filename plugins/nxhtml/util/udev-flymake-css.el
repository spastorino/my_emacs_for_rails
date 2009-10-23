;;; udev-flymake-css.el ---
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2009-09-11 Fri
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(defgroup udev-fmcss nil
  "Customization group for udev-fmcss."
  :group 'nxhtml)

(defcustom udev-fmcss-dir "~/.emacs.d/udev/fmcss-dl/"
  "Directory where to put flymake css source files."
  :type 'directory
  :group 'udev-cedet)

(defvar udev-fmcss-update-buffer nil)

(defvar udev-fmcss-steps
  '(udev-fmcss-fetch
    ;;udev-cedet-fetch-diff
    ;;udev-cedet-check-diff
    ;;udev-cedet-install
    ))

;;;###autoload
(defun udev-fmcss-update ()
  "Fetch and install flymake css from sources.
To determine where to store the sources see `udev-fmcss-dir'."
  (interactive)
  (let* ((has-it (file-exists-p (udev-fmcss-validator-file)))
         (prompt (if has-it
                     "Do you want to update flymake css from sources? "
                   "Do you want to install flymake css from sources? ")))
    (when (y-or-n-p prompt)
      (setq udev-fmcss-update-buffer
            (udev-call-first-step "*Update flymake css*"
                                  ;;udev-cedet-update-buffer
                                  udev-fmcss-steps
                                  "Starting updating flymake css from sources"
                                  nil ;'udev-cedet-setup-when-finished
                                  )))))

(defun udev-fmcss-validator-file ()
  (expand-file-name "css-validator.jar" (file-name-as-directory udev-fmcss-dir)))

(defun udev-fmcss-fetch (log-buffer)
  "Fetch flymake css sources (asynchronously)."
  (let ((default-directory (file-name-as-directory udev-fmcss-dir)))
    (unless (file-directory-p default-directory)
      (when (yes-or-no-p (concat "Directory " default-directory " does not exist. Create it? "))
        (make-directory default-directory t)))
    (url-handler-mode 1)
    (when (file-directory-p default-directory)
      (with-current-buffer log-buffer
        (copy-file
         "http://www.w3.org/QA/Tools/css-validator/css-validator.jar"
         (udev-fmcss-validator-file))
        (current-buffer)))))

;; FlymakeCSS

;; Get http://www.w3.org/QA/Tools/css-validator/css-validator.jar create a directory named ‘lib’ in the same directory. The ‘lib’ dir should contain jars:

;;     * commons-collections-3.2.1.jar
;;     * jigsaw.jar
;;     * velocity-1.6.1.jar
;;     * xml-apis.jar
;;     * commons-lang-2.4.jar
;;     * tagsoup-1.2.jar
;;     * xercesImpl.jar

;; From:

;;     * http://jigsaw.w3.org/Distrib/jigsaw_2.2.6.tar.gz
;;     * http://www.apache.org/dist/commons/collections/binaries/commons-collections-3.2.1-bin.tar.gz
;;     * http://www.apache.org/dist/commons/lang/binaries/commons-lang-2.4-bin.tar.gz
;;     * http://www.apache.org/dist/velocity/engine/1.6.1/velocity-1.6.1.tar.gz
;;     * http://www.apache.org/dist/xerces/j/Xerces-J-bin.2.9.1.tar.gz
;;     * http://home.ccil.org/~cowan/XML/tagsoup/tagsoup-1.2.jar

;; Test validating some CSS file by running:

;;     java -jar css-validator.jar file:somecssfile.css

;; If it works, add this to a suitable place loaded by emacs:

;; ;; Edit this
;; (defconst css-validator "java -jar ~/bin/css-validator.jar")

;; (defun flymake-css-init ()
;;   (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                        'flymake-create-temp-inplace))
;;          (local-file  (file-relative-name
;;                        temp-file
;;                        (file-name-directory buffer-file-name))))
;;     (list css-validator (list "-output gnu" (concat "file:" local-file)))))

;; (push '(".+\\.css$" flymake-css-init) flymake-allowed-file-name-masks)

;; (push '("^file:\\([^:]+\\):\\([^:]+\\):\\(.*\\)" 1 2 nil 3) flymake-err-line-patterns)

;; FlyMake

;; Thanks for this concise and complete tutorial on installing the W3 CSS Validator. It took me about 10 minutes to install it, following these instructions :)

;; One note though: on Windows, I had trouble with

;;    (defconst css-validator "java -jar ~/path/to/css-validator.jar")

;; It appears that perhaps Java does not like all the arguments being passed as a single string[1]. Maybe this is a Windows-specific thing?

;; Anyway I wound up leaving out the line above. Instead, I changed the last line of the first stanza of the config as follows:

;;     (defun flymake-css-init ()
;;       (let* ((temp-file   (flymake-init-create-temp-buffer-copy
;;                            'flymake-create-temp-inplace))
;;              (local-file  (file-relative-name
;;                            temp-file
;;                            (file-name-directory buffer-file-name))))
;;         ;; Edit this line
;;         (list "java" (list "-jar" "c:/path/to/css-validator.jar" "-output" "gnu" (concat "file:" local-file)))))

;; Otherwise everything worked perfectly, thanks again. —NoahSussman


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; udev-flymake-css.el ends here
