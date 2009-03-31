;;; ert-functional.el --- Functional Emacs Lisp Regression Testing Helpers

;; Copyright (C) 2008 Phil Hagelberg

;; Author: Phil Hagelberg
;; Version: 0.2
;; Keywords: lisp, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;;; Commentary:

;; This file includes some extra higher-level helper functions to use
;; while writing automated tests with ert.el. This includes extra
;; predicates and buffer-management functions.

;; For the purposes of ERT, unit tests are defined as tests that just
;; check return values, and functional tests are anything higher-level
;; than that.

;; Since it is not meant to be loaded during normal use, this file
;; includes functions that are not prefixed for readability's sake.

;;; Code:

(defmacro buffer-changes-p (&rest body)
  "Return t if the body changes the buffer contents."
  `(let ((buffer-changed-init-value (buffer-string)))
     (unwind-protect (progn ,@body)
       (string= buffer-changed-init-value
                (buffer-string)))))

(defun buffer-contains-p (regexp &optional buffer)
  "Return t if contents of buffer (defaults to current) matches regexp."
  (save-excursion
    (if buffer (switch-to-buffer buffer))
    (not (not (search-forward-regexp regexp nil t)))))

(defun correctly-indented-p (filename)
  "Returns t if the buffer is already indented the way Emacs would indent it."
  (save-excursion
    (find-file filename)
    (let ((buffer-original-indentation (buffer-string))
          (kill-buffer-query-functions nil))
      (indent-region (point-min) (point-max))
      (let ((buffer-new-indentation (buffer-string)))
        (revert-buffer nil t)
        (kill-buffer nil)
        (string= buffer-original-indentation buffer-new-indentation)))))

(defun ert-test-buffer-substitute (string fn)
  "Removes the all occurrences of STRING in the buffer
and runs FN with at that point each one is removed.

Backslash-escaped STRINGs are unescaped and ignored."
  (let ((len (length string)))
    (save-excursion
      (beginning-of-buffer)
      (while (search-forward string nil t)
        (save-excursion
          (backward-char len)
          (if (eq (char-before (point)) ?\\) (delete-char -1)
            (delete-char len)
            (funcall fn)))))))

(defmacro with-test-buffer (contents &rest body)
  "Runs BODY in a buffer containing CONTENTS.

The mark may be set in the buffer using the string \"<mark>\".
This can be escaped with a backslash to unclude it literally."
  `(with-temp-buffer
     (insert ,contents)
     (beginning-of-buffer)

     (let ((new-mark))
       (ert-test-buffer-substitute "<mark>" (lambda () (setq new-mark (point))))
       (set-mark new-mark))

     (let ((new-point (point)))
       (ert-test-buffer-substitute "<point>" (lambda () (setq new-point (point))))
       (goto-char new-point))
     ,@body))
(put 'with-test-buffer 'lisp-indent-function 1)

(provide 'ert-functional)
;;; ert-functional.el ends here