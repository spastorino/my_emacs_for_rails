;;; anything-rcodetools.el --- accurate Ruby method completion with anything
;; $Id: anything-rcodetools.el,v 1.6 2008/01/14 17:59:34 rubikitch Exp $

;;; Copyright (c) 2007 rubikitch

;; Author: rubikitch <rubikitch@ruby-lang.org>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/download/anything-rcodetools.el

;;; Use and distribution subject to the terms of the Ruby license.

;;; Commentary:

;; (0) You need rcodetools, anything.el and FastRI. Note that you do not have to
;;     configure anything.el if you use anything.el for this package.
;; (1) You need to add to .emacs:
;;       (require 'anything)
;;       (require 'anything-rcodetools)
;;       ;; Command to get all RI entries.
;;       (setq rct-get-all-methods-command "PAGER=cat fri -l")
;;       ;; See docs
;;       (define-key anything-map "\C-z" 'anything-execute-persistent-action)

;;; History:

;; $Log: anything-rcodetools.el,v $
;; Revision 1.6  2008/01/14 17:59:34  rubikitch
;; * uniform format (anything-c-source-complete-ruby, anything-c-source-complete-ruby-all)
;; * rename command: anything-c-ri -> anything-rct-ri
;;
;; Revision 1.5  2008/01/13 17:54:04  rubikitch
;; anything-current-buffer advice.
;;
;; Revision 1.4  2008/01/08 14:47:34  rubikitch
;; Added (require 'rcodetools).
;; Revised commentary.
;;
;; Revision 1.3  2008/01/04 09:32:29  rubikitch
;; *** empty log message ***
;;
;; Revision 1.2  2008/01/04 09:21:23  rubikitch
;; fixed typo
;;
;; Revision 1.1  2008/01/04 09:21:05  rubikitch
;; Initial revision
;;

;;; Code:

(require 'rcodetools)

;;;; Compatibility code
(unless (fboundp 'anything-execute-persistent-action)
  (defun anything-execute-persistent-action ()
    "If a candidate was selected then perform the associated action without quitting anything."
    (interactive)
    (save-selected-window
      (select-window (get-buffer-window anything-buffer))
      (select-window (setq minibuffer-scroll-window
                           (if (one-window-p t) (split-window) (next-window (selected-window) 1))))
      (let* ((anything-window (get-buffer-window anything-buffer))
             (selection (if anything-saved-sources
                            ;; the action list is shown
                            anything-saved-selection
                          (anything-get-selection)))
             (default-action (anything-get-action))
             (action (assoc-default 'persistent-action (anything-get-current-source))))
        (setq action (or action default-action))
        (if (and (listp action)
                 (not (functionp action))) ; lambda
            ;; select the default action
            (setq action (cdar action)))
        (set-window-dedicated-p anything-window t)
        (unwind-protect
            (and action selection (funcall action selection))
          (set-window-dedicated-p anything-window nil))))))

(unless (boundp 'anything-current-buffer)
  (defvar anything-current-buffer nil)
  (defadvice anything (before get-current-buffer activate)
    (setq anything-current-buffer (current-buffer))))

;;;; Main code
(defun anything-rct-ri (pair)
  (ri (substring (cadr (split-string pair "\t")) 1 -1)))

(defun anything-rct-complete  (pair)
  (save-excursion
    (set-buffer anything-current-buffer)
    (search-backward pattern)
    (delete-char (length pattern)))
  (insert (car (split-string pair "\t"))))

(setq rct-complete-symbol-function 'rct-complete-symbol--anything)
(defvar anything-c-source-complete-ruby
  '((name . "Ruby Method Completion")
    (candidates . rct-method-completion-table)
    (init
     . (lambda ()
         (condition-case x
             (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles")
           ((error) (setq rct-method-completion-table nil)))))
    (action
     ("Completion" . anything-rct-complete)
     ("RI" . anything-rct-ri))
    (volatile)
    (persistent-action . anything-rct-ri)))

(defvar rct-get-all-methods-command "PAGER=cat fri -l")
(defun rct-get-all-methods ()
  (interactive)
  (setq rct-all-methods
        (mapcar (lambda (fullname)
                  (replace-regexp-in-string "^.+[:#.]\\([^:#.]+\\)$"
                                            "\\1\t[\\&]" fullname))
                (split-string (shell-command-to-string rct-get-all-methods-command) "\n"))))

(defvar rct-all-methods (rct-get-all-methods))
(defvar anything-c-source-complete-ruby-all
  '((name . "Ruby Method Completion (ALL)")
    (candidates
     . (lambda ()
        (let ((case-fold-search nil)
              (re (format "[:#.]%s" (with-current-buffer anything-current-buffer
                                   (symbol-at-point)))))
          (remove-if-not
           (lambda (meth) (string-match re meth))
           rct-all-methods))))
    (action
     ("Completion" . anything-rct-complete)
     ("RI" . anything-rct-ri))
    (persistent-action . anything-rct-ri)))

(defun rct-complete-symbol--anything ()
  (interactive)
  (let ((anything-sources (list anything-c-source-complete-ruby anything-c-source-complete-ruby-all)))
    (anything)))

(provide 'anything-rcodetools)

;; How to save (DO NOT REMOVE!!)
;; (emacswiki-post "anything-rcodetools.el")
;;; install-elisp.el ends here
