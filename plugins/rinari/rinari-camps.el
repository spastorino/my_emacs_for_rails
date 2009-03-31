;;; rinari-camps.el --- rinari minor mode for camping

;; Copyright (C) 2008 Eric Schulte

;; Authors: Eric Schulte
;; Created: 2008-10-06
;; Keywords: ruby, rails, camping, project, convenience, web

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file ports some basic Rinari (Ruby on Rails minor mode)
;; commands for use inside of a Camping (Simple Ruby MVC Framework)
;; project.

(require 'ruby-mode)
(require 'inf-ruby)
(require 'ruby-compilation)

(defun rinari-camps-root () default-directory)

(defun rinari-camps-name ()
  (let ((root (rinari-camps-root)))
    (directory-file-name (file-relative-name root (expand-file-name ".." root)))))

(defun rinari-camps-web-server (&optional edit-cmd-args)
  "Run script/server.  Dump output to a compilation buffer
allowing jumping between errors and source code.  With optional
prefix argument allows editing of the server command arguments."
  (interactive "P")
  (let* ((default-directory (rinari-camps-root))
	 (command (format "/usr/bin/camping %s.rb" (rinari-camps-name)))
	 (command (if edit-cmd-args
		      (read-string "" (concat command " "))
		    command)))
    (ruby-compilation-run command)))

(defun rinari-camps-console (&optional edit-cmd-args)
  "Run script/console in a compilation buffer, with command
history and links between errors and source code.  With optional
prefix argument allows editing of the console command arguments."
  (interactive "P")
  (let* ((req (expand-file-name (concat (rinari-camps-name) ".rb") (rinari-camps-root))))
    (run-ruby (format "irb -r %s --simple-prompt" req))
    (save-excursion
      (pop-to-buffer "*ruby*")
      (set (make-local-variable 'inferior-ruby-first-prompt-pattern) "^>> ")
      (set (make-local-variable 'inferior-ruby-prompt-pattern) "^>> ")
      (rinari-camps-minor-mode))))

;; minor mode definition
(defvar rinari-camps-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Key map for Rinari Camps minor mode.")

(defun rinari-camps-bind-key-to-func (key func)
  (eval `(define-key rinari-camps-minor-mode-map 
	   ,(format "\C-c;%s" key) ,func))
  (eval `(define-key rinari-camps-minor-mode-map 
	   ,(format "\C-c'%s" key) ,func)))

(defvar rinari-camps-minor-mode-keybindings
  '(("c" . 'rinari-camps-console)
    ("w" . 'rinari-camps-web-server))
  "alist mapping of keys to functions in `rinari-camps-minor-mode'")

(mapcar (lambda (el) (rinari-camps-bind-key-to-func (car el) (cdr el)))
	rinari-camps-minor-mode-keybindings)

;;;###autoload
(define-minor-mode rinari-camps-minor-mode
  "Enable Rinari camps minor mode providing Emacs support for
working with the Camping framework."
  nil
  " Rinari-Camps"
  rinari-camps-minor-mode-map)

(defun rinari-camps ()
  "Launch a minimal rinari-minor-mode for Camping."
  (interactive)
  (rinari-camps-minor-mode)
  (message "you're camping with rinari"))

(provide 'rinari-camps)
;;; rinari-camps.el ends here