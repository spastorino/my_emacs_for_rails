;;; ecb-compatibility.el --- ECB-compatibility for other packages

;; Copyright (C) 2000 - 2005 Jesper Nordenberg,
;;                           Klaus Berndl,
;;                           Kevin A. Burton,
;;                           Free Software Foundation, Inc.

;; Author: Klaus Berndl <klaus.berndl@sdm.de>
;; Maintainer: Klaus Berndl <klaus.berndl@sdm.de>
;;             Kevin A. Burton <burton@openprivacy.org>
;; Keywords: browser, code, programming, tools
;; Created: 2004

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

;; $Id: ecb-compatibility.el,v 1.8 2007/07/08 16:42:06 berndl Exp $

;;; Commentary:
;;
;; Contains compatibility-code for other-packages.
;;
;; Whenever commands of other packages are not fully compatible with ECB then
;; this library should contain the necessary code to make it fully compatible
;; - or at least working acceptable.
;;
;; This file is part of the ECB package which can be found at:
;; http://ecb.sourceforge.net

(eval-when-compile
  (require 'silentcomp))


(require 'ecb-util)
(require 'ecb-layout)

;; To add compatibilty code for packages just do:
;;
;; 1. Add the needed advice(s) to `ecb-compatibility-advices'
;; 2. Add the advice-code below.
;;
;; All advices of `ecb-compatibility-advices' will be autom. enabled when ECB
;; starts and autom. disabled when ECB shuts down. No advice is enabled just
;; by loading the ECB-library!

(defecb-advice-set ecb-compatibility-advices
  "Contains all advices needed for package-compatibility.")

;; package bs.el

(defecb-advice bs-show before ecb-compatibility-advices
  "Ensures `bs-show' works well when called from another window as an
edit-window. Does nothing if called in another frame as the `ecb-frame'."
  (when (equal (selected-frame) ecb-frame)
    (unless (ecb-point-in-edit-window-number)
      (ecb-select-edit-window))
    ;; now we handle if bs-show should always display in the compile-window
    (let ((my-bs-buffer (get-buffer-create "*buffer-selection*")))
      ;; ecb-compilation-buffer-p needs a living buffer!
      (when (and (ecb-compilation-buffer-p my-bs-buffer)
                 ecb-compile-window-height)
        (display-buffer (buffer-name my-bs-buffer))))))

;; package electric.el


(defecb-advice one-window-p around ecb-always-disabled-advices
  "If called for the `ecb-frame' is only returns not nil if there is exactly
one edit-window. Neither the ecb-windows nor the compile-window nor the
minibuffer-window are considered. This adviced version of `one-window-p' is
not for direct usage therefore it's added to `ecb-always-disabled-advices' and
therefore it's always disabled\; use the macro `ecb-with-ecb-advice' instead
if you need this adviced version of `one-window-p'!"
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (setq ad-return-value
            (= (length (ecb-canonical-edit-windows-list)) 1))
    ad-do-it))

(defecb-advice Electric-pop-up-window around ecb-compatibility-advices
  "Ensures that the electric-* commands \(e.g. `electric-buffer-list') work
well with ECB. If BUFFER is a \"compilation-buffer\" in the sense of
`ecb-compilation-buffer-p' then BUFFER will be displayed in the compile-window
of ECB - if there is any. If the compile-window is temporally hidden then the
BUFFER is displayed in an edit-window!"
  (if (and ecb-minor-mode
           (equal (selected-frame) ecb-frame))
      (if (and (ecb-compilation-buffer-p (ad-get-arg 0))
               (equal (ecb-compile-window-state) 'visible))
          (pop-to-buffer (ad-get-arg 0))
        (let ((ecb-compilation-buffer-names nil)
              (ecb-compilation-major-modes nil)
              (ecb-compilation-predicates nil))
          (ecb-with-ecb-advice 'one-window-p 'around
            ad-do-it)))
    ad-do-it))

(defecb-advice electric-command-history before ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defecb-advice electric-buffer-list before ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame)
             (ecb-point-in-dedicated-special-buffer))
    (ecb-select-edit-window)))

(defecb-advice electric-buffer-list after ecb-compatibility-advices
  "Ensures that the electric-* commands work well with ECB."
  (when (and ecb-minor-mode
             (equal (selected-frame) ecb-frame))
    (if (get-buffer "*Buffer List*")
        (bury-buffer (get-buffer "*Buffer List*")))))

;; package master.el (only Emacs >= 22.X)

;; The adviced version of switch-to-buffer-other-window can redraw the layout
;; (e.g. if the buffer in the compile-window is the slave and the
;; compile-window has been made visible), so <window> in the code below can be
;; a destroyed window-object! we have to prevent from this (e.g. by selecting
;; the window before by number).
(when-ecb-running-emacs
 (defecb-advice master-says around ecb-compatibility-advices
   "Makes the function compatible with ECB."
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame)))
       (ecb-with-original-basic-functions ad-do-it)
     (if (null (buffer-live-p (get-buffer master-of)))
         (error "Slave buffer has disappeared")
       (let ((window  (selected-window))
             (point-loc (ecb-where-is-point))
             (p (point)))
         (if (not (eq (window-buffer window) (get-buffer master-of)))
         (switch-to-buffer-other-window master-of))
         (if (ad-get-arg 0)
             (condition-case nil
                 (apply (ad-get-arg 0) (ad-get-arg 1))
               (error nil)))
         (select-window (case (car point-loc)
                          (ecb
                           (ecb-get-ecb-window-by-number (cdr point-loc)))
                          (edit
                           (ecb-get-edit-window-by-number (cdr point-loc)))
                          (compile
                           ecb-compile-window)
                          (minibuf
                           (minibuffer-window ecb-frame))))
         (goto-char (point))))))
   )

;; package scroll-all.el


(defecb-advice count-windows around ecb-always-disabled-advices
  "If the selected frame is the ecb-frame and `scroll-all-mode' is not nil
then return the current number of edit-windows if point is in an edit-window
and always return 1 if point is not in an edit-window. In any other frame or
if `scroll-all-mode' is nil return the number of visible windows."
  (if (and (equal (selected-frame) ecb-frame)
           ecb-minor-mode
           (boundp 'scroll-all-mode)
           scroll-all-mode)
      (setq ad-return-value (if (ecb-point-in-edit-window-number)
                                (length (ecb-canonical-edit-windows-list))
                              1))
    (ecb-with-original-basic-functions
     ad-do-it)))

(defecb-advice scroll-all-function-all around ecb-compatibility-advices
  "Make it compatible with ECB."
  (if (or (not ecb-minor-mode)
          (not (equal (selected-frame) ecb-frame)))
      (ecb-with-original-basic-functions ad-do-it)
    (let (;; This runs the `other-window'-calls in the body in the right mode
          (ecb-other-window-behavior 'only-edit))
      (ecb-with-ecb-advice 'count-windows 'around
        ad-do-it))))


;; package tmm.el

;; Klaus Berndl <klaus.berndl@sdm.de>: We can not use our
;; Electric-pop-up-window advice instaed of this advice because otherwise
;; some commands of the popup-menus of the ecb-buffers would not work - this
;; comes from the save-window-excursion in the the tmm.
(when-ecb-running-emacs
 (defecb-advice tmm-prompt around ecb-compatibility-advices
   "Make it compatible with ECB."
   (if (or (not ecb-minor-mode)
           (not (equal (selected-frame) ecb-frame)))
       (ecb-with-original-basic-functions ad-do-it)
     ;; we set temporally `ecb-other-window-behavior' to a function which
     ;; always selects the "next" window after the
     ;; `ecb-last-edit-window-with-point'
     (let ((ecb-other-window-behavior
            (lambda (win-list edit-win-list ecb-win-list comp-win
                              mini-win point-loc nth-win)
              (ecb-next-listelem edit-win-list
                                 ecb-last-edit-window-with-point)))
           ;; we must not handle the tmm-stuff as compilation-buffer
           (ecb-compilation-buffer-names nil)
           (ecb-compilation-major-modes nil)
           (ecb-compilation-predicates nil))
       ad-do-it)))
 )

  


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: we must support
;; `grep-window-height' in Emacs 22 (XEmacs?)

;; TODO: Klaus Berndl <klaus.berndl@sdm.de>: (while-no-input should be used in
;; the stealthy stuff!! But first test, if it works also on windows - see
;; mailings in the emacs-devel-list... hmm, we have already ecb-throw-on-input
;; etc... 


;; TODO: Klaus Berndl <klaus.berndl@sdm.de>:
;; *** The new package gdb-ui.el provides an enhanced graphical interface to
;; GDB. You can interact with GDB through the GUD buffer in the usual way, but
;; there are also further buffers which control the execution and describe the
;; state of your program. It separates the input/output of your program from
;; that of GDB and watches expressions in the speedbar. It also uses features
;; of Emacs 21 such as the display margin for breakpoints, and the toolbar.
;; This is new in Emacs 21.4 so maybe we have to make it compatible with ECB!
;; But maybe this could be hard because AFAIK gdb-ui.el uses dedicated
;; windows!




;; we disable the advices at load-time
(ecb-disable-advices 'ecb-compatibility-advices)

(silentcomp-provide 'ecb-compatibility)

;;; ecb-compatibility.el ends here
