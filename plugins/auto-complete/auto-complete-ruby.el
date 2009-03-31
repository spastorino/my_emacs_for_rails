(require 'auto-complete)
(require 'rcodetools)

;(defvar ac-source-rcodetools
;  `(
;(candidates
;     . (lambda ()
;                    (all-completions
;                     ac-prefix
;                     (mapcar
;                      (lambda (completion)
;                        (replace-regexp-in-string "\t.*$" "" (car completion)))
;(           rct-get-all-methods)))
;                      ))))

(defvar ac-source-rcodetools
  `((init . (lambda ()
              (condition-case x
                  (rct-exec-and-eval rct-complete-command-name "--completion-emacs-icicles")
                (error) (setq rct-method-completion-table nil))))
    (candidates . (lambda ()
                    (all-completions
                     ac-prefix
                     (mapcar
                      (lambda (completion)
                        (replace-regexp-in-string "\t.*$" "" (car completion)))
                      rct-method-completion-table))))))

(provide 'auto-complete-ruby)
