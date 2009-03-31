(require 'auto-complete)
(require 'yasnippet)

(defun ac-yasnippet-candidate-1 (table)
  (let ((hashtab (yas/snippet-table-hash table))
        (parent (yas/snippet-table-parent table))
        candidates)
    (maphash (lambda (key value)
               (push key candidates))
             hashtab)
    (setq candidates (all-completions ac-prefix (nreverse candidates)))
    (if parent
        (setq candidates
              (append candidates (ac-yasnippet-candidate-1 parent))))
    candidates))

(defun ac-yasnippet-candidate ()
  (let ((table (yas/snippet-table major-mode)))
    (if table
        (ac-yasnippet-candidate-1 table))))

(defface ac-yasnippet-menu-face
  '((t (:background "sandybrown" :foreground "black")))
  "Face for yasnippet candidate menu."
  :group 'auto-complete)

(defface ac-yasnippet-selection-face
  '((t (:background "coral3" :foreground "white")))
  "Face for the yasnippet selected candidate."
  :group 'auto-complete)

(defvar ac-source-yasnippet
  '((candidates . ac-yasnippet-candidate)
    (action . yas/expand)
    (limit . 3)
    (menu-face . ac-yasnippet-menu-face)
    (selection-face . ac-yasnippet-selection-face))
  "Source for Yasnippet.")

(provide 'auto-complete-yasnippet)
