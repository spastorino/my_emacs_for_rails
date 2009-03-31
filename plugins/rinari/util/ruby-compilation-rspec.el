;; Add rspec support to ruby-compilation

(require 'ruby-compilation)

(add-hook 'ruby-mode-hook (lambda ()
                            (when (string-match "_spec.rb$" buffer-file-name)
                              (set (make-local-variable 'ruby-compilation-executable)
                                   "spec")
                              (set (make-local-variable 'ruby-compilation-test-name-flag)
                                   "-e"))))

(fset 'ruby-compilation-this-test-name-old
  'ruby-compilation-this-test-name)

(defun ruby-compilation-this-test-name ()
  (if (equal ruby-compilation-executable "spec")
      (ruby-compilation-this-spec-name)
    (ruby-compilation-this-test-name-old)))
    
(defun ruby-compilation-this-spec-name ()
  "Which test are we currently in?"
  (save-excursion
    (search-backward-regexp "it [\"']\\(.*\\)[\"'] do")
    (match-string 1)))
  
(provide 'ruby-compilation-rspec)
