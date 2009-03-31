(add-to-list 'load-path (file-name-directory (or (buffer-file-name)
                                                 load-file-name)))

(load "ert/ert")
(load "ert/ert-functional")
(load "../ruby-mode")

(defmacro with-ruby-buffer (contents &rest body)
  "Like `with-test-buffer', but sets the buffer to `ruby-mode'."
  `(with-test-buffer ,contents
     (ruby-mode)
     ,@body))
(put 'with-ruby-buffer 'lisp-indent-function 1)

(defmacro test-here-doc-p (contents)
  `(with-ruby-buffer ,contents
     (ert-test-buffer-substitute "<here-doc>" (lambda () (should (ruby-in-here-doc-p))))
     (ert-test-buffer-substitute "<no-here-doc>" (lambda () (should-not (ruby-in-here-doc-p))))))

(ert-deftest ruby-here-doc-p ()
  (test-here-doc-p "
<<FOO
<here-doc>
FOO
<no-here-doc>
<<BAR
<here-doc>
BAR"))

(ert-deftest ruby-here-doc-p-multiple ()
  (test-here-doc-p "
p <no-here-doc>[<<FOO,<no-here-doc> <<BAR,<no-here-doc> <<BAZ]
<here-doc>
FOO
<here-doc>
BAR
<here-doc>
BAZ
<no-here-doc>"))
