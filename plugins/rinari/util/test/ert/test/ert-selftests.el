;;; ert-selftests.el --- Self-Tests for ERT

;; Copyright (C) 2007, 2008 Christian M. Ohler
;; Licensed under the same terms as ERT.

(defvar ert-test-root
  (file-name-directory (or (buffer-file-name) load-file-name))
  "Location of root used for ert tests.")

(add-to-list 'load-path ert-test-root)

(load "../ert")
(load "../ert-functional")

;;; Self-tests.

;; Test that test bodies are actually run.
(defvar ert-test-body-was-run nil)
(ert-deftest ert-test-body-runs ()
  (setq ert-test-body-was-run t))

;; Test that nested test bodies run.
(ert-deftest ert-nested-test-body-runs ()
  (lexical-let ((was-run nil))
    (let ((test (make-ert-test :body (lambda ()
                                       (setq was-run t)))))
      (assert (not was-run))
      (ert-run-test test)
      (assert was-run))))

;; Test that pass/fail works.
(ert-deftest ert-test-pass ()
  (let ((test (make-ert-test :body (lambda ()))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))

(ert-deftest ert-test-fail ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed "failure message"))
              t))))

(ert-deftest ert-test-fail-debug-with-condition-case ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(ert-test-failed "failure message")) t)))))

(ert-deftest ert-test-fail-debug-with-debugger-1 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test)))))

(ert-deftest ert-test-fail-debug-with-debugger-2 ()
  (let ((test (make-ert-test :body (lambda () (ert-fail "failure message")))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil)))))

(ert-deftest ert-test-fail-debug-nested-with-debugger ()
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error t))
                                       (ert-fail "failure message"))))))
    (let ((debugger (lambda (&rest debugger-args)
                      (assert nil nil "Assertion a"))))
      (let ((ert-debug-on-error nil))
        (ert-run-test test))))
  (let ((test (make-ert-test :body (lambda ()
                                     (let ((ert-debug-on-error nil))
                                       (ert-fail "failure message"))))))
    (block nil
      (let ((debugger (lambda (&rest debugger-args)
                        (return-from nil nil))))
        (let ((ert-debug-on-error t))
          (ert-run-test test))
        (assert nil nil "Assertion b")))))

(ert-deftest ert-test-error ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-error) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(error "error message"))
              t))))

(ert-deftest ert-test-error-debug ()
  (let ((test (make-ert-test :body (lambda () (error "error message")))))
    (condition-case condition
        (progn
          (let ((ert-debug-on-error t))
            (ert-run-test test))
          (assert nil))
      ((error)
       (assert (equal condition '(error "error message")) t)))))


;; Test that `should' works.
(ert-deftest ert-test-should ()
  (let ((test (make-ert-test :body (lambda () (should nil)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should nil) :form nil :value nil)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should t)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed) t))))

(ert-deftest ert-test-should-value ()
  (should (eql (should 'foo) 'foo))
  (should (eql (should 'bar) 'bar)))

(ert-deftest ert-test-should-not ()
  (let ((test (make-ert-test :body (lambda () (should-not t)))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (assert (typep result 'ert-test-failed) t)
      (assert (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed ((should-not t) :form t :value t)))
              t)))
  (let ((test (make-ert-test :body (lambda () (should-not nil)))))
    (let ((result (ert-run-test test)))
      (assert (typep result 'ert-test-passed)))))


(ert-deftest ert-test-should-error ()
  ;; No error.
  (let ((test (make-ert-test :body (lambda () (should-error (progn))))))
    (let ((result (let ((ert-debug-on-error nil))
                    (ert-run-test test))))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (progn))
                        :form (progn)
                        :value nil
                        :fail-reason "did not signal an error"))))))
  ;; A simple error.
  (let ((test (make-ert-test :body (lambda () (should-error (error "foo"))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error of unexpected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (error "foo")
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (error "foo") :type 'singularity-error)
                  :form (error "foo")
                  :condition (error "foo")
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  ;; Error of the expected type, no test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (signal 'singularity-error
                                                           nil)
                                                   :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error that fails the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (error "foo")
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (error "foo") :test (lambda (error) nil))
                        :form (error "foo")
                        :condition (error "foo")
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that passes the test, no type.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error (error "foo")
                                                   :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  ;; Error that has the expected type but fails the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal (ert-test-result-with-condition-condition result)
                     '(ert-test-failed
                       ((should-error (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) nil))
                        :form (signal singularity-error nil)
                        :condition (singularity-error)
                        :fail-reason
                        "the error signalled did not pass the test"))))))
  ;; Error that has the expected type and passes the test.
  (let ((test (make-ert-test :body (lambda ()
                                     (should-error
                                      (signal 'singularity-error nil)
                                      :type 'singularity-error
                                      :test (lambda (error) t))))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed)))))

(ert-deftest ert-test-should-error-subtypes ()
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'singularity-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-passed))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'arith-error nil)
                                     :type 'singularity-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'arith-error nil)
                                :type 'singularity-error
                                :exclude-subtypes t)
                  :form (signal arith-error nil)
                  :condition (arith-error)
                  :fail-reason
                  "the error signalled did not have the expected type"))))))
  (let ((test (make-ert-test
               :body (lambda ()
                       (should-error (signal 'singularity-error nil)
                                     :type 'arith-error
                                     :exclude-subtypes t)))))
    (let ((result (ert-run-test test)))
      (should (typep result 'ert-test-failed))
      (should (equal
               (ert-test-result-with-condition-condition result)
               '(ert-test-failed
                 ((should-error (signal 'singularity-error nil)
                                :type 'arith-error
                                :exclude-subtypes t)
                  :form (signal singularity-error nil)
                  :condition (singularity-error)
                  :fail-reason
                  "the error signalled was a subtype of the expected type")))))))

;; Test that `should' errors contain the information we expect them to.
(defmacro ert-test-my-list (&rest args)
  `(list ,@args))

(ert-deftest ert-test-should-failure-debugging ()
  (loop for (body expected-condition) in
        `((,(lambda () (let ((x nil)) (should x)))
           (ert-test-failed ((should x) :form x :value nil)))
          (,(lambda () (let ((x t)) (should-not x)))
           (ert-test-failed ((should-not x) :form x :value t)))
          (,(lambda () (let ((x t)) (should (not x))))
           (ert-test-failed ((should (not x)) :form (not t) :value nil)))
          (,(lambda () (let ((x nil)) (should-not (not x))))
           (ert-test-failed ((should-not (not x)) :form (not nil) :value t)))
          (,(lambda () (let ((x t) (y nil)) (should-not (ert-test-my-list x y))))
           (ert-test-failed
            ((should-not (ert-test-my-list x y))
             :form (list t nil)
             :value (t nil))))
          (,(lambda () (let ((x t)) (should (error "foo"))))
           (error "foo")))
        do
        (let ((test (make-ert-test :body body)))
          (condition-case actual-condition
              (progn
                (let ((ert-debug-on-error t))
                  (ert-run-test test))
                (assert nil))
            ((error)
             (should (equal actual-condition expected-condition)))))))

(ert-deftest ert-test-messages ()
  (let* ((message-string "Test message")
         (messages-buffer (get-buffer-create "*Messages*"))
         (test (make-ert-test :body (lambda () (message "%s" message-string)))))
    (with-current-buffer messages-buffer
      (let ((result (ert-run-test test)))
        (should (equal (concat message-string "\n")
                       (ert-test-result-messages result)))))))

(defun ert-call-with-temporary-messages-buffer (thunk)
  (lexical-let ((new-buffer-name (generate-new-buffer-name
                                  "*Messages* orig buffer")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*Messages*")
            (rename-buffer new-buffer-name))
          (get-buffer-create "*Messages*")
          (funcall thunk))
      (kill-buffer "*Messages*")
      (with-current-buffer new-buffer-name
        (rename-buffer "*Messages*")))))

(ert-deftest ert-test-messages-on-log-truncation ()
  (let ((test (make-ert-test
               :body (lambda ()
                       ;; Emacs would combine messages if we
                       ;; generate the same message multiple
                       ;; times.
                       (message "a")
                       (message "b")
                       (message "c")
                       (message "d")))))
    (let (result)
      (ert-call-with-temporary-messages-buffer
       (lambda ()
         (let ((message-log-max 2))
           (setq result (ert-run-test test)))
         (should (equal (with-current-buffer "*Messages*"
                          (buffer-string))
                        "c\nd\n"))))
      (should (equal (ert-test-result-messages result) "a\nb\nc\nd\n")))))

;; Test `ert-select-tests'.
(ert-deftest ert-test-select-regexp ()
  (should (equal (ert-select-tests "^ert-test-select-regexp$" t)
                 (list (ert-get-test 'ert-test-select-regexp)))))

(ert-deftest ert-test-test-boundp ()
  (should (ert-test-boundp 'ert-test-test-boundp))
  (should-not (ert-test-boundp (make-symbol "ert-not-a-test"))))

(ert-deftest ert-test-select-member ()
  (should (equal (ert-select-tests '(member ert-test-select-member) t)
                 (list (ert-get-test 'ert-test-select-member)))))

(ert-deftest ert-test-select-test ()
  (should (equal (ert-select-tests (ert-get-test 'ert-test-select-test) t)
                 (list (ert-get-test 'ert-test-select-test)))))

(ert-deftest ert-test-select-symbol ()
  (should (equal (ert-select-tests 'ert-test-select-symbol t)
                 (list (ert-get-test 'ert-test-select-symbol)))))

(ert-deftest ert-test-select-and ()
  (let ((test (make-ert-test
               :name nil
               :body nil
               :most-recent-result (make-ert-test-failed
                                    :condition nil
                                    :backtrace nil))))
    (should (equal (ert-select-tests `(and (member ,test) :failed) t)
                   (list test)))))


;; Test utility functions.
(ert-deftest ert-proper-list-p ()
  (should (ert-proper-list-p '()))
  (should (ert-proper-list-p '(1)))
  (should (ert-proper-list-p '(1 2)))
  (should (ert-proper-list-p '(1 2 3)))
  (should (ert-proper-list-p '(1 2 3 4)))
  (should (not (ert-proper-list-p 'a)))
  (should (not (ert-proper-list-p '(1 . a))))
  (should (not (ert-proper-list-p '(1 2 . a))))
  (should (not (ert-proper-list-p '(1 2 3 . a))))
  (should (not (ert-proper-list-p '(1 2 3 4 . a))))
  (let ((a (list 1)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) a)
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cddr a))
    (should (not (ert-proper-list-p a))))
  (let ((a (list 1 2 3 4)))
    (setf (cdr (last a)) (cdddr a))
    (should (not (ert-proper-list-p a)))))

(ert-deftest ert-parse-keys-and-body ()
  (should (equal (ert-parse-keys-and-body '(foo)) '(nil (foo))))
  (should (equal (ert-parse-keys-and-body '(:bar foo)) '((:bar foo) nil)))
  (should (equal (ert-parse-keys-and-body '(:bar foo a (b))) '((:bar foo) (a (b)))))
  (should (equal (ert-parse-keys-and-body '(:bar foo :a (b))) '((:bar foo :a (b)) nil)))
  (should (equal (ert-parse-keys-and-body '(bar foo :a (b))) '(nil (bar foo :a (b)))))
  (should-error (ert-parse-keys-and-body '(:bar foo :a))))



;; Test `ert-run-tests'.
(ert-deftest ert-test-run-tests ()
  (let ((passing-test (make-ert-test :name 'passing-test
                                     :body (lambda () (ert-pass))))
        (failing-test (make-ert-test :name 'failing-test
                                     :body (lambda () (ert-fail
                                                       "failure message"))))
        )
    (let ((ert-debug-on-error nil))
      (let* ((buffer-name (generate-new-buffer-name " *ert-test-run-tests*"))
             (messages nil)
             (mock-message-fn
              (lambda (format-string &rest args)
                (push (apply #'format format-string args) messages))))
        (save-window-excursion
          (unwind-protect
              (let ((case-fold-search nil))
                (ert-run-tests-interactively
                 `(member ,passing-test ,failing-test) buffer-name
                 mock-message-fn)
                (should (equal messages `(,(concat
                                            "Ran 2 tests, 1 results were "
                                            "as expected, 1 unexpected"))))
                (with-current-buffer buffer-name
                  (goto-char (point-min))
                  (should (equal
                           (buffer-substring (point-min)
                                             (save-excursion
                                               (forward-line 5)
                                               (point)))
                           (concat
                            "Selector: (member <passing-test> <failing-test>)\n"
                            "Passed: 1 (0 unexpected)\n"
                            "Failed: 1 (1 unexpected)\n"
                            "Error:  0 (0 unexpected)\n"
                            "Total:  2/2\n")))))
            (when (get-buffer buffer-name)
              (kill-buffer buffer-name))))))))

(ert-deftest ert-test-special-operator-p ()
  (should (ert-special-operator-p 'if))
  (should-not (ert-special-operator-p 'car))
  (should-not (ert-special-operator-p 'ert-special-operator-p))
  (let ((b (gensym)))
    (should-not (ert-special-operator-p b))
    (fset b 'if)
    (should (ert-special-operator-p b))))

;; This test attempts to demonstrate that there is no way to force
;; immediate truncation of the *Messages* buffer from Lisp (and hence
;; justifies the existence of
;; `ert-force-message-log-buffer-truncation'): The only way that came
;; to my mind was (message ""), which doesn't have the desired effect.
(ert-deftest ert-test-builtin-message-log-flushing ()
  (ert-call-with-temporary-messages-buffer
   (lambda ()
     (with-current-buffer "*Messages*"
       (let ((message-log-max 2))
         (let ((message-log-max t))
           (loop for i below 4 do
                 (message "%s" i))
           (should (eql (count-lines (point-min) (point-max)) 4)))
         (should (eql (count-lines (point-min) (point-max)) 4))
         (message "")
         (should (eql (count-lines (point-min) (point-max)) 4))
         (message "Test message")
         (should (eql (count-lines (point-min) (point-max)) 2)))))))

(ert-deftest ert-test-force-message-log-buffer-truncation ()
  (labels ((body ()
             (loop for i below 5 do
                   (message "%s" i)))
           (c (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max x))
                  (body))
                (with-current-buffer "*Messages*"
                  (buffer-string)))))
           (lisp (x)
             (ert-call-with-temporary-messages-buffer
              (lambda ()
                (let ((message-log-max t))
                  (body))
                (let ((message-log-max x))
                  (ert-force-message-log-buffer-truncation))
                (with-current-buffer "*Messages*"
                  (buffer-string))))))
    (loop for x in '(0 1 2 3 4 5 6 t) do
          (should (equal (c x) (lisp x))))))

;;; Predicates

(ert-deftest ert-buffer-changes-p ()
  (with-temp-buffer
    (should (buffer-changes-p
             (insert "hello")))
    (should-not (buffer-changes-p
                 (message "hello")))))

(ert-deftest ert-buffer-contains-p ()
  (with-temp-buffer
    (insert "hello world")
    (should (buffer-contains-p "hello")))
  (should-not (buffer-contains-p "goodbye")))

(ert-deftest ert-correctly-indented-p ()
  (should (correctly-indented-p (concat ert-test-root "well-indented.el")))
  (should-not (correctly-indented-p (concat ert-test-root "badly-indented.el"))))

;;; Utilities

(ert-deftest ert-with-test-buffer ()
  (let ((contents "Foo bar\n  baz\n\tbip"))
    (with-test-buffer contents
      (should (string-equal (buffer-string) contents)))))

(ert-deftest ert-with-test-buffer-inserting ()
  (let ((contents "Foo bar\n  baz\n\tbip"))
    (with-test-buffer contents
      (insert "Hello\n")
      (should (string-equal (buffer-string) (concat "Hello\n" contents))))))

(ert-deftest ert-with-test-buffer-mark ()
  (with-test-buffer "Foo<mark> bar baz"
    (should (string-equal (buffer-substring (point) (mark)) "Foo"))
    (should (string-equal (buffer-string) "Foo bar baz"))))

(ert-deftest ert-with-test-buffer-fake-mark ()
  (with-test-buffer "Foo\\<mark> bar baz"
    (should (string-equal (buffer-string) "Foo<mark> bar baz"))))

(ert-deftest ert-with-test-buffer-point ()
  (with-test-buffer "Foo bar<point> baz"
    (insert "bell")
    (should (string-equal (buffer-string) "Foo barbell baz"))))

(ert-deftest ert-with-test-buffer-mark-and-point ()
  (with-test-buffer "Foo <mark>bar<point> baz"
    (upcase-region (mark) (point))
    (should (string-equal (buffer-string) "Foo BAR baz"))))

;; Run tests and make sure they actually ran.
(let ((window-configuration (current-window-configuration))
      (ert-test-body-was-run nil)
      ;; The buffer name chosen here should not compete with the default
      ;; results buffer name for completion in `switch-to-buffer'.
      (stats (ert-run-tests-interactively "^ert-" " *ert self-tests*")))
  (assert ert-test-body-was-run)
  (when (zerop (+ (ert-stats-passed-unexpected stats)
                  (ert-stats-failed-unexpected stats)
                  (ert-stats-error-unexpected stats)))
    ;; Hide results window only when everything went well.
    (set-window-configuration window-configuration)))

(provide 'ert-selftests)