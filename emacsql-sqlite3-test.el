;;; emacsql-sqlite3-tests.el --- subprocess tests -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'ert)
(require 'emacsql)
(require 'emacsql-sqlite3)

(defvar emacsql-tests-timeout 4
  "Be aggressive about not waiting on subprocesses in unit tests.")

(defun emacsql-make-connection ()
  (emacsql-sqlite3 nil))

(ert-deftest emacsql-basic ()
  "A short test that fully interacts with SQLite."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (emacsql-with-connection (db (emacsql-make-connection))
      (emacsql db [:create-temporary-table foo ([x])])
      (should-error (emacsql db [:create-temporary-table foo ([x])]))
      (emacsql db [:insert :into foo :values ([1] [2] [3])])
      (should (equal (emacsql db [:select * :from foo])
                     '((1) (2) (3)))))))

(ert-deftest emacsql-nul-character ()
  "Try inserting and retrieving strings with a NUL byte."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (emacsql-with-connection (db (emacsql-make-connection))
      (emacsql db [:create-temporary-table foo ([x])])
      (emacsql db [:insert :into foo :values (["a\0bc"])])
      (should (equal (emacsql db [:select * :from foo])
                     '(("a\0bc")))))))

(ert-deftest emacsql-foreign-key ()
  "Tests that foreign keys work properly through EmacSQL."
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (emacsql-with-connection (db (emacsql-make-connection))
      (unwind-protect
           (progn
             ;; Temporarily open foreign key support.
             (emacsql db [:pragma (= foreign_keys ON)])
             (emacsql-thread db
               [:create-table person ([(id integer :primary-key) name])]
               [:create-table likes
                ([(personid integer) color]
                 (:foreign-key [personid] :references person [id]
                               :on-delete :cascade))]
               [:insert :into person :values ([0 "Chris"] [1 "Brian"])])
             (should (equal (emacsql db [:select * :from person :order-by id])
                            '((0 "Chris") (1 "Brian"))))
             (emacsql db [:insert :into likes
                          :values ([0 red] [0 yellow] [1 yellow])])
             (should (equal (emacsql db [:select * :from likes
                                         :order-by [personid color]])
                            '((0 red) (0 yellow) (1 yellow))))
             (emacsql db [:delete :from person :where (= id 0)])
             (should (equal (emacsql db [:select * :from likes])
                            '((1 yellow)))))
        (emacsql-thread db
          [:drop-table likes]
          [:drop-table person])))))

(ert-deftest emacsql-error ()
  "Check that we're getting expected conditions."
  (should-error (emacsql-compile nil [:insert :into foo :values 1])
                :type 'emacsql-syntax)
  (let ((emacsql-global-timeout emacsql-tests-timeout))
    (emacsql-with-connection (db (emacsql-make-connection))
      (emacsql db [:create-temporary-table foo ([x])])
      (should-error (emacsql db [:create-temporary-table foo ([x])])
                    :type 'emacsql-error))))

(ert-deftest emacsql-special-chars ()
  "A short test that interacts with SQLite with special characters."
  (let ((emacsql-global-timeout 4))
    (emacsql-with-connection (db (emacsql-make-connection))
      (emacsql db [:create-temporary-table test-table ([x])])
      (emacsql db [:insert-into test-table :values ([""] [\] ["m\nn"])])
      (should (process-live-p (emacsql-process db)))
      (should (equal (emacsql db [:select * :from test-table])
                     '(("") (\) ("m\nn")))))))

(defun emacsql-sqlite3-run-dot-command-sync (conn cmd &rest args)
  "Like `emacsql-sqlite3-run-dot-command', but synchronously."
  (cl-letf* ((proc (emacsql-process conn))
             (retsym (make-symbol "retsym"))
             ((process-filter proc)
               (lambda (_proc string)
                 (throw retsym string))))
    (catch retsym
      (apply #'emacsql-sqlite3-run-dot-command conn cmd args)
      (accept-process-output proc emacsql-tests-timeout))))

(ert-deftest emacsql-sqlite3-dot-command ()
  "Test `emacsql-sqlite3-run-dot-command'."
  (emacsql-with-connection (conn (emacsql-make-connection))
    (should (equal
             (emacsql-sqlite3-run-dot-command-sync conn :print "OK")
             "OK\n"))
    (should (equal
             (emacsql-sqlite3-run-dot-command-sync conn 'print "OK")
             "OK\n"))
    (should (equal
             (emacsql-sqlite3-run-dot-command-sync conn "print" "OK")
             "OK\n"))
    (should (equal
             (emacsql-sqlite3-run-dot-command-sync conn "print" "O K")
             "O K\n"))))

;;; emacsql-sqlite3-tests.el ends here
