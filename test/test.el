(defmacro frecency--test-with-redefined-functions (fns &rest body)
  "Run BODY with functions redefined according to FNS.
FNS should be a list of (FUNCTION-NAME FUNCTION-BODY) lists,
where FUNCTION-BODY is a lambda form.  This is helpful when, for
whatever reason, `cl-flet' and `cl-labels' don't work."
  (declare (indent defun))
  (let* ((set-forms (cl-loop for (fn def) in fns
                             for orig = (intern (concat (symbol-name fn) "-orig"))
                             collect `(setf (symbol-function ',orig) (symbol-function ',fn))
                             collect `(setf (symbol-function ',fn) ,def)))
         (unset-forms (cl-loop for (fn def) in fns
                               for orig = (intern (concat (symbol-name fn) "-orig"))
                               collect `(setf (symbol-function ',fn) (symbol-function ',orig)))))
    `(progn
       (unwind-protect
           (progn
             ,@set-forms
             ,@body)
         ,@unset-forms))))


(defmacro frecency--test (&rest body)
  `(frecency--test-with-redefined-functions
     ((current-time (lambda ()
                      '(22950 57654 8707 234000))))
     (let ((frecency-max-timestamps 10))
       ,@body)))

;; Put stored value first in each test.  Makes it easier to follow.

(ert-deftest frecency-new-alist ()
  (should
   ;; No existing timestamps or num-timestamps
   (equal '((:total-count . 1)
            (:num-timestamps . 1)
            (:timestamps 1504108854.0087073)
            (:key . val))
          (frecency--test
           (let ((item (a-list :key 'val)))
             (frecency-update item))))))

(ert-deftest frecency-update-alist ()
  (should
   (let ((input '((:num-timestamps . 10)
                  (:timestamps 1 2 3 4 5 6 7 8 9 10)
                  (:total-count . 10)
                  (:key . val)))
         (result '((:num-timestamps . 10)
                   (:timestamps 1504108854.0087073 1 2 3 4 5 6 7 8 9)
                   (:total-count . 11)
                   (:key . val))))
     ;; Existing timestamps and num-timestamps
     (equal result
            (frecency--test
             (frecency-update input))))))

(ert-deftest frecency-new-plist ()
  (should
   ;; No existing timestamps or num-timestamps
   (equal (list :one 1
                :timestamps '(1504108854.0087073)
                :num-timestamps 1
                :total-count 1)
          (frecency--test
           (let ((item (list :one 1)))
             (frecency-update item :get-fn #'plist-get
                              :set-fn #'plist-put))))))

(ert-deftest frecency-update-plist ()
  (should
   ;; No existing timestamps or num-timestamps
   (equal (list :one 1
                :timestamps '(1504108854.0087073
                              1504108854.0087073)
                :num-timestamps 2
                :total-count 2)
          (frecency--test
           (let ((item (list :one 1
                             :timestamps '(1504108854.0087073)
                             :num-timestamps 1
                             :total-count 1)))
             (frecency-update item :get-fn #'plist-get
                              :set-fn #'plist-put))))))

(ert-deftest frecency-score-plist ()
  (let ((input (list :one 1
                     :timestamps '(1504108854.0087073
                                   1504108854.0087073)
                     :num-timestamps 2
                     :total-count 2)))
    ;; Existing timestamps and num-timestamps
    (frecency--test
     (frecency-score input :get-fn #'plist-get))))
