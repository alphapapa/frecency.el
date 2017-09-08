;; TODO: I think there is an Elisp mocking library, IIRC.  Maybe I could just use that...

;;; Code:

;;;; Macros

(defmacro frecency--with-mock-functions (fns &rest body)
  "Run BODY with functions redefined according to FNS.
FNS should be a list of (FUNCTION-NAME FUNCTION-BODY) lists,
where FUNCTION-BODY is a lambda form."
  (declare (indent defun))
  `(cl-letf ,(cl-loop for (fn def) in fns
                      collect `((symbol-function ',fn)
                                ,def))
     ,@body))

(defmacro frecency--test (&rest body)
  `(frecency--with-mock-functions
     ((current-time (lambda ()
                      '(22950 57654 8707 234000))))
     (let ((frecency-max-timestamps 10))
       ,@body)))

(cl-defmacro frecency--test-i/o (&key input output body)
  `(let ((input ,input)
         (output ,output))
     (equal (frecency--test
             ,body)
            output)))

;;;; Tests

;;;;; alists

(ert-deftest frecency-new-alist ()
  (should
   (frecency--test-i/o
    :input (a-list :key 'val)
    :output '((:frecency-total-count . 1)
              (:frecency-num-timestamps . 1)
              (:frecency-timestamps 1504108854.0087073)
              (:key . val))
    :body (frecency-update input))))

(ert-deftest frecency-update-alist ()
  (should
   (frecency--test-i/o
    :input '((:frecency-num-timestamps . 10)
             (:frecency-timestamps 1 2 3 4 5 6 7 8 9 10)
             (:frecency-total-count . 10)
             (:key . val))
    :output '((:frecency-num-timestamps . 10)
              (:frecency-timestamps 1504108854.0087073 1 2 3 4 5 6 7 8 9)
              (:frecency-total-count . 11)
              (:key . val))
    :body (frecency-update input))))

(ert-deftest frecency-score-alist ()
  (should
   (frecency--test-i/o
    :input '((:frecency-num-timestamps . 2)
             (:frecency-timestamps 1504108854.0087073
                                   1504108854.0087073)
             (:frecency-total-count . 2)
             (:key . val))
    :output 100
    :body (frecency-score input))))

(ert-deftest frecency-sort-alist ()
  (should
   (frecency--test-i/o
    :input '(((:frecency-num-timestamps . 1)
              (:frecency-timestamps 1504108854.0087073)
              (:frecency-total-count . 1)
              (:key . val))
             ((:frecency-num-timestamps . 10)
              (:frecency-timestamps 1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073
                                    1504108854.0087073)
              (:frecency-total-count . 11)
              (:key . val)))
    :output '(((:frecency-num-timestamps . 10)
               (:frecency-timestamps 1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073
                                     1504108854.0087073)
               (:frecency-total-count . 11)
               (:key . val))
              ((:frecency-num-timestamps . 1)
               (:frecency-timestamps 1504108854.0087073)
               (:frecency-total-count . 1)
               (:key . val)))
    :body (frecency-sort input))))

;;;;; plists

(ert-deftest frecency-new-plist ()
  (should
   (frecency--test-i/o
    :input (list :one 1)
    :output (list :one 1
                  :frecency-timestamps '(1504108854.0087073)
                  :frecency-num-timestamps 1
                  :frecency-total-count 1)
    :body (frecency-update input
            :get-fn #'plist-get
            :set-fn #'plist-put))))

(ert-deftest frecency-update-plist ()
  (should
   (frecency--test-i/o
    :input (list :one 1
                 :frecency-timestamps '(1504108854.0087073)
                 :frecency-num-timestamps 1
                 :frecency-total-count 1)
    :output (list :one 1
                  :frecency-timestamps '(1504108854.0087073
                                         1504108854.0087073)
                  :frecency-num-timestamps 2
                  :frecency-total-count 2)
    :body (frecency-update input
            :get-fn #'plist-get
            :set-fn #'plist-put))))

(ert-deftest frecency-score-plist ()
  (should
   (frecency--test-i/o
    :input (list :one 1
                 :frecency-timestamps '(1504108854.0087073
                                        1504108854.0087073)
                 :frecency-num-timestamps 2
                 :frecency-total-count 2)
    :output 100
    :body (frecency-score input :get-fn #'plist-get))))

(ert-deftest frecency-sort-plist ()
  (should
   (frecency--test-i/o
    :input (list (list :frecency-num-timestamps 1
                       :frecency-timestamps (list 1504108854.0087073)
                       :frecency-total-count 1
                       :key 'val)
                 (list :frecency-num-timestamps 10
                       :frecency-timestamps (list 1504108854.0087073 1504108854.0087073 1504108854.0087073 1504108854.0087073
                                                  1504108854.0087073 1504108854.0087073 1504108854.0087073 1504108854.0087073
                                                  1504108854.0087073 1504108854.0087073)
                       :frecency-total-count 11
                       :key 'val))
    :output (list (list :frecency-num-timestamps 10
                        :frecency-timestamps (list 1504108854.0087073 1504108854.0087073 1504108854.0087073 1504108854.0087073
                                                   1504108854.0087073 1504108854.0087073 1504108854.0087073 1504108854.0087073
                                                   1504108854.0087073 1504108854.0087073)
                        :frecency-total-count 11
                        :key 'val)
                  (list :frecency-num-timestamps 1
                        :frecency-timestamps (list 1504108854.0087073)
                        :frecency-total-count 1
                        :key 'val))
    :body (frecency-sort input :get-fn #'plist-get))))
