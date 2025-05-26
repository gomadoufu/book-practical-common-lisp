(defun test-+ ()
  (and
   (= (+ 1 2) 3)
   (= (+ 1 2 3) 6)
   (= (+ -1 -3) -4)))

(defun test-+2 ()
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (format t "~:[FAIL~;pass~] ... ~a~%" (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))


(defun report-result (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form))

(defun test-+3 ()
  (report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))
  (report-result (= (+ 1 2 3) 6) '(= (+ 1 2 3) 6))
  (report-result (= (+ -1 -3) -4) '(= (+ -1 -3) -4)))

(defmacro check (form)
  `(report-result ,form ',form))


(check (= (+ 1 2) 3))
(report-result (= (+ 1 2) 3) '(= (+ 1 2) 3))

(defun test-+4 ()
  (check (= (+ 1 2) 3))
  (check (= (+ 1 2 3) 6))
  (check (= (+ -1 -3) -4)))

(defmacro check-2 (&body forms)
  `(progn
     ,@(mapcar (lambda (form)
                 `(report-result ,form ',form))
               forms)))

(defun test-+5 ()
  (check-2
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(defun report-result-2 (result form)
  (format t "~:[FAIL~;pass~] ... ~a~%" result form)
  result)

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for n in names collect `(,n (gensym)))
     ,@body))

(defmacro combine-results (&body forms)
  (with-gensyms (result)
    `(let ((,result t))
       ,@(loop for f in forms
               collect `(unless ,f (setf ,result nil)))
       ,result)))

(defmacro check-3 (&body forms)
  `(combine-results
     ,@(loop for f in forms collect `(report-result-2 ,f ',f))))

(defun test-+6 ()
  (check-3
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -5)))


;;; より良い結果レポートのために
(defun test-* ()
  (check-3
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(defun test-arithmetic ()
  (combine-results
    (test-+6)
    (test-*)))

(defvar *test-name* nil)

(defun report-result-3 (result form)
  (format t "~:[FAIL~;pass~] ... ~a: ~a~%" result *test-name* form)
  result)

(defmacro check-4(&body forms)
  `(combine-results
     ,@(loop for f in forms
             collect `(report-result-3 ,f ',f))))

(defun test-+7 ()
  (let ((*test-name* 'test-+7))
    (check-4
      (= (+ 1 2) 3)
      (= (+ 1 2 3) 6)
      (= (+ -1 -3) -4))))

(defun test-*2 ()
  (let ((*test-name* 'test-*2))
    (check-4
      (= (* 2 2) 4)
      (= (* 3 5) 15))))

(defun test-arithmetic-2 ()
  (combine-results
    (test-+7)
    (test-*2)))

;;; 抽象化の余地
(defmacro deftest (name parameters &body body)
  `(defun ,name ,parameters
     (let ((*test-name* (append *test-name* (list ',name))))
       ,@body)))

(deftest test-+8 ()
  (check-4
    (= (+ 1 2) 3)
    (= (+ 1 2 3) 6)
    (= (+ -1 -3) -4)))

(deftest test-*2 ()
  (check-4
    (= (* 2 2) 4)
    (= (* 3 5) 15)))

(deftest test-arithmetic-3 ()
  (combine-results
    (test-+8)
    (test-*2)))
