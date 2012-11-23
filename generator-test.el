(require 'generator)

(defun list-subrs ()
  (loop for x being the symbols
        when (and (fboundp x)
                  (cps--special-form-p (symbol-function x)))
        collect x))

(defun pp-eval-from-buffer ()
  (interactive)
  (let ((expr (read (current-buffer))))
    (newline)
    (pp expr (current-buffer))))

(defvar *generator-test-last-body* nil)
(defun cps-testcase-1 (body)
  (setf *generator-test-last-body* (cps-generate-evaluator body))
  (let ((expected-result (eval body))
        (actual-result (eval *generator-test-last-body*)))
    (unless (equal expected-result actual-result)
      (error "CPS failure: got %S expected %S in %S"
             actual-result expected-result body))
    actual-result))

(defmacro cps-testcase (&rest body)
  `(cps-testcase-1 (quote ,(cons 'progn body))))

(defvar *cps-test-i* nil)

(defun cps-get-test-i ()
  *cps-test-i*)

(defun cps-run-tests ()
  (cps-testcase (progn 1 2 3))
  (cps-testcase (progn))
  (cps-testcase (inline 1 2 3))
  (cps-testcase (prog1 1 2 3))
  (cps-testcase (prog1 1))
  (cps-testcase (prog2 1 2 3))
  (cps-testcase (progn 'hello))
  (cps-testcase (progn #'hello))

  (cps-testcase (and 1 nil 2))
  (cps-testcase (and 1 2 3))
  (cps-testcase (and))

  (cps-testcase (or nil 1 2))
  (cps-testcase (or 1 2 3))
  (cps-testcase (or))

  (cps-testcase (let* ((i 10)) i))
  (cps-testcase (let ((i 10)) i))

  (cps-testcase
   (setq *cps-test-i* 0)
   (while (< *cps-test-i* 10)
     (setf *cps-test-i* (+ *cps-test-i* 1)))
   *cps-test-i*)

  (cps-testcase
   (let* ((i 0) (j 10))
     (while (< i 10)
       (setf i (+ i 1))
       (setf j (+ j (* i 10))))
     j))

  (cps-testcase
   (let* ((i 0) (j 10))
     (while (< i 10)
       (incf i)
       (setf j (+ j (* i 10))))
     j))

  (cps-testcase
   (setf *cps-test-i* 0)
   (let* ((*cps-test-i* 5))
     (cps-get-test-i)))

  (cps-testcase
   (+ (+ 3 5) 1))

  t)

(cps-run-tests)

;; Local Variables:
;; lexical-binding: t
;; End:
