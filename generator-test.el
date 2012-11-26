;; -*- lexical-binding: t -*-
(require 'generator)
(require 'ert)

(defun generator-list-subrs ()
  (loop for x being the symbols
        when (and (fboundp x)
                  (cps--special-form-p (symbol-function x)))
        collect x))

(defvar *generator-test-last-body* nil)
(defun cps-testcase-1 (body)
  (let ((lexical-binding t))
    (setf *generator-test-last-body* (cps-generate-evaluator body))
    (let ((expected-result (eval body))
          (actual-result (eval *generator-test-last-body*)))
      (unless (equal expected-result actual-result)
        (error "CPS failure: got %S expected %S in %S"
               actual-result expected-result body))
      actual-result)))

(defmacro cps-testcase (name &rest body)
  `(ert-deftest ,name ()
       (cps-testcase-1 '(progn ,@body))))

(put 'cps-testcase 'lisp-indent-function 1)

(defvar *cps-test-i* nil)
(defun cps-get-test-i ()
  *cps-test-i*)

(cps-testcase cps-simple-1 (progn 1 2 3))
(cps-testcase cps-empty-progn (progn))
(cps-testcase cps-inline-not-progn (inline 1 2 3))
(cps-testcase cps-prog1-a (prog1 1 2 3))
(cps-testcase cps-prog1-b (prog1 1))
(cps-testcase cps-prog1-c (prog2 1 2 3))
(cps-testcase cps-quote (progn 'hello))
(cps-testcase cps-function (progn #'hello))

(cps-testcase cps-and-fail (and 1 nil 2))
(cps-testcase cps-and-succeed (and 1 2 3))
(cps-testcase cps-and-empty (and))

(cps-testcase cps-or-fallthrough (or nil 1 2))
(cps-testcase cps-or-alltrue (or 1 2 3))
(cps-testcase cps-or-empty (or))

(cps-testcase cps-let* (let* ((i 10)) i))
(cps-testcase cps-let (let ((i 10)) i))

(cps-testcase cps-while-dynamic
  (setq *cps-test-i* 0)
  (while (< *cps-test-i* 10)
    (setf *cps-test-i* (+ *cps-test-i* 1)))
  *cps-test-i*)

(cps-testcase cps-while-lexical
 (let* ((i 0) (j 10))
   (while (< i 10)
     (setf i (+ i 1))
     (setf j (+ j (* i 10))))
   j))

(cps-testcase cps-while-incf
 (let* ((i 0) (j 10))
   (while (< i 10)
     (incf i)
     (setf j (+ j (* i 10))))
   j))

(cps-testcase cps-dynbind
 (setf *cps-test-i* 0)
 (let* ((*cps-test-i* 5))
   (cps-get-test-i)))

(cps-testcase cps-nested-application
 (+ (+ 3 5) 1))

(cps-testcase cps-unwind-protect
 (setf *cps-test-i* 0)
 (unwind-protect
     (setf *cps-test-i* 1)
   (setf *cps-test-i* 2))
 *cps-test-i*)

(cps-testcase cps-catch-unused
 (catch 'mytag 42))

(cps-testcase cps-catch-thrown
 (1+ (catch 'mytag
       (throw 'mytag (+ 2 2)))))

(cps-testcase cps-loop
 (loop for x from 1 to 10 collect x))

(cps-testcase cps-loop-backquote
 `(a b ,(loop for x from 1 to 10 collect x) -1))

(cps-testcase cps-if-branch-a
 (if t 'abc))

(cps-testcase cps-if-branch-b
 (if t 'abc 'def))

(cps-testcase cps-if-condition-fail
 (if nil 'abc 'def))

(cps-testcase cps-cond-empty
 (cond))

(cps-testcase cps-cond-atomi
 (cond (42)))

(cps-testcase cps-cond-complex
 (cond (nil 22) ((1+ 1) 42) (t 'bad)))

(put 'cps-test-error 'error-conditions '(cps-test-condition))

(cps-testcase cps-condition-case
  (condition-case
      condvar
      (signal 'cps-test-error 'test-data)
    (cps-test-condition condvar)))

(cps-testcase cps-condition-case-no-error
  (condition-case
      condvar
      42
    (cps-test-condition condvar)))
