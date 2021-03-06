;; -*- lexical-binding: t -*-
;;; generator-test.el --- Testcases for generator.el

;; Copyright (C) Daniel Colascione

;; Author: Daniel Colascione <dancol@dancol.org>
;; Keywords: elisp

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; See README.md.

;;; Code:

(require 'generator)
(require 'ert)

(defun generator-list-subrs ()
  (loop for x being the symbols
        when (and (fboundp x)
                  (cps--special-form-p (symbol-function x)))
        collect x))

(defmacro cps-testcase (name &rest body)
  "Perform a simple test of the continuation-transforming code.

`cps-testcase' defines an ERT testcase called NAME that evaluates
BODY twice: once using ordinary `eval' and once using
lambda-generators.  The test ensures that the two forms produce
identical output.
"
  `(progn
     (ert-deftest ,name ()
       (should
        (equal
         (funcall (lambda () ,@body))
         (next
          (funcall
           (lambda-generator () (yield (progn ,@body))))))))
     (ert-deftest ,(intern (format "%s-noopt" name)) ()
       (should
        (equal
         (funcall (lambda () ,@body))
         (next
          (funcall
           (let ((cps-disable-atomic-optimization t))
             (lambda-generator () (yield (progn ,@body)))))))))))

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
(cps-testcase cps-let*-shadow-empty (let* ((i 10)) (let (i) i)))
(cps-testcase cps-let (let ((i 10)) i))
(cps-testcase cps-let-shadow-empty (let ((i 10)) (let (i) i)))
(cps-testcase cps-let-novars (let nil 42))
(cps-testcase cps-let*-novars (let* nil 42))

(cps-testcase cps-let-parallel
  (let ((a 5) (b 6)) (let ((a b) (b a)) (list a b))))

(cps-testcase cps-let*-parallel
  (let* ((a 5) (b 6)) (let* ((a b) (b a)) (list a b))))

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

(ert-deftest cps-generator-basic ()
  (let* ((gen (lambda-generator ()
                (yield 1)
                (yield 2)
                (yield 3)))
         (gen-inst (funcall gen)))
    (should (eql (next gen-inst) 1))
    (should (eql (next gen-inst) 2))
    (should (eql (next gen-inst) 3))

    ;; should-error doesn't seem to catch the generator-end case, so
    ;; we write our own.

    (let (errored)
      (condition-case nil
          (next gen-inst)
        (stop-iteration
         (setf errored t)))
      (should errored))))

(defgenerator mygenerator (i)
  (yield 1)
  (yield i)
  (yield 2))

(ert-deftest cps-test-do-iterator ()
  (let (mylist)
    (do-iterator (x (mygenerator 4))
      (push x mylist))

    (assert (equal mylist '(2 4 1)))))

(defgenerator gen-using-yield-value ()
  (let (f)
    (setf f (yield 42))
    (yield f)))

(ert-deftest cps-yield-value ()
  (let ((it (gen-using-yield-value)))
    (should (eql (next it -1) 42))
    (should (eql (next it -1) -1))))

(ert-deftest cps-loop ()
  (should
   (equal (loop for x iterating (mygenerator 42)
                collect x)
          '(1 42 2))))

