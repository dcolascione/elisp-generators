;; -*- lexical-binding: t -*-
;;; generator.el --- Provide generators to elisp

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

(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(assert lexical-binding)

(defvar *cps-bindings* nil)
(defvar *cps-states* nil)
(defvar *cps-value-symbol* nil)
(defvar *cps-state-symbol* nil)

(defvar *cps-dynamic-wrappers* '(identity)
  "List of transformer functions to apply to atomic forms we
evaluate in CPS context.")

(defconst cps-standard-special-forms
  '(setq setq-default throw interactive)
  "List of special forms that we treat just like ordinary
  function applications." )

(defun cps--debug-funcall (func &rest args)
  (message "XXX:%S: args=%S" func args)
  (let ((result (apply func args)))
    (message "XXX:%S: result=%S" func result)
    result))

(defun cps--special-form-p (definition)
  "Non-nil if and only if DEFINITION is a special form."
  ;; Copied from ad-special-form-p
  (if (and (symbolp definition) (fboundp definition))
      (setf definition (indirect-function definition)))
  (and (subrp definition) (eq (cdr (subr-arity definition)) 'unevalled)))

(defmacro cps--define-unsupported (function)
  `(defun ,(intern (format "cps--transform-%s" function))
     (error "%s not supported in generators" ,function)))

(defmacro cps--with-value-wrapper (wrapper &rest body)
  "Continue generating CPS code with an atomic-form wrapper
to the current stack of such wrappers.  WRAPPER is a function that
takes a form and returns a wrapped form.

Whenever we generate an atomic form (i.e., a form that can't
yield), we first (before actually inserting that form in our
generated code) pass that form through all the transformer
functions.  We use this facility to wrap forms that can transfer
control flow non-locally in goo that diverts this control flow to
the CPS state machinery.
"
  `(let ((*cps-dynamic-wrappers*
          (cons
           ,wrapper
           *cps-dynamic-wrappers*)))
     ,@body))
(put 'cps--with-value-wrapper 'lisp-indent-function 1)

(defun cps--make-dynamic-binding-wrapper (dynamic-var static-var)
  (assert lexical-binding)
  (lambda (form)
    `(let ((,dynamic-var ,static-var))
       (unwind-protect ; Update the static shadow after evaluation is done
           ,form
         (setf ,static-var ,dynamic-var))
       ,form)))

(defmacro cps--with-dynamic-binding (dynamic-var static-var &rest body)
  "Evaluate BODY such that generated atomic evaluations run with
DYNAMIC-VAR bound to STATIC-VAR."
  `(cps--with-value-wrapper
       (cps--make-dynamic-binding-wrapper ,dynamic-var ,static-var)
     ,@body))
(put 'cps--with-dynamic-binding 'lisp-indent-function 2)

(defun cps--add-state (kind body)
  "Create a new CPS state with body BODY and return the state's name."
  (let* ((state (cl-gensym (format "cps-state-%s-" kind))))
    (push (cons state body) *cps-states*)
    (push state *cps-bindings*)
    state))
(put 'cps--add-state 'lisp-indent-function 1)

(defun cps--add-binding (original-name)
  (car (push (cl-gensym (format "cps-binding-%s-" original-name))
             *cps-bindings*)))

(defun cps--find-special-form-handler (form)
  (let* ((handler-name (format "cps--transform-%s" (car-safe form)))
         (handler (intern-soft handler-name)))
    (and (fboundp handler) handler)))

(defvar cps-disable-atomic-optimization nil
  "When t, always rewrite forms into cps even when they
don't yield.")

(defvar cps--yield-seen)

(defun cps--atomic-p (form)
  "Return whether the given form never yields."

  (and (not cps-disable-atomic-optimization)
       (let* ((cps--yield-seen)
              (foo (macroexpand-all
                    `(macrolet ((cps-internal-yield
                                 (_val)
                                 (setf cps--yield-seen t)))
                       ,form))))
         (not cps--yield-seen))))

(defun cps--transform-1 (form next-state)
  (pcase form

    ;; If we're looking at an "atomic" form (i.e., one that does not
    ;; yield), just evaluate the form as a whole instead of rewriting
    ;; it into CPS.

    ((guard (cps--atomic-p form))
     (let ((tform `(prog1 ,form (setf ,*cps-state-symbol* ,next-state))))
       (loop for wrapper in *cps-dynamic-wrappers*
             do (setf tform (funcall wrapper tform)))
       (cps--add-state "atom"
         `(setf ,*cps-value-symbol* ,tform))))

    ;; Process `and'.

    (`(and)                             ; (and) -> t
     (cps--transform-1 t next-state))
    (`(and ,condition)                  ; (and CONDITION) -> CONDITION
     (cps--transform-1 condition next-state))
    (`(and ,condition . ,rest)
     ;; Evaluate CONDITION; if it's true, go on to evaluate the rest
     ;; of the `and'.
     (cps--transform-1
      condition
      (cps--add-state "and"
        `(setf ,*cps-state-symbol*
               (if ,*cps-value-symbol*
                   ,(cps--transform-1 `(and ,@rest)
                                      next-state)
                 ,next-state)))))

    ;; Process `catch'.

    (`(catch ,tag . ,body)
     (let ((tag-binding (cps--add-binding "catch-tag")))
       (cps--transform-1 tag
                         (cps--add-state "cps-update-tag"
                           `(setf ,tag-binding ,*cps-value-symbol*
                                  ,*cps-state-symbol*
                                  ,(cps--with-value-wrapper
                                       (cps--make-catch-wrapper
                                        tag-binding next-state)
                                     (cps--transform-1 `(progn ,@body)
                                                       next-state)))))))

    ;; Process `cond': transform into `if' or `or' depending on the
    ;; precise kind of the condition we're looking at.

    (`(cond)                            ; (cond) -> nil
     (cps--transform-1 nil next-state))
    (`(cond (,condition) . ,rest)
     (cps--transform-1 `(or ,condition (cond ,@rest))
                       next-state))
    (`(cond (,condition . ,body) . ,rest)
     (cps--transform-1 `(if ,condition
                            (progn ,@body)
                          (cond ,@rest))
                       next-state))

    ;; Process `condition-case': do the heavy lifting in a helper
    ;; function.

    (`(condition-case ,var ,bodyform . ,handlers)
     (cps--with-value-wrapper
         (cps--make-condition-wrapper var next-state handlers)
       (cps--transform-1 bodyform
                         next-state)))

    ;; Process `if'.

    (`(if ,cond ,then . ,else)
     (cps--transform-1 cond
                       (cps--add-state "if"
                         `(setf ,*cps-state-symbol*
                                (if ,*cps-value-symbol*
                                    ,(cps--transform-1 then
                                                       next-state)
                                  ,(cps--transform-1 `(progn ,@else)
                                                     next-state))))))

    ;; Process `progn' and `inline': they are identical except for the
    ;; name, which has some significance to the byte compiler.

    (`(inline) (cps--transform-1 nil next-state))
    (`(inline ,form) (cps--transform-1 form next-state))
    (`(inline ,form . ,rest)
     (cps--transform-1 form
                       (cps--transform-1 `(inline ,@rest)
                                         next-state)))

    (`(progn) (cps--transform-1 nil next-state))
    (`(progn ,form) (cps--transform-1 form next-state))
    (`(progn ,form . ,rest)
     (cps--transform-1 form
                       (cps--transform-1 `(progn ,@rest)
                                         next-state)))

    ;; Process `let' in a helper function that transforms it into a
    ;; let* with temporaries.

    (`(let ,bindings . ,body)
     (let* ((bindings (loop for binding in bindings
                            collect (if (symbolp binding)
                                        (list binding nil)
                                      binding)))
            (temps (loop for (var value-form) in bindings
                         collect (cps--add-binding var))))
       (cps--transform-1
        `(let* ,(append
                 (loop for (var value-form) in bindings
                       for temp in temps
                       collect (list temp value-form))
                 (loop for (var binding) in bindings
                       for temp in temps
                       collect (list var temp)))
           ,@body)
        next-state)))

    ;; Process `let*' binding: process one binding at a time.  Flatten
    ;; lexical bindings.

    (`(let* () . ,body)
     (cps--transform-1 `(progn ,@body) next-state))

    (`(let* (,binding . ,more-bindings) . ,body)
     (let* ((var (if (symbolp binding) binding (car binding)))
            (value-form (car (cdr-safe binding)))
            (new-var (cps--add-binding var)))

       (cps--transform-1
        value-form
        (cps--add-state "let*"
          `(setf ,new-var ,*cps-value-symbol*
                 ,*cps-state-symbol*
                 ,(if (or (not lexical-binding) (special-variable-p var))
                      (cps--with-dynamic-binding var new-var
                        (cps--transform-1
                         `(let* ,more-bindings ,@body)
                         next-state))
                    (cps--transform-1
                     (cps--replace-variable-references
                      var new-var
                      `(let* ,more-bindings ,@body))
                     next-state)))))))

    ;; Process `or'.

    (`(or) (cps--transform-1 nil next-state))
    (`(or ,condition) (cps--transform-1 condition next-state))
    (`(or ,condition . ,rest)
     (cps--transform-1
      condition
      (cps--add-state "or"
        `(setf ,*cps-state-symbol*
               (if ,*cps-value-symbol*
                   ,next-state
                 ,(cps--transform-1
                   `(or ,@rest) next-state))))))

    ;; Process `prog1'.

    (`(prog1 ,first) (cps--transform-1 first next-state))
    (`(prog1 ,first . ,body)
     (cps--transform-1
      first
      (let ((temp-var-symbol (cps--add-binding "prog1-temp")))
        (cps--add-state "prog1"
          `(setf ,temp-var-symbol
                 ,*cps-value-symbol*
                 ,*cps-state-symbol*
                 ,(cps--transform-1
                   `(progn ,@body)
                   (cps--add-state "prog1inner"
                     `(setf ,*cps-value-symbol* ,temp-var-symbol
                            ,*cps-state-symbol* ,next-state))))))))

    ;; Process `prog2'.

    (`(prog2 ,form1 ,form2 . ,body)
     (cps--transform-1
      `(progn ,form1 (prog1 ,form2 ,@body))
      next-state))

    ;; Process `unwind-protect': If we're inside an unwind-protect, we
    ;; have a block of code UNWINDFORMS which we would like to run
    ;; whenever control flows away from the main piece of code,
    ;; BODYFORM.  We deal with the local control flow case by
    ;; generating BODYFORM such that it yields to a continuation that
    ;; executes UNWINDFORMS, which then yields to NEXT-STATE.
    ;;
    ;; Non-local control flow is trickier: we need to ensure that we
    ;; execute UNWINDFORMS even when control bypasses our normal
    ;; continuation.  To make this guarantee, we wrap every external
    ;; application (i.e., every piece of elisp that can transfer
    ;; control non-locally) in an unwind-protect that runs UNWINDFORMS
    ;; before allowing the non-local control transfer to proceed.
    ;;
    ;; Unfortunately, because elisp lacks a mechanism for generically
    ;; capturing the reason for an arbitrary non-local control
    ;; transfer and restarting the transfer at a later point, we
    ;; cannot reify non-local transfers and cannot allow
    ;; continuation-passing code inside UNWINDFORMS.

    (`(unwind-protect ,bodyform . ,unwindforms)
     (cps--with-value-wrapper
         ;; On the non-local path
         (cps--make-unwind-wrapper unwindforms)

       ;; On the local control flow path, we have BODYFORMS yield to
       ;; the "unwind" state, which leaves the current value forms
       ;; alone --- we just run UNWINDFORMS as a monolithic block and
       ;; throw away its value, resuming execution as if we weren't in
       ;; `unwind-protect' at all.
       (cps--transform-1
        bodyform
        (cps--add-state "unwind"
          `(progn
             ,@unwindforms
             (setf ,*cps-state-symbol* ,next-state))))))

    ;; Process `while'.

    (`(while ,test . ,body)
     ;; Open-code state addition instead of using cps--add-state: we
     ;; need our states to be self-referential. (That's what makes the
     ;; state a loop.)
     (let* ((loop-state
             (cl-gensym "cps-state-while-"))
            (eval-loop-condition-state
             (cps--transform-1 test loop-state))
            (loop-state-body
             `(progn
                (setf ,*cps-state-symbol*
                      (if ,*cps-value-symbol*
                          ,(cps--transform-1
                            `(progn ,@body)
                            eval-loop-condition-state)
                        ,next-state)))))
       (push (cons loop-state loop-state-body) *cps-states*)
       (push loop-state *cps-bindings*)
       eval-loop-condition-state))

    ;; Process various kinds of `quote'.

    (`(quote ,arg) (cps--add-state "quote"
                     `(setf ,*cps-value-symbol* (quote ,arg)
                            ,*cps-state-symbol* ,next-state)))
    (`(function ,arg) (cps--add-state "function"
                        `(setf ,*cps-value-symbol* (function ,arg)
                               ,*cps-state-symbol* ,next-state)))

    ;; Deal with `yield'.

    (`(cps-internal-yield ,value)
     (cps--transform-1
      value
      (cps--add-state "yield"
        `(progn
           (setf ,*cps-state-symbol* ,next-state)
           (throw 'cps-yield ,*cps-value-symbol*)))))

    ;; Catch any unhandled special forms.

    ((and `(,name . ,_)
          (guard (cps--special-form-p name))
          (guard (not (memq name cps-standard-special-forms))))
     name ; Shut up byte compiler
     (error "special form %S incorrect or not supported" form))

    ;; Process regular function applications with nontrivial
    ;; parameters, converting them to applications of trivial
    ;; let-bound parameters.

    ((and `(,function . ,arguments)
          (guard (not (loop for argument in arguments
                            always (atom argument)))))
     (let ((argument-symbols
            (loop for argument in arguments
                  collect (if (atom argument)
                              argument
                            (cl-gensym "cps-argument-")))))

       (cps--transform-1
        `(let* ,(loop for argument in arguments
                      for argument-symbol in argument-symbols
                      unless (eq argument argument-symbol)
                      collect (list argument-symbol argument))
           ,(cons function argument-symbols))
        next-state)))

    ;; Process everything else by doing a plain eval at runtime.

    (t
     (let ((tform `(prog1 ,form (setf ,*cps-state-symbol* ,next-state))))
       (loop for wrapper in *cps-dynamic-wrappers*
             do (setf tform (funcall wrapper tform)))
       (cps--add-state "atom"
         `(setf ,*cps-value-symbol* ,tform))))))

(defun cps--make-catch-wrapper (tag-binding next-state)
  (lambda (form)
    (let ((normal-exit-symbol
           (cl-gensym "cps-normal-exit-from-catch-")))
      `(let (,normal-exit-symbol)
         (prog1
             (catch ,tag-binding
               (prog1
                   ,form
                 (setf ,normal-exit-symbol t)))
           (unless ,normal-exit-symbol
             (setf ,*cps-state-symbol* ,next-state)))))))

(defun cps--make-condition-wrapper (var next-state handlers)
  ;; Each handler is both one of the transformers with which we wrap
  ;; evaluated atomic forms and a state to which we jump when we
  ;; encounter the given error.

  (let* ((error-symbol (cps--add-binding "condition-case-error"))
         (lexical-error-symbol (cl-gensym "cps-lexical-error-"))
         (processed-handlers
          (loop for (condition . body) in handlers
                collect (cons condition
                              (cps--transform-1
                               (cps--replace-variable-references
                                var error-symbol
                                `(progn ,@body))
                               next-state)))))

    (lambda (form)
      `(condition-case
           ,lexical-error-symbol
           ,form
         ,@(loop
            for (condition . error-state) in processed-handlers
            collect
            `(,condition
              (setf ,error-symbol
                    ,lexical-error-symbol
                    ,*cps-state-symbol*
                    ,error-state)))))))

(defun cps--replace-variable-references (var new-var form)
  "Replace all non-shadowed references to VAR with NEW-VAR in FORM.
This routine does not modify FORM, instead returning a modified
copy."
  (macroexpand-all
   `(cl-symbol-macrolet ((,var ,new-var)) ,form)))

(defun cps--make-unwind-wrapper (unwind-forms)
  (assert lexical-binding)
  (lambda (form)
    (let ((normal-exit-symbol
           (cl-gensym "cps-normal-exit-from-unwind-")))
      `(let (,normal-exit-symbol)
         (unwind-protect
             (prog1
                 ,form
               (setf ,normal-exit-symbol t))
           (unless ,normal-exit-symbol
             ,unwind-forms))))))

(put 'stop-iteration 'error-conditions '(stop-iteration))

(defun cps-generate-evaluator (form)
  (let* (*cps-states*
         *cps-bindings*
         (*cps-value-symbol* (cl-gensym "cps-current-value-"))
         (*cps-state-symbol* (cl-gensym "cps-current-state-"))
         (terminal-state (cps--add-state "terminal"
                           '(signal 'stop-iteration t)))
         (initial-state (cps--transform-1
                         (macroexpand-all form)
                         terminal-state)))

    `(let ,(list* *cps-state-symbol* *cps-value-symbol* *cps-bindings*)
       ,@(loop for (state . body) in *cps-states*
               collect `(setf ,state (lambda () ,body)))
       (setf ,*cps-state-symbol* ,initial-state)
       (lambda (yield-return)
         (setf ,*cps-value-symbol* yield-return)
         (catch 'cps-yield
           (while t
             (funcall ,*cps-state-symbol*)))))))

(defmacro yield (_value)
  (error "`yield' used outside a generator"))

(defmacro defgenerator (name arglist &rest body)
  "Creates a generator NAME.

When called as a function, NAME returns a closure that
encapsulates the state of a computation.  When a form in BODY
calls `yield' with a value, this closure returns that value.
Calling the closure again resumes computation from the point
after the call to `yield'. "

  (cl-assert lexical-binding)

  `(defun ,name ,arglist
     ,(cps-generate-evaluator
       `(macrolet ((yield (value) `(cps-internal-yield ,value)))
          ,@body))))

(put 'defgenerator 'lisp-indent-function 'defun)

(defmacro lambda-generator (arglist &rest body)
  "Return a lambda generator.

lambda-generator is to defgenerator as lambda is to defun."

  (cl-assert lexical-binding)

  `(lambda ,arglist
     ,(cps-generate-evaluator
       `(macrolet ((yield (value) `(cps-internal-yield ,value)))
          ,@body))))

(put 'lambda-generator 'lisp-indent-function 'defun)

(defun next (iterator &optional yield-result)
  "Extract a value from an iterator.
YIELD-RESULT becomes the return value of `yield` in the
context of the generator.

This routine raises the stop-iteration condition if the iterator
cannot supply more values.

"

  (funcall iterator yield-result))

(defmacro* do-iterator ((var iterator &optional result) &rest body)
  "Loop over values from an iterator.
Evaluate BODY with VAR bound to each value from ITERATOR.  Then
evaluate RESULT to get return value, default nil."
  (let ((done-symbol (cl-gensym "iterator-done"))
        (it-symbol (cl-gensym "iterator")))
    `(let (,var
           (,done-symbol nil)
           (,it-symbol ,iterator))
       (while (not ,done-symbol)
         (condition-case nil
             (setf ,var (next ,it-symbol))
           (stop-iteration
            (setf ,done-symbol t)))
         (unless ,done-symbol ,@body))
       ,result)))

(put 'do-iterator 'lisp-indent-function 1)

(defvar cl--loop-args)

(defmacro cps--advance-for (conscell)
  ;; See cps--handle-loop-for
  `(condition-case nil
       (progn
         (setcar ,conscell (next (cdr ,conscell)))
         ,conscell)
     (stop-iteration
      nil)))

(defmacro cps--initialize-for (iterator)
  ;; See cps--handle-loop-for
  (let ((cs (cl-gensym "cps--loop-temp")))
    `(let ((,cs (cons nil ,iterator)))
       (cps--advance-for ,cs))))

(defun cps--handle-loop-for (var)
  "Support `iterating' in `loop'.  "

  ;; N.B. While the cl-loop-for-handler is a documented interface,
  ;; there's no documented way for cl-loop-for-handler callbacks to do
  ;; anything useful!  Additionally, cl-loop currently lexbinds useful
  ;; internal variables, so our only option is to modify
  ;; cl--loop-args.  If we substitute a general-purpose for-clause for
  ;; our iterating clause, however, we can't preserve the
  ;; parallel-versus-sequential `loop' semantics for for clauses ---
  ;; we need a terminating condition as well, which requires us to use
  ;; while, and inserting a while would break and-sequencing.
  ;;
  ;; To work around this problem, we actually use the "for var in LIST
  ;; by FUNCTION" syntax, creating a new fake list each time through
  ;; the loop, this "list" being a cons cell (val . it).

  (let ((it-form (pop cl--loop-args)))
    (setf cl--loop-args
          (append
           `(for ,var
                 in (cps--initialize-for ,it-form)
                 by 'cps--advance-for)
           cl--loop-args))))

(put 'iterating 'cl-loop-for-handler 'cps--handle-loop-for)

(provide 'generator)

;;; generator.el ends here
