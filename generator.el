(eval-when-compile
  (require 'cl))

(defvar *cps-bindings* nil)
(defvar *cps-states* nil)
(defvar *cps-value-symbol* nil)
(defvar *cps-state-symbol* nil)

(defvar *cps-dynamic-bindings* nil
  "Alist of dynamic bindings in effect.
The keys may be any form recognized by LETF.")

(defun cps--special-form-p (definition)
  "Non-nil if and only if DEFINITION is a special form."
  ;; Copied from ad-special-form-p
  (if (and (symbolp definition) (fboundp definition))
      (setf definition (indirect-function definition)))
  (and (subrp definition) (eq (cdr (subr-arity definition)) 'unevalled)))

(defmacro cps--define-unsupported (function)
  `(defun ,(intern (format "cps--transform-%s" function))
     (error "%s not supported in generators" ,function)))

(defmacro cps--with-dynamic-binding (dynamic-var static-var &rest body)
  `(let ((*cps-dynamic-bindings*
          (cons (cons ,dynamic-var ,static-var) *cps-dynamic-bindings*)))
     ,@body))
(put 'cps--with-dynamic-binding 'lisp-indent-function 2)

(defun cps--add-state (kind body)
  "Create a new CPS state with body BODY and return the state's name."
  (let ((state (gensym (format "cps-state-%s-" kind))))
    (push (cons state body) *cps-states*)
    (push state *cps-bindings*)
    state))
(put 'cps--add-state 'lisp-indent-function 1)

(defun cps--add-binding (original-name)
  (car (push (gensym (format "cps-binding-%s-" original-name))
             *cps-bindings*)))

(defun cps--find-special-form-handler (form)
  (let* ((handler-name (format "cps--transform-%s" (car-safe form)))
         (handler (intern-soft handler-name)))
    (and (fboundp handler) handler)))

(defun cps--transform-1 (form next-state)
  (let (handler)
    (cond
     ((setf handler (cps--find-special-form-handler form))
      (funcall handler (cdr form) next-state))

     ((cps--special-form-p (car-safe form))
      (error "unhandled special form %s" form))

     ((and (car-safe form)
           (or (symbolp (car-safe form))
               (functionp (car-safe form))))
      (cps--transform-application form next-state))

     (t
      (cps--transform-atom form next-state)))))

(defun cps--transform-atom (form next-state)
  (when (loop for (dkey . dval) in *cps-dynamic-bindings*
              thereis dkey)
    (setf form `(let ,(loop for (dkey . dval) in *cps-dynamic-bindings*
                            when dkey
                            collect (list dkey dval))
                  ,form)))

  (cps--add-state "atom"
   `(progn
      (setf ,*cps-value-symbol* ,form)
      (setf ,*cps-state-symbol* ,next-state))))

(defun cps--transform-application (form next-state)
  (let* ((function (car form))
         (arguments (cdr form)))
    (if (loop for argument in arguments
              always (atom argument))
        (cps--transform-atom form next-state)
      (let ((argument-symbols
             (loop for argument in arguments
                   collect (if (atom argument)
                               argument
                             (gensym "cps-argument-")))))

        (cps--transform-1
         `(let* ,(loop for argument in arguments
                       for argument-symbol in argument-symbols
                       unless (eq argument argument-symbol)
                       collect (list argument-symbol argument))
            ,(cons function argument-symbols))
         next-state)))))

(defun cps--transform-and (form next-state)
  (if (null form) ; (AND) -> t
      (cps--transform-1 t next-state)
    (cps--transform-1
     (car form)
     (if (null (cdr form))
         ;; Base case: (and TERM) -> TERM
         next-state
       ;; Recurrence case: (and TERM1 REST...). Evaluate TERM1 and send
       ;; its value to our new state.  There, if TERM1 evaluated to
       ;; non-nil, ignore its value and evaluate remaining AND
       ;; conditions; if TERM1 evaluated to nil, go to NEXT-STATE
       ;; directly (skipping the evaluation of remaining conditions),
       ;; sending it the nil value.
       (cps--add-state "and"
        `(setf ,*cps-state-symbol*
               (if ,*cps-value-symbol*
                   ,(cps--transform-1 `(and ,@(cdr form)) next-state)
                 ,next-state)))))))

(cps--define-unsupported catch) ; XXX
(cps--define-unsupported throw)

(defun cps--transform-cond (form next-state)
  (let* ((condition (car form))
         (remaining-conditions (cdr form))
         (predicate (car condition))
         (condition-body (cdr condition)))

    ;; Generate a series of nested IF or OR invocations depending on
    ;; whether the current condition has a body.

    (cps--transform-1
     (cond ((null condition) nil)
           ((null condition-body)
            `(or ,predicate
                 ,(cond ,@remaining-conditions)))
           (t
            `(if ,predicate
                 (progn ,@condition-body)
               ,(cond ,@remaining-conditions))))
     next-state)))

(cps--define-unsupported condition-case) ; XXX
(cps--define-unsupported defvar)
(cps--define-unsupported defconst)

(defun cps--transform-function (form next-state)
  (cps--add-state "function"
   `(progn
      (setf ,*cps-value-symbol* (function ,@form))
      (setf ,*cps-state-symbol* ,next-state))))

(defun cps--transform-if (form next-state)
  (cps--transform-1
   (car form)
   (cps--add-state "if"
    `(setf ,*cps-state-symbol*
       (if ,*cps-value-symbol*
           ,(cps--transform-1 (cadr form))
         ,(cps-transform-1 `(progn ,@(cddr form))))))))

(defun cps--transform-inline (body next-state)
  (if (cdr body)
      ;; Evaluate FORM1, ignores its value, and goes on to evaluate
      ;; REST as if it were a nested INLINE.
      (cps--transform-1
       (car body)
       (cps--transform-1 `(inline ,@(cdr body)) next-state))
    ;; In the (inline FORM) case, just send the value of FORM to
    ;; NEXT-STATE.
    (cps--transform-1 (car body) next-state)))

(defun cps--transform-interactive (body next-state)
  ;; When actually evaluated, INTERACTIVE always evaluates to NIL.
  (cps--transform-1 nil next-state))

(defun cps--replace-variable-references (var new-var form)
  "Replace all non-shadowed references to VAR with NEW-VAR in FORM.
This routine does not modify FORM."
  (macroexpand-all
   `(cl-symbol-macrolet ((,var ,new-var)) ,form)))

(defun cps--transform-let (form next-state)
  ;; To preserve LET's parallel binding semantics, LET*-bind
  ;; temporaries to the value forms, then LET* bind the intended
  ;; variables to the temporaries.
  (let* ((bindings (car form))
         (temps (loop for (var value-form) in bindings
                      collect (cps--add-binding var))))
    (cps--transform-1
     `(let*
          ,(append
            (loop for (var value-form) in bindings
                  for temp in temps
                  collect (list temp value-form))
            (loop for (var binding) in bindings
                  for temp in temps
                  collect (list var temp)))
        ,@(cdr form))
     next-state)))

(defun cps--transform-let* (form next-state)
  (let* ((bindings (car form))
         (binding (car bindings))
         (var (if (symbolp binding) binding (car binding)))
         (value-form (car (cdr-safe binding))))

    (check-type var symbol)

    (if (null bindings)
        ;; Base base: no bindings, so LET* becomes PROGN.
      (cps--transform-1 `(progn ,@(cdr form)) next-state)

      ;; Recurrence case: handle the first binding and recurse to
      ;; handle others, if any.  We deal with a dynamic binding by
      ;; turning it into a lexical binding, then remembering to bind
      ;; the dynamic variable to the lexical one much later, when we
      ;; evaluate code that could observe the dynamic binding.

      (let ((new-var (cps--add-binding var)))
        (cps--transform-1
         value-form
         (cps--add-state "let*"
          `(progn
             (setf ,new-var ,*cps-value-symbol*)
             (setf ,*cps-state-symbol*
                   ,(if (or (not lexical-binding) (special-variable-p var))
                        (cps--with-dynamic-binding var new-var
                          (cps--transform-1
                           `(let* ,(cdr bindings) ,@(cdr form))
                           next-state))
                      (cps--transform-1
                       (cps--replace-variable-references
                        var new-var
                        `(let* ,(cdr bindings) ,@(cdr form)))
                       next-state))))))))))

(defun cps--transform-or (form next-state)
  (cps--transform-1
   (car form)
   (if (null form)
       ;; Base case: (or) -> nil
       (cps--transform-1 nil next-state)
     ;; Recurrence case: (or TERM1 REST...) Evaluate TERM1 and send
     ;; its value to our new state.  There, if TERM1 evaluated to
     ;; non-nil, send its value to NEXT-STATE; otherwise, treat the
     ;; remaining terms as a nested OR.
     (cps--add-state "or"
      `(setf ,*cps-state-symbol*
             (if ,*cps-value-symbol*
                 ,next-state
               ,(cps--transform-1
                 `(or ,@(cdr form))
                 next-state)))))))

(defun cps--transform-prog1 (form next-state)
  (if (null (cdr form))
      (cps--transform-atom (car form) next-state)
    (let ((temp-var-symbol (cps--add-binding "prog1-temp")))
      (cps--transform-1
       (car form)

       ;; Save the value of the first form into a temporary, evaluates
       ;; the rest of the form as if it were a progn, ignore the result,
       ;; and send the original value we saved to NEXT-STATE.

       (cps--add-state "prog1"
        `(progn
           (setf ,temp-var-symbol ,*cps-value-symbol*)
           (setf ,*cps-state-symbol*
                 ,(cps--transform-1
                   `(progn ,@(cdr form))
                   (cps--add-state "prog1inner"
                    `(progn
                       (setf ,*cps-value-symbol* ,temp-var-symbol)
                       (setf ,*cps-state-symbol* ,next-state)))))))))))

(defun cps--transform-prog2 (body next-state)
  (cps--transform-1
   `(progn
      ,(car body)
      (prog1
          ,(cadr body)
        ,@(cddr body)))
   next-state))

(defun cps--transform-progn (body next-state)
  (if (cdr body)
      ;; Evaluate FORM1, ignores its value, and goes on to evaluate
      ;; REST as if it were a nested PROGN.
      (cps--transform-1
       (car body)
       (cps--transform-1 `(progn ,@(cdr body)) next-state))
    ;; In the (progn FORM) case, just send the value of FORM to
    ;; NEXT-STATE.
    (cps--transform-1 (car body) next-state)))

(defun cps--transform-quote (form next-state)
  (cps--add-state "quote"
   `(progn
      (setf ,*cps-value-symbol* (quote ,@form))
      (setf ,*cps-state-symbol* ,next-state))))

(defun cps--transform-save-current-buffer (form next-state)
  (let ((current-buffer-symbol (gensym "cps-current-buffer-")))
    (cps--transform-1
     `(let ((,current-buffer-symbol (current-buffer)))
        (unwind-protect
            (progn ,@form)
          (if (buffer-live-p ,current-buffer-symbol)
            (set-buffer ,current-buffer-symbol))))
     next-state)))

(defun cps--save-excursion ()
  (list (current-buffer) (point) (mark)))

(defun cps--return-from-excursion (info)
  ;; XXX make this function act more like save_excursion_restore
  (set-buffer (first info))
  (goto-char (second info))
  (set-mark (third info)))

(defun cps--transform-save-excursion (form next-state)
  (let ((saved-excursion-symbol (gensym "cps-saved-excursion-")))
    (cps--with-dynamic-binding nil 'save-excursion
      (cps--transform-1
       `(let* ((,saved-excursion-symbol (cps--save-excursion)))
          (unwind-protect
              (progn ,@form)
            (cps--return-from-excursion ,saved-excursion-symbol)))
       next-state))))

(defun cps--save-restriction ()
  ;; See save_restriction_save
  (if (buffer-narrowed-p)
      (let ((begin-marker (make-marker))
            (end-marker (make-marker)))
        (set-marker begin-marker (point-min))
        (set-marker end-marker (point-max))
        (set-marker-insertion-type end-marker t)
        (list begin-marker end-marker))
    (current-buffer)))

(defun cps--return-from-restriction (data)
  ;; See save_restriction_restore
  (with-current-buffer (if (bufferp data)
                           data
                         (marker-buffer (car data)))

    (if (bufferp data)
        (widen)
      (narrow-to-region (car data) (cadr data)))))

(defun cps--transform-save-restriction (form next-state)
  (let ((saved-restriction-symbol (gensym "cps-saved-restriction-")))
    (cps--with-dynamic-binding nil 'save-restriction
      (cps--transform-1
       `(let* ((,saved-restriction-symbol (cps--save-restriction)))
          (unwind-protect
              (progn ,@form)
            (cps--return-from-restriction ,saved-restriction-symbol)))
       next-state))))

(defun cps--transform-setq (form next-state)
  (cps--add-state "setq"
   `(progn
      (setf ,*cps-value-symbol* (setq ,@form))
      (setf ,*cps-state-symbol* ,next-state))))

(defun cps--transform-setq-default (form next-state)
  (cps--add-state "setq-default"
   `(progn
      (setf ,*cps-value-symbol* (setq-default ,@form))
      (setf ,*cps-state-symbol* ,next-state))))

(cps--define-unsupported track-mouse)
(cps--define-unsupported unwind-protect) ;; XXX?!

(defun cps--transform-while (form next-state)
  (let* ((loop-state
          (gensym "cps-state-while-"))
         (eval-loop-condition-state
          (cps--transform-1 (car form) loop-state))
         (loop-state-body
          `(progn
             (setf ,*cps-state-symbol*
                   (if ,*cps-value-symbol*
                       ,(cps--transform-1
                         `(progn ,@(cdr form))
                         eval-loop-condition-state)
                     ,next-state)))))
    (push (cons loop-state loop-state-body) *cps-states*)
    (push loop-state *cps-bindings*)
    eval-loop-condition-state))

(defun cps-generate-evaluator (form)
  (let* (*cps-states*
         *cps-bindings*
         (*cps-value-symbol* (gensym "cps-current-value-"))
         (*cps-state-symbol* (gensym "cps-current-state-"))
         (terminal-state (cps--add-state "terminal" nil))
         (initial-state (cps--transform-1 form terminal-state)))

    `(let ,(list* *cps-state-symbol* *cps-value-symbol* *cps-bindings*)
       ,@(loop for (state . body) in *cps-states*
               collect `(setf ,state (lambda () ,body)))
       (setf ,*cps-state-symbol* ,initial-state)
       (while (not (eq ,*cps-state-symbol* ,terminal-state))
         (funcall ,*cps-state-symbol*))
       ,*cps-value-symbol*)))

(defmacro defgenerator (name arglist &rest body)
  "Like DEFUN, except that BODY may contain YIELD."
  )

(provide 'generator)

;; Local Variables:
;; lexical-binding: t
;; End:
