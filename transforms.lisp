(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria anaphora iterate destructuring-match provide-toplevel) :silent t)

(defpackage transforms
  (:export enable-transforms)
  (:use cl iterate anaphora destructuring-match provide-toplevel))

(in-package transforms)

(defparameter *transforms* nil)

(defun symbol-eq (x y)
   (and (symbolp x) (symbolp y) (string= (string x) (string y))))

(defun walking-replace (form function)
   (sb-walker:walk-form form nil
      (lambda (subform context env)
         (declare (ignore context env))
         (funcall function subform))))

(defmacro add-transform (name function)
   `(push (list ',name ,function) *transforms*))

(defun enable-transforms (forms)
   (if (symbolp forms)
       (enable-transforms (list forms))
       (iter (for form in forms)
             (aif (second (assoc form *transforms* :test #'symbol-eq))
                  (funcall it)
                  (error "unknown transform ~a" form)))))

(defun walking-hook (lam)
   (lambda ()
      (add-hook (lambda (form)
         (walking-replace form lam)))))

(add-transform right-arrow-lambda
   (walking-hook
      (lambda (form)
         (bind :on-failure form 
               :binding-mode multiple 
               (lhs '-> rhs) form `(lambda (,@lhs) ,@rhs)))))
