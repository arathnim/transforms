(proclaim '(optimize (speed 0) (safety 3) (debug 3)))

(ql:quickload '(alexandria anaphora iterate destructuring-match provide-toplevel) :silent t)

(defpackage transforms
  (:nicknames tf)
  (:export enable-transforms add-transform walking-hook transform)
  (:use cl iterate anaphora destructuring-match provide-toplevel))

(in-package transforms)

(defparameter *transforms* nil)

(defvar *gensym* (make-hash-table :test #'equalp))
(defun gen-sym (&optional (prefix "gs"))
   (setf (gethash prefix *gensym*) (+ 1 (gethash prefix *gensym* 0)))
   (intern (string-upcase (format nil "~a~a" prefix (gethash prefix *gensym*)))))

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

(defun transform (form)
   (provide-toplevel::apply-hooks form))

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

(add-transform underscore-lambda
   (walking-hook
      (lambda (form)
         (bind :on-failure form 
               :binding-mode multiple 
               (fun '_ (optional rest)) form (generate-hole-lambda '_ form)))))

(add-transform hole-lambda
   (walking-hook
      (lambda (form)
         (bind :on-failure form 
               :binding-mode multiple 
               (fun '<> (optional rest)) form (generate-hole-lambda '<> form)))))

(defun generate-hole-lambda (symbol form)
   (iter (for src in form)
         (collect 
            (if (symbol-eq symbol src)
                (let ((gs (gen-sym "lambda-arg")))
                      (collect gs into vars)
                      gs)
                src)
            into body)
         (finally (return `(lambda (,@vars) ,body)))))
