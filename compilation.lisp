;;;; compilation.lisp


(in-package #:compilation)

(defun compile-expression (exp)
  (flet ((compile-elm (x)
          (typecase x
            (string (intern x))
            (t x))))
    (if (listp exp)
      (mapcar #'compile-elm exp)
      (compile-elm exp))))

(defun compile-line (line)
  (let ((lbl (car line))
        (cmd (cadr line)))
    (list lbl
      (when cmd
        (switch ((car cmd) :test #'equal)
          ("END" '(return))
          ("GOTO" `(go ,(cadr cmd)))
          ("PRINT" `(format t "~A~%" ,(intern (cadr cmd))))
          ("IF" `(when ,(compile-expression (cadr cmd)) 
                   (go ,(third cmd))))
          ("LET" `(setf ,(intern (cadr cmd)) 
                        ,(compile-expression (third cmd)))))))))

(defun trim-trailing-nil (list)
  (if (null (car (last list)))
    (butlast list)
    list))

(defun find-all-variables (body)
  (->> body
    (remove-if-not (lambda (x) 
                     (and (listp x)
                          (eq (car x) 'setf))))
    (mapcar #'cadr)
    (remove-duplicates)))

(defun wrap-program-body (body)
  `(block nil 
    (let ,(find-all-variables body) 
      (tagbody ,@body))))

(defun compile-ast (ast)
  (setf ast (sort ast #'< :key #'car))
  (values (->> ast
            (mapcar #'compile-line)
            (mapcar #'trim-trailing-nil)
            (apply #'append)
            (wrap-program-body))
          ast))