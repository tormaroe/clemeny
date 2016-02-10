;;;; clemeny.lisp


(in-package #:clemeny)

(defun file-string (path)
  (with-open-file (stream path)
    (let ((data (make-string (file-length stream))))
      (read-sequence data stream)
      data)))

(defun parse-file (path)
  (parse (file-string path)))

(defun parse (source)
  (esrap:parse 'grammar:program source))

(defun run (path)
  (let ((source (file-string path)))
    (multiple-value-bind (code ast)
        (->> source (parse) (compile-ast))
      (format t "~&SOURCE CODE:~%~A~%" source)
      (format t "~&~%ABSTRACT SYNTAX TREE:~%")
      (print ast)
      (format t "~&~%COMPILED CODE:~%")
      (print code)
      (format t "~&~%RUNNING PROGRAM:~%")
      (eval code)
      (format t "~&~%DONE!~%"))))
