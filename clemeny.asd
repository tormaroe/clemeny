;;;; clemeny.asd

(asdf:defsystem #:clemeny
  :description "A BASIC to Common Lisp compiler"
  :author "Torbjørn Marø <torbjorn.maro@gmail.com>"
  :license "MIT"
  :depends-on (#:esrap #:alexandria #:cl-arrows)
  :components ((:file "package")
  			   (:file "grammar" :depends-on ("package"))
  			   (:file "compilation" :depends-on ("package"))
               (:file "clemeny" :depends-on ("package" "grammar" "compilation"))))

