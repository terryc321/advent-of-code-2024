;;;; day5.asd

(asdf:defsystem #:day5
  :depends-on ("uiop" "cl-ppcre" "split-sequence")
  :description "Describe day5 here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "day5")
               (:file "fun")
               ))

