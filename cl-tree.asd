(in-package :asdf-user)

(defsystem "cl-tree"
  :description "Common Lisp implementation of Unix 'tree' command."
  :version "0.0.1"
  :author "Florian Patzl"
  :licence "Public Domain"
  :serial t
  :depends-on (:pathnames)
  :components ((:file "packages")
	       (:file "tree")))


	       
