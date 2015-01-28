(in-package :asdf-user)

(defsystem "cl-tree"
  :description "Common Lisp implementation of Unix 'tree' command."
  :version "0.0.1"
  :author "Florian Patzl"
  :licence "Public Domain"
  :serial t
  :depends-on (:pathnames)
  :components ((:file "packages")
	       (:file "tree"))
  :entry-point "com.github.flpa.cl-tree:tree-tmpa")


;; draft from asdf docu: defining test package to
;;
;; (defsystem foo/test
;;   :depends-on (foo fiveam) ; fiveam is a test framework library
;;   :perform (test-op (o s)
;;                     (uiop:symbol-call :fiveam  '#:run!
;;                        (uiop:find-symbol* '#:foo-test-suite
;;                                             :foo-tests)))
;;   ...)


	       
