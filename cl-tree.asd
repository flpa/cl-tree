;; cl-tree: Reimplementing Linux 'tree' command in Common Lisp
;; Copyright (C) 2015 Florian Patzl 
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;; -----------------------------------------------------------------------------

(in-package :asdf-user)

(defsystem cl-tree
  :description "Common Lisp implementation of Unix 'tree' command."
  :version "0.0.1"
  :author "Florian Patzl"
  :licence "GPL3"
  :serial t
  :depends-on (:pathnames)
  :components ((:file "packages")
	       (:file "tree"))
  :entry-point "com.github.flpa.cl-tree:tree-tmpa")


(defsystem cl-tree/test
  :depends-on (cl-tree fiveam)
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam  '#:run!
                       (uiop:find-symbol* '#:all
					  '#:com.github.flpa.cl-tree.test)))
  :description "Test system for cl-tree"
  :version "0.0.1"
  :author "Florian Patzl"
  :licence "GPL3"
  :serial t
  :components ((:file "packages")
	       (:file "tests")))
