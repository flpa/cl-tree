;;;; cl-tree: Reimplementing Unix 'tree' command in Common Lisp
;;;; Copyright (C) 2015 Florian Patzl 
;;;;
;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.
;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;;; GNU General Public License for more details.
;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <http://www.gnu.org/licenses/>.
;;;; -----------------------------------------------------------------------------------------------

(in-package :asdf-user)

(defsystem cl-tree
  :description "Common Lisp implementation of Unix 'tree' command."
  :version "0.0.2"
  :author "Florian Patzl"
  :licence "GPL3"
  :depends-on (#:unix-options) ; TODO depending on uiop did not work?
  :serial t
  :components ((:file "core")
	       (:file "cmdline")
	       (:file "util"))
  :entry-point "com.github.flpa.cl-tree.cmdline:tree-cmd")

(defsystem cl-tree/test
  :description "Test system for cl-tree"
  :version "0.0.2"
  :author "Florian Patzl"
  :licence "GPL3"
  :depends-on (#:cl-tree #:fiveam)
  :serial t
  :components ((:file "test/suites")
	       (:file "test/core")
	       (:file "test/cmdline")
	       (:file "test/util"))
  :perform (test-op (o s)
                    (uiop:symbol-call :fiveam  '#:run!
                       (uiop:find-symbol* '#:all
					  '#:com.github.flpa.cl-tree.test.suites))))
