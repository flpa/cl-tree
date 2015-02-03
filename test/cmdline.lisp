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

(in-package :cl-user)
(defpackage #:com.github.flpa.cl-tree.test.cmdline
  (:use :common-lisp
        :5am
        :com.github.flpa.cl-tree.test.suites)
  (:import-from :com.github.flpa.cl-tree.cmdline
                ::collect-directory-pathnames)
  (:import-from :uiop/os
                :chdir))

(in-package :com.github.flpa.cl-tree.test.cmdline)

(def-suite cmdline :in all)

(def-suite test-collect-directory-pathnames :in cmdline)
(in-suite test-collect-directory-pathnames)

;; this is the correct is equal order! check others
;; TODO: consistency for macro param naming! out vs expected 
(macrolet ((fn (desc in out)
             `(test ,desc
                    (chdir #P"/tmp/") ; tests use /tmp/ as current working directory
                    (is (equal (mapcar #'pathname ,out) 
                               (collect-directory-pathnames ,in))))))
  (fn no-dirs-returns-cwd '() '("/tmp/./"))
  (fn relative-dir-merges-cwd '("folder/") '("/tmp/folder/"))
  (fn multiple-relatives '("a/" "b/") '("/tmp/a/" "/tmp/b/"))
  (fn absolute-not-changed '("/var/log/") '("/var/log/"))
  (fn trailing-slash-is-added '("/home") '("/home/")))
