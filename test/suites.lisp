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
(defpackage #:com.github.flpa.cl-tree.test.suites
  (:use :common-lisp
	:5am)
  (:export :all))

(in-package :com.github.flpa.cl-tree.test.suites)

;;; Some words about the tests:
;;; I'm following the rule of only checking one case per test. This has the benefit that each check
;;; has a description: the test name. Failing tests immediately communicate which aspect of the
;;; method under test is broken. Because of this approach and the fact that most of the tests are
;;; single-line method invocations checking whether input results in the expected output, lots of
;;; boilerplate test code was necessary. Right now, this problem has been tackled by introducing
;;; the pattern of defining local macros, which provide test code templates, for each set of
;;; similar tests.
;;; I am confident that this solution will not appeal to everyone, maybe not even to me when
;;; revisiting the file in a couple of weeks. For now, I consider the benefit of reduced boilerplate
;;; code to outweigh the complexity added by macros.
;;;
;;; TODO: I could go one step further by writing a macro that handles
;;;         - suite creation
;;;         - test creation
;;;       , only receiving method under test, test fn prototype and tuples of desc, in, out?
;;;       That would rather be an extension of 5am, though.

(def-suite all)

