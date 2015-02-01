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
(defpackage #:com.github.flpa.cl-tree.test
  (:use :common-lisp
	:com.github.flpa.cl-tree
	:5am)
  (:import-from :com.github.flpa.cl-tree
		::visible-p
		::base-name
		::remove-leading-dots
		::sort-with-hidden
		::filter-pathnames))

(in-package :com.github.flpa.cl-tree.test)

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
(def-suite test-base-name :in all)
(in-suite test-base-name)

(macrolet ((fn (desc in out)
	     `(test ,desc
		(is (equal (base-name (pathname ,in)) ,out)))))
  (fn single-dir "/home/" "home")
  (fn two-dirs "/home/dir/" "dir")
  (fn many-dirs "/home/is/great/and/a/dir/" "dir")
  (fn simple-file "/home/file" "file")
  (fn nested-file "/home/has/folders/file" "file")
  (fn file-with-ext "/home/file.c" "file.c"))

(def-suite test-remove-leading-dots :in all)
(in-suite test-remove-leading-dots)

(macrolet ((fn (desc in out)
	     `(test ,desc
		(is (equal (remove-leading-dots ,in) ,out)))))
  (fn no-ext "plain" "plain")
  (fn with-ext "main.c" "main.c")
  (fn hidden-no-ext ".hidden" "hidden")
  (fn hidden-with-ext ".hidden.asd" "hidden.asd")
  (fn empty "" "")
  (fn dot "." "")
  (fn two-dots ".." ""))

(def-suite test-sort-with-hidden :in all)
(in-suite test-sort-with-hidden)

(macrolet ((fn (desc in out)
	     `(test ,desc
		(is (equal ,out (sort-with-hidden ,in))))))
  (fn no-hidden '("c" "a" "b") '("a" "b" "c"))
  (fn single-hidden '("c" "a" ".b") '("a" ".b" "c"))
  (fn multiple-hidden '("alpha" ".aha" ".arm") '(".aha" "alpha" ".arm")))

(def-suite test-filter-pathnames :in all)
(in-suite test-filter-pathnames)

;; TODO: Could this global parameter cause problems? I couldn't get a 'let'-block surrounding
;;       all of his working, maybe that'd need 'eval-when'?
(defparameter *numbers-0to5* (loop for i from 0 to 5 collect i))

(macrolet ((fn (desc predicates out)
	     `(test ,desc
		(is (equal ,out (filter-pathnames *numbers-0to5* ,predicates))))))
  (fn filter-even (list #'evenp) '(0 2 4))
  (fn filter-even-greater-zero (list #'evenp #'(lambda (x) (> x 0))) '(2 4))
  (fn no-predicates '() *numbers-0to5*))

(def-suite test-print-report :in all)
(in-suite test-print-report)

(macrolet ((fn (desc dircount filecount out)
	     `(test ,desc
		(is (equal ,out
			   (with-output-to-string (*standard-output*)
			     (com.github.flpa.cl-tree::print-report ,dircount ,filecount)))))))
  (fn 3dirs-2files 3 2 "
3 directories, 2 files")
  (fn 1dir-1file 1 1 "
1 directory, 1 file")
  (fn 0dirs-0files 0 0 "
0 directories, 0 files"))

(def-suite test-visible-p :in all)
(in-suite test-visible-p)

(test visible-file
  (is-true (visible-p #P"/tmp/main.c")))
(test hidden-file
  (is-false (visible-p #P"/tmp/.config")))
(test visible-directory
  (is-true (visible-p #P"/tmp/")))
(test hidden-directory
  (is-false (visible-p #P"/tmp/.git/")))
