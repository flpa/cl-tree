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
(defpackage #:com.github.flpa.cl-tree.test.core
  (:use :common-lisp
	:5am
        :com.github.flpa.cl-tree.test.suites)
  (:import-from :com.github.flpa.cl-tree.core
		::visible-p
		::base-name
		::remove-leading-dots
		::sort-with-hidden
		::filter-pathnames
                ::print-report
                ::build-predicates
                ::file-or-non-empty-dir-p)
  (:import-from :uiop/pathname
		:directory-pathname-p)
  (:import-from :com.github.flpa.cl-tree.util
                :with-temporary-directory))

(in-package :com.github.flpa.cl-tree.test.core)

;;; ---------------------------------------------
;;; See suites.lisp for explanations of the
;;; approach used in these tests.
;;; ---------------------------------------------

(def-suite core :in all)

(def-suite test-base-name :in core)
(in-suite test-base-name)

(macrolet ((fn (desc in expected)
	     `(test ,desc
		(is (equal ,expected (base-name (pathname ,in)))))))
  (fn single-dir "/home/" "home")
  (fn two-dirs "/home/dir/" "dir")
  (fn many-dirs "/home/is/great/and/a/dir/" "dir")
  (fn simple-file "/home/file" "file")
  (fn nested-file "/home/has/folders/file" "file")
  (fn file-with-ext "/home/file.c" "file.c"))

(def-suite test-remove-leading-dots :in core)
(in-suite test-remove-leading-dots)

(macrolet ((fn (desc in expected)
	     `(test ,desc
		(is (equal ,expected (remove-leading-dots ,in))))))
  (fn no-ext "plain" "plain")
  (fn with-ext "main.c" "main.c")
  (fn hidden-no-ext ".hidden" "hidden")
  (fn hidden-with-ext ".hidden.asd" "hidden.asd")
  (fn empty "" "")
  (fn dot "." "")
  (fn two-dots ".." ""))

(def-suite test-sort-with-hidden :in core)
(in-suite test-sort-with-hidden)

(macrolet ((fn (desc in expected)
	     `(test ,desc
		(is (equal ,expected (sort-with-hidden ,in))))))
  (fn no-hidden '("c" "a" "b") '("a" "b" "c"))
  (fn single-hidden '("c" "a" ".b") '("a" ".b" "c"))
  (fn multiple-hidden '("alpha" ".aha" ".arm") '(".aha" "alpha" ".arm")))

(def-suite test-filter-pathnames :in core)
(in-suite test-filter-pathnames)

;; TODO: Could this global parameter cause problems? I couldn't get a 'let'-block surrounding
;;       all of his working, maybe that'd need 'eval-when'?
(defparameter *numbers-0to5* (loop for i from 0 to 5 collect i))

(macrolet ((fn (desc predicates expected)
	     `(test ,desc
		(is (equal ,expected (filter-pathnames *numbers-0to5* ,predicates))))))
  (fn filter-even (list #'evenp) '(0 2 4))
  (fn filter-even-greater-zero (list #'evenp #'(lambda (x) (> x 0))) '(2 4))
  (fn no-predicates '() *numbers-0to5*))

(def-suite test-print-report :in core)
(in-suite test-print-report)

(macrolet ((fn (desc dircount filecount expected)
	     `(test ,desc
		(is (equal (format nil "~%~A" ,expected)
			   (with-output-to-string (*standard-output*)
			     (print-report ,dircount ,filecount)))))))
  (fn 3dirs-2files 3 2 "3 directories, 2 files")
  (fn 1dir-1file 1 1 "1 directory, 1 file")
  (fn 0dirs-0files 0 0 "0 directories, 0 files"))

(def-suite test-visible-p :in core)
(in-suite test-visible-p)

(test visible-file
  (is-true (visible-p #P"/tmp/main.c")))
(test hidden-file
  (is-false (visible-p #P"/tmp/.config")))
(test visible-directory
  (is-true (visible-p #P"/tmp/")))
(test hidden-directory
  (is-false (visible-p #P"/tmp/.git/")))

(def-suite test-file-or-non-empty-dir-p :in core)
(in-suite test-file-or-non-empty-dir-p)

(test file
  (is-true (file-or-non-empty-dir-p #P"/tmp/main.c")))
(test non-empty-dir
  (with-temporary-directory 
    (ensure-directories-exist (merge-pathnames #P"a/" directory))
    (is-true (file-or-non-empty-dir-p directory))))
(test empty-dir
  (with-temporary-directory 
    (is-false (file-or-non-empty-dir-p directory))))


(def-suite test-build-predicates :in core)
(in-suite test-build-predicates)

;; TODO: This test is hard to read and will need adaptions whenever a new parameter is added.
;;       Is there a better way?
(macrolet ((fn (desc (show-hidden directories-only prune-empty) expected)
	     `(test ,desc 
		(is-false (set-difference ,expected (build-predicates ,show-hidden 
                                                                 ,directories-only 
                                                                 ,prune-empty))))))
  (fn no-params-only-hidden-filtered (nil nil nil) (list #'visible-p))
  (fn with-hidden-no-predicates (t nil nil) '()) 
  (fn directories-only (t t nil) (list #'directory-pathname-p)) 
  (fn prune-empty (t nil t) (list #'file-or-non-empty-dir-p)))
