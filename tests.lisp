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

(in-package :cl-user)
(defpackage :com.github.flpa.cl-tree.test
  (:use :common-lisp
	:com.github.flpa.cl-tree
	:5am))

(in-package :com.github.flpa.cl-tree.test)

;; TODO: well that escalated quickly... lots of boilerplate code.
;;       Revert to 1 test per method? Simplify by using macros?

(def-suite all)

(def-suite test-base-name :in all)
(in-suite test-base-name)

;; (defmacro basename-test (name in exp)
;;   `(test ,name
;;     (is (string-equal (base-name (pathname ,in)) ,exp))))

(test single-dir
  (is (string-equal "home" (base-name #p"/home/"))))

(test two-dirs
  (is (string-equal "dir" (base-name #p"/home/dir/"))))

(test many-dirs
  (is (string-equal "dir" (base-name #p"/home/is/great/and/a/dir/"))))

(test simple-file
  (is (string-equal "file" (base-name #p"/home/file"))))

(test nested-file
  (is (string-equal "file" (base-name #p"/home/has/folders/file"))))

(test file-with-ext
  (is (string-equal "file.c" (base-name #p"/home/has/folders/file.c"))))


(def-suite test-remove-leading-dots :in all)
(in-suite test-remove-leading-dots)

(test no-ext
  (is (string-equal "plain" (remove-leading-dots "plain"))))

(test ext
  (is (string-equal "main.c" (remove-leading-dots "main.c"))))

(test hidden-no-ext
  (is (string-equal "hidden" (remove-leading-dots ".hidden"))))

(test hidden-ext
  (is (string-equal "hidden.asd" (remove-leading-dots ".hidden.asd"))))

(test empty
  (is (string-equal "" (remove-leading-dots ""))))

(test dot
  (is (string-equal "" (remove-leading-dots "."))))

(test two-dots
  (is (string-equal "" (remove-leading-dots ".."))))


(def-suite test-sort-with-hidden :in all)
(in-suite test-sort-with-hidden)

(test no-hidden
  (equal `("a" "b" "c") (sort-with-hidden `("c" "a" "b"))))

(test hidden
  (equal `("a" ".b" "c") (sort-with-hidden `("c" "a" ".b"))))

(test multiple-hidden
  (equal `(".aha" "alpha" ".arm") (sort-with-hidden `("alpha" ".aha" ".arm"))))


(def-suite test-filter-pathnames :in all)
(in-suite test-filter-pathnames)

(defparameter *numbers* (loop for i from 0 to 5 collect i)) 

(test one-predicate	
  (is (equal '(0 2 4) (filter-pathnames *numbers* (list #'evenp)))))

(test two-predicates
  (is (equal '(2 4)
	     (filter-pathnames *numbers*
			       (list #'evenp
				     #'(lambda (x) (> x 0)))))))
(test no-predicates
  (is (equal *numbers* (filter-pathnames *numbers* nil))))
