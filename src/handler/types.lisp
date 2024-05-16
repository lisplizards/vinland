;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/handler/types)

(deftype keyword-list ()
  `(and list
        (satisfies keyword-list-p)))

(deftype string-list ()
  `(and list
        (satisfies string-list-p)))

(deftype binding-alist ()
  `(and list
        cons-list
        (satisfies binding-alist-p)))

(deftype cons-list ()
  `(and list
        (satisfies cons-list-p)))

(deftype function-list ()
  `(and list
        (satisfies function-list-p)))

(defun keyword-list-p (lst)
  (declare (type list lst))
  (every #'keywordp lst))

(defun string-list-p (lst)
  (declare (type list lst))
  (every #'stringp lst))

(defun cons-list-p (lst)
  (declare (type list lst))
  (every #'consp lst))

(defun binding-alist-p (lst)
  (declare (type cons-list lst))
  (every #'(lambda (cons)
             (and (keywordp (car cons))
                  (stringp (cdr cons))))
         lst))

(defun function-list-p (lst)
  (declare (type list lst))
  (every #'functionp lst))
