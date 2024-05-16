;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland)

(defparameter *request* nil
  "The LACK.REQUEST object representing the current request; dynamically bound.")

(defparameter *response* nil
  "The LACK.RESPONSE object representing the response to return; dynamically bound")

(defparameter *route* nil
  "The symbol for the current route; dynamically bound")

(defparameter *binding* nil
  "Association list containing the current route's dynamic path components, with each binding
keyword as the cons CAR and the value as the CDR")
