;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/params)

(defmacro validate-params (params)
  "Calls generic function %VALIDATE-PARAMS to validate PARAMS for the current
route-name and request method; a macro."
  `(%validate-params foo.lisp.vinland:*route*
                     (lack/request:request-method foo.lisp.vinland:*request*)
                     ,params))

(defgeneric %validate-params (route-name request-method params)
  (:documentation "Validates and sanitizes PARAMS."))
