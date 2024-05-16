;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.vinland/response
  (:use #:cl)
  (:export #:status-code
           #:status-code-informational
           #:status-code-success
           #:status-code-redirect
           #:status-code-client-error
           #:status-code-server-error)
  (:export #:unknown-status-error)
  (:export #:status-code-to-keyword
           #:status-code-to-text
           #:status-keyword-to-code
           #:status-keyword-to-text
           #:status-text
           #:status-text-clack-response)
  (:documentation "HTTP response related utilities"))

(defpackage #:foo.lisp.vinland/params
  (:use #:cl)
  (:export #:validate-params
           #:%validate-params)
  (:documentation "Used to validate request parameters; non-essential but can be used in route handlers"))

(defpackage #:foo.lisp.vinland/web
  (:use #:cl)
  (:export #:query-params
           #:body-params
           #:get-query-params
           #:get-body-params
           #:get-query-param
           #:get-body-param
           #:collect-query-params
           #:collect-body-params
           #:collect-nested-body-params
           #:negotiate
           #:set-response-status
           #:set-response-headers
           #:binding
           #:cookie
           #:set-cookies
           #:delete-cookie
           #:set-session-options
           #:session
           #:set-session
           #:delete-session
           #:clear-session
           #:csrf-token
           #:halt
           #:render)
  (:export #:unauthorized-redirect-error
           #:double-render-error
           #:invalid-binding-error
           #:client-error
           #:server-error)
  (:export #:html-safe
           #:html-safe-value
           #:make-html-safe)
  (:export #:respond
           #:redirect
           #:redirect-back)
  (:documentation "Helper utilities around LACK.REQUEST and LACK.RESPONSE, to be called within the context
of route handlers"))

(defpackage #:foo.lisp.vinland/handler/types
  (:use #:cl)
  (:export #:keyword-list
           #:string-list
           #:binding-alist
           #:cons-list
           #:function-list))

(defpackage #:foo.lisp.vinland/handler/simple
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/handler/types
                #:keyword-list
                #:string-list
                #:binding-alist
                #:cons-list
                #:function-list)
  (:export #:route/simple
           #:define-controller)
  (:documentation "Defines the \"simple\" sub-protocol (identifier symbol: ROUTE/SIMPLE)"))

(defpackage #:foo.lisp.vinland
  (:use #:cl)
  (:import-from #:foo.lisp.resource
                #:*origin*)
  (:import-from #:foo.lisp.flash
                #:*flash*)
  (:import-from #:foo.lisp.vinland/handler/simple
                #:route/simple)
  (:export #:*request*
           #:*response*
           #:*route*
           #:*binding*
           #:*origin*
           #:*flash*)
  (:export #:route/simple)
  (:documentation "Package for special variables and sub-protocol identifiers"))
