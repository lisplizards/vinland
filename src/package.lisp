;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.vinland/params
  (:use #:cl)
  (:export #:validate-params
           #:%validate-params)
  (:documentation "Package for validation of request parameters; non-essential but can be called from route handlers."))

(defpackage #:foo.lisp.vinland/static
  (:use #:cl)
  (:export #:generate-error-files)
  (:documentation "Package for generation of static assets, including error pages."))

(defpackage #:foo.lisp.vinland/web
  (:use #:cl)
  (:import-from #:foo.lisp.raven
                #:route-path)
  (:import-from #:foo.lisp.http-response
                #:client-error
                #:server-error)
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
           #:append-response-headers
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
           #:render
           #:route-url
           #:route-path)
  (:export #:redirect-not-allowed-error
           #:unsafe-redirect-error
           #:double-render-error
           #:invalid-binding-error
           #:http-error
           #:client-error
           #:server-error)
  (:export #:html-safe
           #:html-safe-value
           #:make-html-safe)
  (:export #:respond
           #:redirect
           #:redirect-back
           #:importmap)
  (:documentation "Exports macros and functions for HTTP related operations, intended to be called from route handlers."))

(defpackage #:foo.lisp.vinland/handler/types
  (:use #:cl)
  (:export #:keyword-list
           #:string-list
           #:binding-alist
           #:cons-list
           #:function-list)
  (:documentation "Types intended to be used from handler (sub-protocol) packages."))

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
  (:documentation "Defines the \"simple\" handler sub-protocol (identifier symbol: ROUTE/SIMPLE)."))

(defpackage #:foo.lisp.vinland/errors-app/util
  (:use #:cl)
  (:export #:*root-directory*
           #:*media-type-fallback*
           #:*handler-media-types*
           #:*handlers*
           #:*static-file-types*
           #:*static-file-namestrings*
           #:*static-media-types*
           #:*default-static-file-types*
           #:*recommended-http-error-response-status-codes*
           #:*http-error-response-status-codes*)
  (:documentation "Defines common utilities for use with error applications"))

(defpackage #:foo.lisp.vinland/errors-app/simple/basic
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/errors-app/util
                #:*root-directory*
                #:*media-type-fallback*
                #:*handlers*
                #:*handler-media-types*
                #:*static-file-types*
                #:*static-file-namestrings*
                #:*static-media-types*
                #:*default-static-file-types*
                #:*recommended-http-error-response-status-codes*
                #:*http-error-response-status-codes*)
  (:export #:make-app)
  (:export #:*recommended-http-error-response-status-codes*
           #:*http-error-response-status-codes*)
  (:documentation "Defines the \"basic\" errors application for use with LACK/MIDDLEWARE/ERRORS."))

(defpackage #:foo.lisp.vinland/errors-app/simple/dynamic-override
  (:use #:cl)
  (:import-from #:foo.lisp.vinland/errors-app/util
                #:*root-directory*
                #:*media-type-fallback*
                #:*handlers*
                #:*handler-media-types*
                #:*static-file-types*
                #:*static-file-namestrings*
                #:*static-media-types*
                #:*default-static-file-types*
                #:*recommended-http-error-response-status-codes*
                #:*http-error-response-status-codes*)
  (:export #:make-app)
  (:export #:*recommended-http-error-response-status-codes*
           #:*http-error-response-status-codes*)
  (:documentation "Defines the \"dynamic-override\" errors application for use with LACK/MIDDLEWARE/ERRORS."))

(uiop:define-package #:foo.lisp.vinland
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
  (:documentation "Package that exports special variables and sub-protocol identifiers."))
