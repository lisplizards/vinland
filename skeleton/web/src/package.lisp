(in-package #:cl-user)

(defpackage #:<% @var name %>/config
  (:use #:cl)
  (:export #:*system-directory*
           #:*static-directory*)
  (:documentation "Package containing configuration-related special variables."))

(defpackage #:<% @var name %>/dao
  (:use #:cl)
  (:documentation "Package containing Database Access Object class or struct definitions."))

(defpackage #:<% @var name %>/store
  (:use #:cl #:<% @var name %>/dao)
  (:documentation "Package containing persistence related functions."))

(defpackage #:<% @var name %>/component
  (:use #:cl)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:export #:flash-container
           #:flash
           #:site-header
           #:site-footer)
  (:documentation "Package containing building-block functions to compose HTML views."))

(defpackage #:<% @var name %>/layout
  (:use #:cl)
  (:import-from #:make-hash
                #:make-hash)
  (:export #:with-main-layout)
  (:documentation "Package containing HTML layout macros called from view functions."))

(defpackage #:<% @var name %>/view
  (:use #:cl)
  (:import-from #:<% @var name %>/layout
                #:with-main-layout)
  (:export #:root)
  (:documentation "Package containing functions to render complete HTML documents (Content-Type: text/html)."))

<%- @unless skip-hotwire %>
(defpackage #:<% @var name %>/turbo
  (:use #:cl)
  (:documentation "Package containing functions to render Hotwire Turbo documents (Content-Type: text/vnd.turbo-stream.html)."))

<%- @endunless %>
(defpackage #:<% @var name %>/user
  (:use #:cl)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:import-from #:foo.lisp.vinland/web
                #:halt
                #:redirect
                #:set-response-headers)
  (:documentation "Package containing functions related to user sessions"))

(defpackage #:<% @var name %>/controller
  (:use #:cl)
  (:import-from #:make-hash
                #:make-hash)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:import-from #:foo.lisp.flash
                #:clear-flash
                #:get-flash
                #:flash
                #:flash-now
                #:flash-keep
                #:delete-flash)
  (:import-from #:foo.lisp.params
                #:get-param
                #:get-nested-param
                #:collect-params
                #:collect-nested-params)
  (:import-from #:foo.lisp.vinland/params
                #:validate-params)
  (:import-from #:foo.lisp.vinland/handler/simple
                #:define-controller)
  (:import-from #:foo.lisp.vinland/web
                #:halt
                #:client-error
                #:server-error
                #:binding
                #:body-params
                #:query-params
                #:get-body-param
                #:get-query-param
                #:collect-body-params
                #:collect-query-params
                #:html-safe
                #:negotiate
                #:render
                #:redirect
                #:redirect-back
                #:set-response-status
                #:set-response-headers
                #:cookie
                #:set-cookies
                #:delete-cookie
                #:set-session-options
                #:session
                #:set-session
                #:clear-session)
  (:export #:root)
  (:documentation "Package containing controllers."))

(defpackage #:<% @var name %>/params
  (:use #:cl)
  (:import-from #:safety-params
                #:validation-error
                #:invalid-keys
                #:missing-keys
                #:unpermitted-keys
                #:validate
                #:alist
                #:requires
                #:satisfies)
  (:import-from #:foo.lisp.vinland/params
                #:%validate-params)
  (:import-from #:foo.lisp.params
                #:get-param
                #:get-nested-param
                #:collect-params
                #:collect-nested-params)
  (:documentation "Package containing generic functions used to validate request query or body parameters."))

(defpackage #:<% @var name %>/web
  (:use #:cl #:<% @var name %>/controller)
  (:export #:*router*
           #:*web*)
  (:documentation "Package containing special variables required to start the application."))

(defpackage #:<% @var name %>/app
  (:use #:cl)
  (:export #:*app*)
  (:documentation "Package containing *APP*, the Clack application *WEB* wrapped in a middleware pipeline."))

(defpackage #:<% @var name %>/cli
  (:use #:cl)
  (:export #:main)
  (:documentation "Program entrypoint package."))
