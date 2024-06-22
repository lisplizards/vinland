(in-package #:cl-user)

(defpackage #:<% @var name %>/config
  (:use #:cl)
  (:export #:*system-directory*
           #:*static-errors-directory*)
  (:documentation "Package containing configuration-related special variables."))

(defpackage #:<% @var name %>/dao
  (:use #:cl)
  (:documentation "Package containing Database Access Object class or struct definitions."))

(defpackage #:<% @var name %>/store
  (:use #:cl #:<% @var name %>/dao)
  (:documentation "Package containing persistence related functions."))

(defpackage #:<% @var name %>/http-error
  (:use #:cl)
  (:import-from #:<% @var name %>/config
                #:*static-errors-directory*)
  (:export #:*static-file-types*
           #:*required-handlers*
           #:*http-errors*)
  (:export #:generate-static)
  (:documentation "Package containing functions used to render HTTP error responses."))

(defpackage #:<% @var name %>/json
  (:use #:cl #:<% @var name %>/dao)
  (:import-from #:make-hash
                #:make-hash)
  (:import-from #:foo.lisp.vinland/web
                #:route-url
                #:route-path)
  (:export #:root)
  (:documentation "Package containing JSON document generation functions called from controllers."))

(defpackage #:<% @var name %>/user
  (:use #:cl)
  (:import-from #:lack/middleware/user
                #:*current-user*)
  (:documentation "Package containing functions related to user authentication and authorization."))

(defpackage #:<% @var name %>/controller
  (:use #:cl)
  (:import-from #:make-hash
                #:make-hash)
  (:import-from #:lack/middleware/user
                #:*current-user*)
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
                #:route-url
                #:route-path
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
                #:negotiate
                #:render
                #:redirect
                #:set-response-status
                #:set-response-headers)
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
  (:documentation "Package containing *APP*, i.e., the Clack application wrapped in a middleware pipeline."))

(defpackage #:<% @var name %>/cli
  (:use #:cl)
  (:export #:main)
  (:documentation "Program entrypoint package."))
