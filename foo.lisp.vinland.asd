;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.vinland"
  :version "1.4.1"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://www.vinland.dev/"
  :bug-tracker "https://github.com/lisplizards/vinland/issues"
  :source-control (:git "https://github.com/lisplizards/vinland.git")
  :depends-on ("foo.lisp.http-response"
               "foo.lisp.lack-middleware-flash"
               "foo.lisp.lack-middleware-errors"
               "foo.lisp.lack-request"
               "foo.lisp.params"
               "foo.lisp.raven"
               "foo.lisp.resource"
               "lack-app-file"
               "lack-component"
               "lack-middleware-csrf"
               "lack-response"
               "uiop")
  :components ((:module "src"
                :components
                ((:file "web" :depends-on ("package" "specials"))
                 (:module "errors-app" :depends-on ("package" "handler")
                  :components
                  ((:file "util")
                   (:module "simple" :depends-on ("util")
                    :components
                    ((:file "dynamic-override")
                     (:file "basic")))))
                 (:module "handler" :depends-on ("package" "specials")
                  :components
                  ((:file "simple" :depends-on ("types"))
                   (:file "types")))
                 (:file "params" :depends-on ("package"))
                 (:file "specials" :depends-on ("package"))
                 (:file "static" :depends-on ("package"))
                 (:file "package"))))
  :description "Web framework for epic applications"
  :in-order-to ((test-op (test-op "foo.lisp.vinland/tests"))))

(defsystem "foo.lisp.vinland/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("alexandria"
               "foo.lisp.vinland"
               "jonathan"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "web" :depends-on ("package"))
                 (:file "params" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.vinland"
  :perform (test-op (op c) (symbol-call :rove :run c)))
