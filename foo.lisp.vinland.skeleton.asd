;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.vinland.skeleton"
  :version "1.1.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://www.vinland.dev/"
  :bug-tracker "https://github.com/lisplizards/vinland/issues"
  :source-control (:git "https://github.com/lisplizards/vinland.git")
  :depends-on ("cl-project"
               "uiop")
  :components ((:module "src"
                :components
               ((:module "skeleton"
                 :components
                 ((:file "main" :depends-on ("package"))
                  (:file "package"))))))
  :description "Project skeleton generator for Vinland applications."
  :in-order-to ((test-op (test-op "foo.lisp.vinland.skeleton/tests"))))

(defsystem "foo.lisp.vinland.skeleton/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.vinland.skeleton"
               "rove")
  :components ((:module "tests"
                :components
                ((:module "skeleton"
                 :components
                 ((:file "main" :depends-on ("package"))
                  (:file "package"))))))
  :description "Test system for foo.lisp.vinland.skeleton"
  :perform (test-op (op c) (symbol-call :rove :run c)))
