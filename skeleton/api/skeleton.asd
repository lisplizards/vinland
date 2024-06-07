(defsystem "<% @var name %>"
  <%- @if long-name %>
  :long-name "<% @var long-name %>"
  <%- @endif %>
  <%- @if version %>
  :version "<% @var version %>"
  <%- @endif %>
  <%- @if author %>
  :author "<% @var author %>"
  <%- @else %>
  :author nil
  <%- @endif %>
  <%- @if maintainer %>
  :maintainer "<% @var maintainer %>"
  <%- @endif %>
  <%- @if license %>
  :license"<% @var license %>"
  <%- @endif %>
  :build-operation "program-op"
  :build-pathname "bin/<% @var program %>"
  :entry-point "<% @var name %>/cli:main"
  <%- @if homepage %>
  :homepage "<% @var homepage %>"
  <%- @endif %>
  <%- @if bug-tracker %>
  :bug-tracker "<% @var bug-tracker %>"
  <%- @endif %>
  <%- @if git-remote %>
  :source-control (:git "<% @var git-remote %>")
  <%- @else %>
  <%- @if source-control %>
  :source-control "<% @var source-control %>"
  <%- @endif %>
  <%- @endif %>
  :depends-on ("cl-jschema"
               "clingon"
               "com.inuoe.jzon"
               "foo.lisp.lack-middleware-debug"
               "foo.lisp.lack-middleware-errors"
               "foo.lisp.lack-middleware-head"
               "foo.lisp.lack-middleware-http-methods"
               "foo.lisp.lack-middleware-redact"
               "foo.lisp.lack-middleware-redis"
               "foo.lisp.lack-middleware-request-id"
               "foo.lisp.lack-middleware-user"
               "foo.lisp.vinland"
               "frugal-uuid"
               "lack"
               "lack-middleware-backtrace"
               "local-time"
               "make-hash"
               "safety-params"
               "trivia"
               "woo")
  :components ((:module "src"
                :components
                ((:file "cli" :depends-on ("app"))
                 (:file "app" :depends-on ("http-error" "web"))
                 (:file "web" :depends-on ("controller" "params"))
                 (:file "params" :depends-on ("controller"))
                 (:file "controller" :depends-on ("json" "store" "user"))
                 (:file "user" :depends-on ("store"))
                 (:file "store" :depends-on ("dao"))
                 (:file "json" :depends-on ("dao"))
                 (:file "http-error" :depends-on ("config"))
                 (:file "dao" :depends-on ("config"))
                 (:file "config" :depends-on ("package"))
                 (:file "package"))))
  :description "<% @var description %>"
  :in-order-to ((test-op (test-op "<% @var name %>/tests"))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t
                   :compression t))

(defsystem "<% @var name %>/tests"
  <%- @if author %>
  :author "<% @var author %>"
  <%- @endif %>
  <%- @if maintainer %>
  :maintainer "<% @var maintainer %>"
  <%- @endif %>
  <%- @if license %>
  :license "<% @var license %>"
  <%- @endif %>
  :depends-on ("<% @var name %>"
               <%- @if rove %>
               "rove"
               <%- @endif %>
               <%- @if parachute %>
               "parachute"
               <%- @endif %>)
  :components ((:module "tests"
                :components
                ((:file "cli" :depends-on ("package"))
                 (:file "app" :depends-on ("package"))
                 (:file "web" :depends-on ("package"))
                 (:file "params" :depends-on ("package"))
                 (:file "controller" :depends-on ("package"))
                 (:file "user" :depends-on ("package"))
                 (:file "store" :depends-on ("package"))
                 (:file "json" :depends-on ("package"))
                 (:file "http-error" :depends-on ("package"))
                 (:file "dao" :depends-on ("package"))
                 (:file "config" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for <% @var name %>"
  <%- @if rove %>
  :perform (test-op (op c) (symbol-call :rove :run c)))
  <%- @endif %>
  <%- @if parachute %>
  :perform (test-op (op c)
                    (progn
                      (symbol-call :parachute :test :<% @var name %>/config)
                      (symbol-call :parachute :test :<% @var name %>/dao)
                      (symbol-call :parachute :test :<% @var name %>/json)
                      (symbol-call :parachute :test :<% @var name %>/http-error)
                      (symbol-call :parachute :test :<% @var name %>/store)
                      (symbol-call :parachute :test :<% @var name %>/user)
                      (symbol-call :parachute :test :<% @var name %>/controller)
                      (symbol-call :parachute :test :<% @var name %>/params)
                      (symbol-call :parachute :test :<% @var name %>/web)
                      (symbol-call :parachute :test :<% @var name %>/app)
                      (symbol-call :parachute :test :<% @var name %>/cli))))
  <%- @endif %>
