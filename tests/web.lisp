(in-package #:foo.lisp.vinland/tests/web)

(deftest query-params
    (testing
     "Returns the parsed query parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:query-string "foo=bar"
                                          :headers ,(make-hash-table :test #'equal)))))
       (ok (equal '(("foo" . "bar"))
                  (foo.lisp.vinland/web:query-params))))))

(deftest get-query-param
    (testing
     "Returns the value of the query parameter with the given name, or NIL when not found"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:query-string "foo=bar"
                                          :headers ,(make-hash-table :test #'equal)))))
       (ok (string= "bar" (foo.lisp.vinland/web:get-query-param "foo")))
       (ok (null (foo.lisp.vinland/web:get-query-param "baaz"))))))

(deftest collect-query-params
    (testing
     "Returns a list of values for the given query parameter names"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:query-string "foo=bar&baaz=quux&baar=foobar"
                                          :headers ,(make-hash-table :test #'equal)))))
       (ok (equal '("bar" nil "foobar")
                  (foo.lisp.vinland/web:collect-query-params '("foo" "bogus" "baar")))))))

(deftest body-params
    (testing
     "Returns the parsed form body parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request
              `(:request-method :POST
                :request-uri "/"
                :content-type "application/x-www-form-urlencoded"
                :content-length 11
                :headers ,(alexandria:alist-hash-table
                           '(("content-type" . "application/x-www-form-urlencoded")))
                :raw-body ,(flex:make-flexi-stream
                            (flex:make-in-memory-input-stream
                             (flex:string-to-octets "hello=world"))
                            :external-format :utf-8)))))
       (ok (equal '(("hello" . "world"))
                  (foo.lisp.vinland/web:body-params)))))

  (testing
   "Returns the parsed JSON body parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request
            `(:request-method :POST
              :request-uri "/"
              :content-type "application/json"
              :content-length 17
              :headers ,(alexandria:alist-hash-table
                         '(("content-type" . "application/json")))
              :raw-body ,(flex:make-flexi-stream
                          (flex:make-in-memory-input-stream
                           (flex:string-to-octets "{\"hello\":\"world\"}"))
                          :external-format :utf-8)))))
     (ok (equal '(("hello" . "world"))
                (foo.lisp.vinland/web:body-params))))))

(deftest get-body-param
    (testing
     "Returns the value of the form body parameter with the given name, or NIL when not found"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request
              `(:request-method :POST
                :request-uri "/"
                :content-type "application/x-www-form-urlencoded"
                :content-length 11
                :headers ,(alexandria:alist-hash-table
                           '(("content-type" . "application/x-www-form-urlencoded")))
                :raw-body ,(flex:make-flexi-stream
                            (flex:make-in-memory-input-stream
                             (flex:string-to-octets "hello=world"))
                            :external-format :utf-8)))))
       (ok (equal "world" (foo.lisp.vinland/web:get-body-param "hello")))
       (ok (null (foo.lisp.vinland/web:get-body-param "foo")))))

  (testing
   "Returns the value of the JSON body parameter with the given name, or NIL when not found"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request
            `(:request-method :POST
              :request-uri "/"
              :content-type "application/json"
              :content-length 17
              :headers ,(alexandria:alist-hash-table
                         '(("content-type" . "application/json")))
              :raw-body ,(flex:make-flexi-stream
                          (flex:make-in-memory-input-stream
                           (flex:string-to-octets "{\"hello\":\"world\"}"))
                          :external-format :utf-8)))))
     (ok (string= "world" (foo.lisp.vinland/web:get-body-param "hello")))
     (ok (null (foo.lisp.vinland/web:get-body-param "foo"))))))

(deftest collect-body-params
    (testing
     "Maps the specified parameter names to their values"
     (let* ((form-data-str "foo=bar&baaz=quux&baar=foobar")
            (form-data-octets (flex:string-to-octets form-data-str :external-format :utf-8))
            (foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-method :POST
                 :request-uri "/"
                 :content-type "application/x-www-form-urlencoded"
                 :content-length ,(length form-data-octets)
                 :headers ,(alexandria:alist-hash-table
                            '(("content-type" . "application/x-www-form-urlencoded")))
                 :raw-body ,(flex:make-flexi-stream
                             (flex:make-in-memory-input-stream
                              form-data-octets)
                             :external-format :utf-8)))))
       (ok (equal '("bar" nil "foobar")
                  (foo.lisp.vinland/web:collect-body-params '("foo" "bogus" "baar")))))))

(deftest collect-nested-body-params
    (testing
     "Maps the specified parameter paths to their values"
     (let* ((json-data-str (jojo:to-json
                            '((:|qux| . 11)
                              (:|bar| . "baz")
                              (:|foo| . ((:|baaz| . "quux")
                                         (:|quux| . ((:|foo| . "bar")))
                                         (:|bar| . 33)
                                         (:|foobar| '(1 2 3)))))
                            :from :alist))
            (json-data-octets (flex:string-to-octets json-data-str :external-format :utf-8))
            (foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-method :POST
                 :request-uri "/"
                 :content-type "application/json"
                 :content-length ,(length json-data-octets)
                 :headers ,(alexandria:alist-hash-table
                            '(("content-type" . "application/json")))
                 :raw-body ,(flex:make-flexi-stream
                             (flex:make-in-memory-input-stream
                              json-data-octets)
                             :external-format :utf-8)))))
       (let ((result (foo.lisp.vinland/web:collect-nested-body-params
                      '(("qux")
                        ("bar")
                        ("bogus")
                        ("foo" "baaz")
                        ("foo" "bar")
                        ("foo" "bogus")
                        ("foo" "quux" "foo")))))
         (ok (equalp '(11 "baz" nil "quux" 33 nil "bar")
                     result))))))

(deftest negotiate
    (testing
     "performs content-negotiation by returning the body of the matching clause, checked against the Accept header"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request
              `(:request-uri "/"
                :request-method :GET
                :headers ,(alexandria:alist-hash-table
                           '(("accept" . "application/json"))
                           :test #'equal)))))
       (ok (equal "foo" (foo.lisp.vinland/web:negotiate
                          ("text/html"
                           "bar")
                          ("application/json"
                           "foo"))))))

  (testing
   "signals CLIENT-ERROR when no clause matches and no fallback is provided"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request
            `(:request-uri "/"
              :request-method :GET
              :headers ,(alexandria:alist-hash-table
                         '(("accept" . "application/xhtml+xml"))
                         :test #'equal)))))
     (ok (signals (foo.lisp.vinland/web:negotiate
                    ("text/html"
                     "bar")
                    ("application/json"
                     "foo"))
                  'foo.lisp.vinland/web:client-error))))

  (testing
   "runs the fallback clause when provided instead of signalling CLIENT-ERROR"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request
            `(:request-uri "/"
              :request-method :GET
              :headers ,(alexandria:alist-hash-table
                         '(("accept" . "application/xhtml+xml"))
                         :test #'equal)))))
     (ok (equal "baaz" (foo.lisp.vinland/web:negotiate
                         ("text/html"
                          "bar")
                         ("application/json"
                          "foo")
                         (t
                          "baaz")))))))

(deftest set-response-status
    (testing
     "sets the HTTP response status code on the Lack response when provided a response code integer"
     (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                        `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal))))
           (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:set-response-status 204)))
       (ok (= 204 (lack/response:response-status foo.lisp.vinland:*response*)))
       ))

  (testing
   "sets the HTTP response status code on the Lack response when provided a response code keyword"
   (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                      `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:set-response-status :no-content)))
     (ok (= 204 (lack/response:response-status foo.lisp.vinland:*response*)))))

  (testing
   "signals REDIRECT-NOT-ALLOWED-ERROR when given a redirect (3xx) response status code"
   (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                      `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:set-response-status 302)
                  'foo.lisp.vinland/web:redirect-not-allowed-error))
     (ok (signals (foo.lisp.vinland/web:set-response-status :see-other)
                  'foo.lisp.vinland/web:redirect-not-allowed-error)))))

(deftest set-response-headers
    (testing
     "sets response headers on the Lack response"
     (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                        `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal))))
           (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:set-response-headers
                  :content-type "text/plain"
                  :x-foo "bar")))
       (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
         (ok (equal "text/plain" (getf response-headers :content-type)))
         (ok (equal "bar" (getf response-headers :x-foo)))))))

(deftest binding
    (testing
     "returns the value of the binding for the current path"
     (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                        `(:request-uri "/widgets/123"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal))))
           (foo.lisp.vinland:*binding* '((:|widget-id| . "123"))))
       (ok (string= "123" (foo.lisp.vinland/web:binding :|widget-id|)))))

  (testing
   "signals INVALID-BINDING-ERROR when the binding does not exist for the route"
   (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                      `(:request-uri "/widgets/123"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*binding* '((:|widget-id| . "123"))))
     (ok (signals (foo.lisp.vinland/web:binding :|foo|)
                  'foo.lisp.vinland/web:invalid-binding-error)))))

(deftest cookie
    (testing
     "returns the cookie string associated with the given name from the request, returning NIL when not found"
     (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                        `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(alexandria:alist-hash-table
                                                     '(("cookie" . "_sid=xyz;_foo=123"))
                                                     :test #'equal)))))
       (ok (equal "123" (foo.lisp.vinland/web:cookie "_foo")))
       (ok (null (foo.lisp.vinland/web:cookie "_bogus"))))))

(deftest set-cookies
    (testing
     "given an association list, sets the set-cookies field of the lack response"
     (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:set-cookies `(("_sid" . (:value "xyz"
                                                                :domain "localhost"
                                                                :path "/"
                                                                :samesite :lax
                                                                :secure nil
                                                                :httponly t))
                                                     ("foo" . (:value "abc"
                                                               :path "/foo"
                                                               :domain "localhost"
                                                               :samesite :lax
                                                               :secure nil
                                                               :httponly nil))))))
       (ok (equal `("foo" (:value "abc"
                           :path "/foo"
                           :domain "localhost"
                           :samesite :lax
                           :secure nil
                           :httponly nil)
                    "_sid" (:value "xyz"
                            :domain "localhost"
                            :path "/"
                            :samesite :lax
                            :secure nil
                            :httponly t))
                  (lack/response:response-set-cookies foo.lisp.vinland:*response*))))))

(deftest delete-cookie
  (testing
   "sets a cookie with an expiration at UNIX epoch to instruct the user-agent to remove the cookie"
   (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:delete-cookie "_foo")))
     (ok (equal `("_foo" (:value ""
                          :path "/"
                          :expires 2208988800))
                (lack/response:response-set-cookies foo.lisp.vinland:*response*))))))

(deftest set-session-options
    (testing
     "sets Lack session options, merging in values from the plist"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal)
                                          :lack.session.options (:id "xyz"
                                                                 :new-session nil
                                                                 :change-id nil
                                                                 :expire t)))))
       (ok (null (foo.lisp.vinland/web:set-session-options '(:new-session t :expire nil :change-id t))))
       (ok (equal (getf (lack/request:request-env foo.lisp.vinland:*request*)
                        :lack.session.options)
                  '(:id "xyz"
                    :new-session t
                    :change-id t
                    :expire nil))))))

(deftest session
    (testing
     "looks up and returns from the Lack session hash-table the data associated with the given key"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal)
                                          :lack.session ,(alexandria:alist-hash-table
                                                          '(("foo" . "bar")
                                                            ("baaz" . nil))
                                                          :test #'equal)))))
       (multiple-value-bind (result foundp)
           (foo.lisp.vinland/web:session "foo")
         (ok (equal "bar" result))
         (ok (eq t foundp)))
       (multiple-value-bind (result foundp)
           (foo.lisp.vinland/web:session "baaz")
         (ok (null result))
         (ok (eq t foundp)))
       (multiple-value-bind (result foundp)
           (foo.lisp.vinland/web:session "bogus")
         (ok (null result))
         (ok (null foundp))))))

(deftest set-session
    (testing
     "stores an entry in the Lack session hash-table"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal)
                                          :lack.session ,(alexandria:alist-hash-table
                                                          '()
                                                          :test #'equal)))))
       (ok (equal "bar" (foo.lisp.vinland/web:set-session "foo" "bar")))
       (ok (= 11 (foo.lisp.vinland/web:set-session :foo 11)))
       (let ((session (getf (lack/request:request-env foo.lisp.vinland:*request*)
                            :lack.session)))
         (ok (= 2 (hash-table-count session)))
         (ok (string= "bar" (gethash "foo" session)))
         (ok (= 11 (gethash :foo session)))))))

(deftest delete-session
    (testing
     "deletes an entry from the Lack session hash-table"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal)
                                          :lack.session ,(alexandria:alist-hash-table
                                                          '(("foo" . "bar"))
                                                          :test #'equal)))))
       (ok (eq t (foo.lisp.vinland/web:delete-session "foo")))
       (ok (null (foo.lisp.vinland/web:delete-session "bogus")))
       (let ((session (getf (lack/request:request-env foo.lisp.vinland:*request*)
                            :lack.session)))
         (ok (zerop (hash-table-count session)))))))

(deftest clear-session
    (testing
     "clears the Lack session hash-table"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal)
                                          :lack.session ,(alexandria:alist-hash-table
                                                          '(("foo" . "bar")
                                                            ("baaz" . "quux"))
                                                          :test #'equal)))))
       (let ((session (getf (lack/request:request-env foo.lisp.vinland:*request*)
                            :lack.session)))
         (assert (= 2 (hash-table-count session)))
         (ok (eq session (foo.lisp.vinland/web:clear-session)))
         (ok (zerop (hash-table-count session)))))))

(deftest halt
    (testing
     "throws HALT with the optional result"
     (let ((catch-result (catch 'foo.lisp.vinland/web:halt
                           (foo.lisp.vinland/web:halt))))
       (ok (null catch-result)))
     (let ((catch-result (catch 'foo.lisp.vinland/web:halt
                           (foo.lisp.vinland/web:halt '(204 ())))))
       (ok (equal catch-result '(204 ()))))))

(deftest client-error
    (testing
     "signals CLIENT-ERROR with a keyword or integer status-code"
     (ok (signals (foo.lisp.vinland/web:client-error 400)
                  'foo.lisp.vinland/web:client-error))
     (ok (signals (foo.lisp.vinland/web:client-error :bad-request)
                  'foo.lisp.vinland/web:client-error))))

(deftest server-error
    (testing
     "signals SERVER-ERROR with a keyword or integer status-code"
     (ok (signals (foo.lisp.vinland/web:server-error 500)
                  'foo.lisp.vinland/web:server-error))
     (ok (signals (foo.lisp.vinland/web:server-error :internal-server-error)
                  'foo.lisp.vinland/web:server-error))))

(deftest html-safe
    (testing
     "returns an HTML-SAFE instance containing the given HTML string"
     (let ((result (foo.lisp.vinland/web:html-safe "<p>Hello, World.</p>")))
       (ok (eq (type-of result)
               'foo.lisp.vinland/web:html-safe))
       (ok (equal "<p>Hello, World.</p>"
                  (foo.lisp.vinland/web:html-safe-value result))))))

(deftest render
    (testing
     "sets the response status and headers and calls VIEW with ARGS to set the response body"
     (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:render :status :im-a-teapot
                                              :headers '(:content-type "text/plain"
                                                         :x-foo "bar")
                                              :view (lambda (&key x)
                                                      (format nil "I'm a teapot ~A" x))
                                              :args '(:x ":)"))))
       (ok (= 418 (lack/response:response-status foo.lisp.vinland:*response*)))
       (ok (equal '(:content-type "text/plain"
                    :x-foo "bar")
                  (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "I'm a teapot :)" (lack/response:response-body foo.lisp.vinland:*response*)))))

  (testing
   "signals REDIRECT-NOT-ALLOWED-ERROR when called with a 3xx redirection status"
   (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:render :status 302
                                               :headers '(:content-type "text/plain"
                                                          :x-foo "bar")
                                               :view (lambda ()
                                                       "Redirecting...")
                                               :args ())
                  'foo.lisp.vinland/web:redirect-not-allowed-error))
     (ok (signals (foo.lisp.vinland/web:render :status :see-other
                                               :headers '(:content-type "text/plain"
                                                          :x-foo "bar")
                                               :view (lambda ()
                                                       "Redirecting..."))
                  'foo.lisp.vinland/web:redirect-not-allowed-error)))))

(deftest respond
    (testing
     "sets the response status and headers and calls the RENDER keyword argument to set the response body"
     (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:respond :status :im-a-teapot
                                               :headers '(:content-type "text/plain"
                                                          :x-foo "bar")
                                               :render (lambda ()
                                                         "I'm a teapot"))))
       (ok (= 418 (lack/response:response-status foo.lisp.vinland:*response*)))
       (ok (equal '(:content-type "text/plain"
                    :x-foo "bar")
                  (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "I'm a teapot" (lack/response:response-body foo.lisp.vinland:*response*)))))

  (testing
   "signals REDIRECT-NOT-ALLOWED-ERROR when called with a 3xx redirection status"
   (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:respond :status 302
                                                :headers '(:content-type "text/plain"
                                                           :x-foo "bar")
                                                :render (lambda ()
                                                          "Redirecting..."))
                  'foo.lisp.vinland/web:redirect-not-allowed-error))
     (ok (signals (foo.lisp.vinland/web:respond :status :see-other
                                                :headers '(:content-type "text/plain"
                                                           :x-foo "bar")
                                                :render (lambda ()
                                                          "Redirecting..."))
                  'foo.lisp.vinland/web:redirect-not-allowed-error))))

  (testing
   "signals DOUBLE-RENDER-ERROR when a response body is already set"
   (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:respond :headers '(:content-type "text/plain"
                                                        :x-foo "bar")
                                             :render (lambda ()
                                                       "Hello, World."))))
     (ok (signals (foo.lisp.vinland/web:respond :render (lambda ()
                                                          "Foo"))
                  'foo.lisp.vinland/web:double-render-error)))))

(deftest redirect
    (testing
     "redirects to the given location with the given status integer and headers"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(make-hash-table :test #'equal))))
           (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:redirect "/foo"
                                                :status 303
                                                :headers '(:x-foo "bar"))))
       (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
       (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
         (ok (equal "/foo" (getf response-headers :location)))
         (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "redirects to the given location with the given status keyword and headers"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect "/foo"
                                              :status :see-other
                                              :headers '(:x-foo "bar"))))
     (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
     (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "/foo" (getf response-headers :location)))
       (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "signals DOUBLE-RENDER-ERROR when the response already has a body"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (setf (lack/response:response-body foo.lisp.vinland:*response*)
           "Hello, World.")
     (ok (signals (foo.lisp.vinland/web:redirect "/foo")
                  'foo.lisp.vinland/web:double-render-error))))

  (testing
   "signals TYPE-ERROR when the redirect status integer is not in the 3xx range"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect "/foo"
                                                 :status 201)
                  'type-error))))

  (testing
   "signals TYPE-ERROR when the redirect status keyword is not in the 3xx range"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect "/foo"
                                                 :status :created)
                  'type-error))))

  (testing
   "signals TYPE-ERROR when the redirect location string is NIL"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect nil)
                  'type-error))))

  (testing
   "signals SIMPLE-ERROR when the redirect location string is empty"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect ""
                                                 :status 303
                                                 :headers '(:x-foo "bar"))
                  'simple-error))))

  (testing
   "signals UNSAFE-REDIRECT-ERROR when redirecting to another host while ALLOW-OTHER-HOST is NIL"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :server-name "foo.example.com"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect "http://bar.example.org/")
                  'foo.lisp.vinland/web:unsafe-redirect-error))))

  (testing
   "allows redirecting to another host when ALLOW-OTHER-HOST is T"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :server-name "foo.example.com"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect "http://bar.example.org/"
                                              :allow-other-host t)))
     (ok (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                      :location)
                "http://bar.example.org/")))))

(deftest redirect-back
    (testing
     "redirects to the referrer location when present, with the given status integer and headers"
     (let ((foo.lisp.vinland:*request*
             (lack/request:make-request `(:request-uri "/"
                                          :request-method :GET
                                          :headers ,(alexandria:alist-hash-table
                                                     '(("referer" . "/foo"))
                                                     :test #'equal))))
           (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
       (ok (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                     :status 303
                                                     :headers '(:x-foo "bar"))))
       (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
       (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
         (ok (equal "/foo" (getf response-headers :location)))
         (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "redirects to the referrer location when present, with the given status keyword and headers"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(alexandria:alist-hash-table
                                                   '(("referer" . "/foo"))
                                                   :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                   :status :see-other
                                                   :headers '(:x-foo "bar"))))
     (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
     (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "/foo" (getf response-headers :location)))
       (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "redirects to the default-location when the referrer is not present, with the given status code and headers"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(alexandria:alist-hash-table
                                                   ()
                                                   :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                   :status 303
                                                   :headers '(:x-foo "bar"))))
     (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
     (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "/fallback" (getf response-headers :location)))
       (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "redirects to the default-location when the referrer is not present, with the given status keyword and headers"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(alexandria:alist-hash-table
                                                   ()
                                                   :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                   :status :see-other
                                                   :headers '(:x-foo "bar"))))
     (ok (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
     (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
       (ok (equal "/fallback" (getf response-headers :location)))
       (ok (equal "bar" (getf response-headers :x-foo))))))

  (testing
   "signals DOUBLE-RENDER-ERROR when the response already has a body"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (setf (lack/response:response-body foo.lisp.vinland:*response*)
           "Hello, World.")
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :default-location "/fallback")
                  'foo.lisp.vinland/web:double-render-error))))

  (testing
   "signals TYPE-ERROR when the redirect status integer is not in the 3xx range"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :status 201)
                  'type-error))))

  (testing
   "signals TYPE-ERROR when the redirect status keyword is not in the 3xx range"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :default-location "/foo"
                   :status :created)
                  'type-error))))

  (testing
   "signals TYPE-ERROR when the redirect default-location is NIL"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :default-location nil)
                  'type-error))))

  (testing
   "signals SIMPLE-ERROR when the redirect default-location string is empty"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :default-location "")
                  'simple-error))))

  (testing
   "signals UNSAFE-REDIRECT-ERROR when redirecting to another host while ALLOW-OTHER-HOST is NIL"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :server-name "foo.example.com"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (signals (foo.lisp.vinland/web:redirect-back
                   :default-location "http://bar.example.org/")
                  'foo.lisp.vinland/web:unsafe-redirect-error))))

  (testing
   "allows redirecting to another host when ALLOW-OTHER-HOST is T and target is the default location"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :server-name "foo.example.com"
                                        :request-method :GET
                                        :headers ,(make-hash-table :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect-back
                :default-location "http://bar.example.org/"
                :allow-other-host t)))
     (ok (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                      :location)
                "http://bar.example.org/"))))

  (testing
   "allows redirecting to another host when ALLOW-OTHER-HOST is T and target is the referrer"
   (let ((foo.lisp.vinland:*request*
           (lack/request:make-request `(:request-uri "/"
                                        :server-name "foo.example.com"
                                        :request-method :GET
                                        :headers ,(alexandria:alist-hash-table
                                                   '(("referer" . "http://bar.example.org/"))
                                                   :test #'equal))))
         (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
     (ok (null (foo.lisp.vinland/web:redirect-back
                :default-location "/foo"
                :allow-other-host t)))
     (ok (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                      :location)
                "http://bar.example.org/")))))
