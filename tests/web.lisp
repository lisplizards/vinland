(in-package #:foo.lisp.vinland/tests/web)

(define-test web)

(define-test query-params
    :parent web
    (define-test "returns the parsed query parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
        (let ((foo.lisp.vinland:*request*
                (lack/request:make-request `(:query-string "foo=bar"
                                             :headers ,(make-hash-table :test #'equal)))))
          (true (equal '(("foo" . "bar"))
                     (foo.lisp.vinland/web:query-params))))))

(define-test get-query-param
  :parent web
  (define-test "returns the value of the query parameter with the given name, or NIL when not found"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:query-string "foo=bar"
                                           :headers ,(make-hash-table :test #'equal)))))
        (true (string= "bar" (foo.lisp.vinland/web:get-query-param "foo")))
        (true (null (foo.lisp.vinland/web:get-query-param "baaz"))))))


(define-test collect-query-params
  :parent web
  (define-test "returns a list of values for the given query parameter names"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:query-string "foo=bar&baaz=quux&baar=foobar"
                                           :headers ,(make-hash-table :test #'equal)))))
        (true (equal '("bar" nil "foobar")
                   (foo.lisp.vinland/web:collect-query-params '("foo" "bogus" "baar")))))))

(define-test body-params
  :parent web
  (define-test "returns the parsed form body parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
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
        (true (equal '(("hello" . "world"))
                     (foo.lisp.vinland/web:body-params)))))

  (define-test "returns the parsed JSON body parameters, an association list, from FOO.LISP.VINLAND:*REQUEST*"
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
        (true (equal '(("hello" . "world"))
                     (foo.lisp.vinland/web:body-params))))))


(define-test get-body-param
  :parent web
  (define-test "returns the value of the form body parameter with the given name, or NIL when not found"
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
        (true (string= "world" (foo.lisp.vinland/web:get-body-param "hello")))
        (true (null (foo.lisp.vinland/web:get-body-param "foo")))))

  (define-test "returns the value of the JSON body parameter with the given name, or NIL when not found"
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
        (true (string= "world" (foo.lisp.vinland/web:get-body-param "hello")))
        (true (null (foo.lisp.vinland/web:get-body-param "foo"))))))

(define-test collect-body-params
  :parent web
  (define-test "maps the specified parameter names to their values"
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
        (true (equal '("bar" nil "foobar")
                     (foo.lisp.vinland/web:collect-body-params '("foo" "bogus" "baar")))))))

(define-test collect-nested-body-params
  :parent web
  (define-test "maps the specified parameter paths to their values"
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
          (true (equalp '(11 "baz" nil "quux" 33 nil "bar")
                        result))))))

(define-test negotiate
  :parent web
  (define-test "performs media-type negotiation by returning the body of the matching clause, checked against the Accept header"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "application/json"))
                            :test #'equal)))))
        (true (string= "foo" (foo.lisp.vinland/web:negotiate
                              ("text/html"
                               "bar")
                              ("application/json"
                               "foo"))))))

  (define-test "matches the clause by traversing the Accept header, checking against each parsed value"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "text/vnd.turbo-stream.html, text/html, application/xhtml+xml"))
                            :test #'equal)))))
        (true (string= "quux" (foo.lisp.vinland/web:negotiate
                               ("text/html"
                                "bar")
                               ("text/vnd.turbo-stream.html"
                                "quux")
                               ("application/json"
                                "foo"))))))

  (define-test "when no matches the clause by traversing the Accept header, checking against each parsed value, when the match is not the first value"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "text/vnd.turbo-stream.html, text/html, application/xhtml+xml"))
                            :test #'equal)))))
        (true (string= "bar" (foo.lisp.vinland/web:negotiate
                              ("text/html"
                               "bar")
                              ("application/json"
                               "foo"))))))

  (define-test "returns the body of the matched clause, even when the body is NIL"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "text/vnd.turbo-stream.html, text/html, application/xhtml+xml"))
                            :test #'equal)))))
        (true (null (foo.lisp.vinland/web:negotiate
                     ("text/html")
                     ("application/json"
                      "foo"))))))

  (define-test "signals CLIENT-ERROR when no clause matches and no fallback is provided"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "application/xhtml+xml"))
                            :test #'equal)))))
        (fail (foo.lisp.vinland/web:negotiate
               ("text/html"
                "bar")
               ("application/json"
                "foo"))
              'foo.lisp.vinland/web:client-error)))

  (define-test "runs the fallback clause when provided instead of signalling CLIENT-ERROR"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request
               `(:request-uri "/"
                 :request-method :GET
                 :headers ,(alexandria:alist-hash-table
                            '(("accept" . "application/xhtml+xml"))
                            :test #'equal)))))
        (true (string= "baaz" (foo.lisp.vinland/web:negotiate
                               ("text/html"
                                "bar")
                               ("application/json"
                                "foo")
                               (t
                                "baaz")))))))

(define-test set-response-status
  :parent web
  (define-test "sets the HTTP response status code on the Lack response when provided a response code integer"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:set-response-status 204)))
        (true (= 204 (lack/response:response-status foo.lisp.vinland:*response*)))))

  (define-test "sets the HTTP response status code on the Lack response when provided a response code keyword"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:set-response-status :no-content)))
        (true (= 204 (lack/response:response-status foo.lisp.vinland:*response*)))))

  (define-test "signals REDIRECT-NOT-ALLOWED-ERROR when given a redirect (3xx) response status code"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:set-response-status 302)
              'foo.lisp.vinland/web:redirect-not-allowed-error)
        (fail (foo.lisp.vinland/web:set-response-status :see-other)
              'foo.lisp.vinland/web:redirect-not-allowed-error))))

(define-test set-response-headers
  :parent web
  (define-test "sets response headers on the Lack response"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 '(:content-type "text/html"
                                                                            :x-bar "baaz"))))
        (true (null (foo.lisp.vinland/web:set-response-headers
                     :content-type "text/plain"
                     :x-foo "bar")))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "text/plain" (getf response-headers :content-type)))
          (true (= 1 (count :content-type response-headers)))
          (true (string= "bar" (getf response-headers :x-foo)))
          (true (string= "baaz" (getf response-headers :x-bar)))))))

(define-test append-response-headers
  :parent web
  (define-test "appends response headers on the Lack response"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 '(:content-type "text/html"
                                                                            :x-bar "baaz"))))
        (true (null (foo.lisp.vinland/web:append-response-headers
                     :content-type "text/plain"
                     :x-foo "bar")))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (find "text/html" response-headers :test #'string=))
          (true (find "text/plain" response-headers :test #'string=))
          (true (string= "text/html" (getf response-headers :content-type)))
          (true (= 2 (count :content-type response-headers)))
          (true (string= "bar" (getf response-headers :x-foo)))
          (true (string= "baaz" (getf response-headers :x-bar)))))))

(define-test binding
  :parent web
  (define-test "returns the value of the binding for the current path"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/widgets/123"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*binding* '((:|widget-id| . "123"))))
        (true (string= "123" (foo.lisp.vinland/web:binding :|widget-id|)))))

  (define-test "signals INVALID-BINDING-ERROR when the binding does not exist for the route"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/widgets/123"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*binding* '((:|widget-id| . "123"))))
        (fail (foo.lisp.vinland/web:binding :|foo|)
              'foo.lisp.vinland/web:invalid-binding-error))))

(define-test cookie
  :parent web
  (define-test "returns the cookie string associated with the given name from the request, returning NIL when not found"
      (let ((foo.lisp.vinland:*request* (lack/request:make-request
                                         `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      '(("cookie" . "_sid=xyz;_foo=123"))
                                                      :test #'equal)))))
        (true (string= "123" (foo.lisp.vinland/web:cookie "_foo")))
        (true (null (foo.lisp.vinland/web:cookie "_bogus"))))))

(define-test set-cookies
  :parent web
  (define-test "given an association list, sets the set-cookies field of the lack response"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:set-cookies `(("_sid" . (:value "xyz"
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
        (true (equal `("foo" (:value "abc"
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

(define-test delete-cookie
  :parent web
  (define-test "sets a cookie with an expiration at UNIX epoch to instruct the user-agent to remove the cookie"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:delete-cookie "_foo")))
        (true (equal `("_foo" (:value ""
                               :path "/"
                               :httponly t
                               :secure nil
                               :samesite :strict
                               :expires 2208988800))
                     (lack/response:response-set-cookies foo.lisp.vinland:*response*))))))

(define-test set-session-options
  :parent web
  (define-test "sets Lack session options, merging in values from the plist"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request (list :request-uri "/"
                                               :request-method :GET
                                               :headers (make-hash-table :test #'equal)
                                               :lack.session.options '(:id "xyz"
                                                                       :new-session nil
                                                                       :change-id nil
                                                                       :expire t)))))
        (true (null (foo.lisp.vinland/web:set-session-options '(:new-session t :expire nil :change-id t))))
        (true (equalp (getf (lack/request:request-env foo.lisp.vinland:*request*)
                            :lack.session.options)
                      '(:id "xyz"
                        :new-session t
                        :change-id t
                        :expire nil))))))

(define-test session
  :parent web
  (define-test "looks up and returns from the Lack session hash-table the data associated with the given key"
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
          (true (string= "bar" result))
          (true (eq t foundp)))
        (multiple-value-bind (result foundp)
            (foo.lisp.vinland/web:session "baaz")
          (true (null result))
          (true (eq t foundp)))
        (multiple-value-bind (result foundp)
            (foo.lisp.vinland/web:session "bogus")
          (true (null result))
          (true (null foundp))))))

(define-test set-session
  :parent web
  (define-test "stores an entry in the Lack session hash-table"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal)
                                           :lack.session ,(alexandria:alist-hash-table
                                                           '()
                                                           :test #'equal)))))
        (true (string= "bar" (foo.lisp.vinland/web:set-session "foo" "bar")))
        (true (= 11 (foo.lisp.vinland/web:set-session :foo 11)))
        (let ((session (getf (lack/request:request-env foo.lisp.vinland:*request*)
                             :lack.session)))
          (true (= 2 (hash-table-count session)))
          (true (string= "bar" (gethash "foo" session)))
          (true (= 11 (gethash :foo session)))))))

(define-test delete-session
  :parent web
  (define-test "deletes an entry from the Lack session hash-table"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal)
                                           :lack.session ,(alexandria:alist-hash-table
                                                           '(("foo" . "bar"))
                                                           :test #'equal)))))
        (true (eq t (foo.lisp.vinland/web:delete-session "foo")))
        (true (null (foo.lisp.vinland/web:delete-session "bogus")))
        (let ((session (getf (lack/request:request-env foo.lisp.vinland:*request*)
                             :lack.session)))
          (true (zerop (hash-table-count session)))))))

(define-test clear-session
  :parent web
  (define-test "clears the Lack session hash-table"
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
          (true (eq session (foo.lisp.vinland/web:clear-session)))
          (true (zerop (hash-table-count session)))))))

(define-test halt
  :parent web
  (define-test "throws HALT with the optional result"
      (let ((catch-result (catch 'foo.lisp.vinland/web:halt
                            (foo.lisp.vinland/web:halt))))
        (true (null catch-result)))
    (let ((catch-result (catch 'foo.lisp.vinland/web:halt
                          (foo.lisp.vinland/web:halt '(204 ())))))
      (true (equal catch-result '(204 ()))))))

(define-test client-error
  :parent web
  (define-test "signals CLIENT-ERROR with a keyword or integer status-code"
      (fail (foo.lisp.vinland/web:client-error 400)
            'foo.lisp.vinland/web:client-error)
    (fail (foo.lisp.vinland/web:client-error :bad-request)
          'foo.lisp.vinland/web:client-error)))

(define-test server-error
  :parent web
  (define-test "signals SERVER-ERROR with a keyword or integer status-code"
      (fail (foo.lisp.vinland/web:server-error 500)
            'foo.lisp.vinland/web:server-error)
    (fail (foo.lisp.vinland/web:server-error :internal-server-error)
          'foo.lisp.vinland/web:server-error)))

(define-test html-safe
  :parent web
  (define-test "returns an HTML-SAFE instance containing the given HTML string"
      (let ((result (foo.lisp.vinland/web:html-safe "<p>Hello, World.</p>")))
        (true (eq (type-of result)
                  'foo.lisp.vinland/web:html-safe))
        (true (string= "<p>Hello, World.</p>"
                       (foo.lisp.vinland/web:html-safe-value result))))))

(define-test render
  :parent web
  (define-test "sets the response status and headers and calls VIEW with ARGS to set the response body"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:render :status :im-a-teapot
                                                 :headers '(:content-type "text/plain"
                                                            :x-foo "bar")
                                                 :view (lambda (&key x)
                                                         (format nil "I'm a teapot ~A" x))
                                                 :args '(:x ":)"))))
        (true (= 418 (lack/response:response-status foo.lisp.vinland:*response*)))
        (true (equal '(:content-type "text/plain"
                       :x-foo "bar")
                     (lack/response:response-headers foo.lisp.vinland:*response*)))
        (true (string= "I'm a teapot :)" (lack/response:response-body foo.lisp.vinland:*response*)))))

  (define-test "signals REDIRECT-NOT-ALLOWED-ERROR when called with a 3xx redirection status"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:render :status 302
                                           :headers '(:content-type "text/plain"
                                                      :x-foo "bar")
                                           :view (lambda ()
                                                   "Redirecting...")
                                           :args ())
              'foo.lisp.vinland/web:redirect-not-allowed-error)
        (fail (foo.lisp.vinland/web:render :status :see-other
                                           :headers '(:content-type "text/plain"
                                                      :x-foo "bar")
                                           :view (lambda ()
                                                   "Redirecting..."))
              'foo.lisp.vinland/web:redirect-not-allowed-error))))

(define-test respond
  :parent web
  (define-test "sets the response status and headers and calls the RENDER keyword argument to set the response body"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:respond :status :im-a-teapot
                                                  :headers '(:content-type "text/plain"
                                                             :x-foo "bar")
                                                  :render (lambda ()
                                                            "I'm a teapot"))))
        (true (= 418 (lack/response:response-status foo.lisp.vinland:*response*)))
        (true (equal '(:content-type "text/plain"
                       :x-foo "bar")
                     (lack/response:response-headers foo.lisp.vinland:*response*)))
        (true (string= "I'm a teapot" (lack/response:response-body foo.lisp.vinland:*response*)))))

  (define-test "signals REDIRECT-NOT-ALLOWED-ERROR when called with a 3xx redirection status"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:respond :status 302
                                            :headers '(:content-type "text/plain"
                                                       :x-foo "bar")
                                            :render (lambda ()
                                                      "Redirecting..."))
              'foo.lisp.vinland/web:redirect-not-allowed-error)
        (fail (foo.lisp.vinland/web:respond :status :see-other
                                            :headers '(:content-type "text/plain"
                                                       :x-foo "bar")
                                            :render (lambda ()
                                                      "Redirecting..."))
              'foo.lisp.vinland/web:redirect-not-allowed-error)))

  (define-test "signals DOUBLE-RENDER-ERROR when a response body is already set"
      (let ((foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:respond :headers '(:content-type "text/plain"
                                                             :x-foo "bar")
                                                  :render (lambda ()
                                                            "Hello, World."))))
        (fail (foo.lisp.vinland/web:respond :render (lambda ()
                                                      "Foo"))
              'foo.lisp.vinland/web:double-render-error))))

(define-test redirect
  :parent web
  (define-test "redirects to the given location with the given status integer and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect "/foo"
                                                   :status 303
                                                   :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/foo" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "redirects to the given location with the given status keyword and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect "/foo"
                                                   :status :see-other
                                                   :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/foo" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "signals DOUBLE-RENDER-ERROR when the response already has a body"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (setf (lack/response:response-body foo.lisp.vinland:*response*)
              "Hello, World.")
        (fail (foo.lisp.vinland/web:redirect "/foo")
              'foo.lisp.vinland/web:double-render-error)))

  (define-test "signals TYPE-ERROR when the redirect status integer is not in the 3xx range"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect "/foo"
                                             :status 201)
              'type-error)))

  (define-test "signals TYPE-ERROR when the redirect status keyword is not in the 3xx range"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect "/foo"
                                             :status :created)
              'type-error)))

  (define-test "signals SIMPLE-ERROR when the redirect location string is empty"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect ""
                                             :status 303
                                             :headers '(:x-foo "bar"))
              'simple-error)))

  (define-test "signals UNSAFE-REDIRECT-ERROR when redirecting to another host while ALLOW-OTHER-HOST is NIL"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :server-name "foo.example.com"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect "http://bar.example.org/")
              'foo.lisp.vinland/web:unsafe-redirect-error)))

  (define-test "allows redirecting to another host when ALLOW-OTHER-HOST is T"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :server-name "foo.example.com"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect "http://bar.example.org/"
                                                   :allow-other-host t)))
        (true (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                           :location)
                     "http://bar.example.org/")))))

(define-test redirect-back
  :parent web
  (define-test "redirects to the referrer location when present, with the given status integer and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      '(("referer" . "/foo"))
                                                      :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                        :status 303
                                                        :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/foo" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "redirects to the referrer location when present, with the given status keyword and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      '(("referer" . "/foo"))
                                                      :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                        :status :see-other
                                                        :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/foo" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "redirects to the default-location when the referrer is not present, with the given status code and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      ()
                                                      :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                        :status 303
                                                        :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/fallback" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "redirects to the default-location when the referrer is not present, with the given status keyword and headers"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      ()
                                                      :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back :default-location "/fallback"
                                                        :status :see-other
                                                        :headers '(:x-foo "bar"))))
        (true (= 303 (lack/response:response-status foo.lisp.vinland:*response*)))
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (true (string= "/fallback" (getf response-headers :location)))
          (true (string= "bar" (getf response-headers :x-foo))))))

  (define-test "signals DOUBLE-RENDER-ERROR when the response already has a body"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (setf (lack/response:response-body foo.lisp.vinland:*response*)
              "Hello, World.")
        (fail (foo.lisp.vinland/web:redirect-back
               :default-location "/fallback")
              'foo.lisp.vinland/web:double-render-error)))

  (define-test "signals TYPE-ERROR when the redirect status integer is not in the 3xx range"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect-back
               :status 201)
              'type-error)))

  (define-test "signals TYPE-ERROR when the redirect status keyword is not in the 3xx range"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect-back
               :default-location "/foo"
               :status :created)
              'type-error)))

  (define-test "signals TYPE-ERROR when the redirect default-location is NIL"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect-back
               :default-location nil)
              'type-error)))

  (define-test "signals SIMPLE-ERROR when the redirect default-location string is empty"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect-back
               :default-location "")
              'simple-error)))

  (define-test "signals UNSAFE-REDIRECT-ERROR when redirecting to another host while ALLOW-OTHER-HOST is NIL"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :server-name "foo.example.com"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (fail (foo.lisp.vinland/web:redirect-back
               :default-location "http://bar.example.org/")
              'foo.lisp.vinland/web:unsafe-redirect-error)))

  (define-test "allows redirecting to another host when ALLOW-OTHER-HOST is T and target is the default location"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :server-name "foo.example.com"
                                           :request-method :GET
                                           :headers ,(make-hash-table :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back
                     :default-location "http://bar.example.org/"
                     :allow-other-host t)))
        (true (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                           :location)
                     "http://bar.example.org/"))))

  (define-test "allows redirecting to another host when ALLOW-OTHER-HOST is T and target is the referrer"
      (let ((foo.lisp.vinland:*request*
              (lack/request:make-request `(:request-uri "/"
                                           :server-name "foo.example.com"
                                           :request-method :GET
                                           :headers ,(alexandria:alist-hash-table
                                                      '(("referer" . "http://bar.example.org/"))
                                                      :test #'equal))))
            (foo.lisp.vinland:*response* (lack/response:make-response 200 ())))
        (true (null (foo.lisp.vinland/web:redirect-back
                     :default-location "/foo"
                     :allow-other-host t)))
        (true (equal (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                           :location)
                     "http://bar.example.org/")))))

(defun widgets (env)
  (declare (ignore env))
  (let ((foo.lisp.vinland:*origin* "http://acme.example.com"))
    `(200
      (:x-route-url ,(foo.lisp.vinland/web:route-url 'widgets)
       :x-route-path ,(foo.lisp.vinland/web:route-path 'widgets)
       :content-type "text/plain")
      ("Widgets"))))

(defun widget (env)
  (declare (ignore env))
  (let ((foo.lisp.vinland:*origin* "http://acme.example.com"))
    `(200
      (:x-route-url ,(foo.lisp.vinland/web:route-url
                      'widget
                      :|widget-id| "abc")
       :x-route-path ,(foo.lisp.vinland/web:route-path
                       'widget
                       :|widget-id| "abc")
       :content-type "text/plain")
      ("Widget"))))

(define-test route-url-and-route-path-with-no-kwargs
  :parent web
  (define-test "calls the path generator for the route, prefixing with the origin for ROUTE-URL"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/widgets" ,'widgets))))
             (web (funcall router :clack)))
        (let ((response (funcall web `(:path-info "/widgets"
                                       :method :GET
                                       :accept "text/plain"
                                       :headers ,(alexandria:alist-hash-table
                                                  ()
                                                  :test #'equal)))))
          (let ((response-code (first response))
                (x-route-url (getf (second response) :x-route-url))
                (x-route-path (getf (second response) :x-route-path)))
            (true (= 200 response-code))
            (true (string= "http://acme.example.com/widgets" x-route-url))
            (true (string= "/widgets" x-route-path)))))))

(define-test route-url-and-route-path-with-kwargs
  :parent web
  (define-test "calls the path generator for the route, prefixing with the origin for ROUTE-URL"
      (let* ((router (foo.lisp.raven:compile-router
                      `(("/widgets/:widget-id" ,'widget))))
             (web (funcall router :clack)))
        (let ((response (funcall web `(:path-info "/widgets/xyz"
                                       :method :GET
                                       :accept "text/plain"
                                       :headers ,(alexandria:alist-hash-table
                                                  ()
                                                  :test #'equal)))))
          (let ((response-code (first response))
                (x-route-url (getf (second response) :x-route-url))
                (x-route-path (getf (second response) :x-route-path)))
            (true (= 200 response-code))
            (true (string= "http://acme.example.com/widgets/abc" x-route-url))
            (true (string= "/widgets/abc" x-route-path)))))))
