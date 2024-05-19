(in-package #:foo.lisp.vinland/tests/response)

(deftest status-code-to-keyword
    (testing
     "Translates the given HTTP response status code (integer) to a keyword"
     (ok (eq :continue
             (foo.lisp.vinland/response:status-code-to-keyword 100)))
     (ok (eq :switching-protocols
             (foo.lisp.vinland/response:status-code-to-keyword 101)))
     (ok (eq :processing
             (foo.lisp.vinland/response:status-code-to-keyword 102)))
     (ok (eq :early-hints
             (foo.lisp.vinland/response:status-code-to-keyword 103)))
     (ok (eq :ok
             (foo.lisp.vinland/response:status-code-to-keyword 200)))
     (ok (eq :created
             (foo.lisp.vinland/response:status-code-to-keyword 201)))
     (ok (eq :accepted
             (foo.lisp.vinland/response:status-code-to-keyword 202)))
     (ok (eq :non-authoritative-information
             (foo.lisp.vinland/response:status-code-to-keyword 203)))
     (ok (eq :no-content
             (foo.lisp.vinland/response:status-code-to-keyword 204)))
     (ok (eq :reset-content
             (foo.lisp.vinland/response:status-code-to-keyword 205)))
     (ok (eq :partial-content
             (foo.lisp.vinland/response:status-code-to-keyword 206)))
     (ok (eq :multi-status
             (foo.lisp.vinland/response:status-code-to-keyword 207)))
     (ok (eq :already-reported
             (foo.lisp.vinland/response:status-code-to-keyword 208)))
     (ok (eq :im-used
             (foo.lisp.vinland/response:status-code-to-keyword 226)))
     (ok (eq :multiple-choices
             (foo.lisp.vinland/response:status-code-to-keyword 300)))
     (ok (eq :moved-permanently
             (foo.lisp.vinland/response:status-code-to-keyword 301)))
     (ok (eq :found
             (foo.lisp.vinland/response:status-code-to-keyword 302)))
     (ok (eq :see-other
             (foo.lisp.vinland/response:status-code-to-keyword 303)))
     (ok (eq :not-modified
             (foo.lisp.vinland/response:status-code-to-keyword 304)))
     (ok (eq :temporary-redirect
             (foo.lisp.vinland/response:status-code-to-keyword 307)))
     (ok (eq :permanent-redirect
             (foo.lisp.vinland/response:status-code-to-keyword 308)))
     (ok (eq :bad-request
             (foo.lisp.vinland/response:status-code-to-keyword 400)))
     (ok (eq :unauthorized
             (foo.lisp.vinland/response:status-code-to-keyword 401)))
     (ok (eq :payment-required
             (foo.lisp.vinland/response:status-code-to-keyword 402)))
     (ok (eq :forbidden
             (foo.lisp.vinland/response:status-code-to-keyword 403)))
     (ok (eq :not-found
             (foo.lisp.vinland/response:status-code-to-keyword 404)))
     (ok (eq :method-not-allowed
             (foo.lisp.vinland/response:status-code-to-keyword 405)))
     (ok (eq :not-acceptable
             (foo.lisp.vinland/response:status-code-to-keyword 406)))
     (ok (eq :proxy-authentication-required
             (foo.lisp.vinland/response:status-code-to-keyword 407)))
     (ok (eq :request-timeout
             (foo.lisp.vinland/response:status-code-to-keyword 408)))
     (ok (eq :conflict
             (foo.lisp.vinland/response:status-code-to-keyword 409)))
     (ok (eq :gone
             (foo.lisp.vinland/response:status-code-to-keyword 410)))
     (ok (eq :length-required
             (foo.lisp.vinland/response:status-code-to-keyword 411)))
     (ok (eq :precondition-failed
             (foo.lisp.vinland/response:status-code-to-keyword 412)))
     (ok (eq :payload-too-large
             (foo.lisp.vinland/response:status-code-to-keyword 413)))
     (ok (eq :uri-too-long
             (foo.lisp.vinland/response:status-code-to-keyword 414)))
     (ok (eq :unsupported-media-type
             (foo.lisp.vinland/response:status-code-to-keyword 415)))
     (ok (eq :range-not-satisfiable
             (foo.lisp.vinland/response:status-code-to-keyword 416)))
     (ok (eq :expectation-failed
             (foo.lisp.vinland/response:status-code-to-keyword 417)))
     (ok (eq :im-a-teapot
             (foo.lisp.vinland/response:status-code-to-keyword 418)))
     (ok (eq :misdirected-request
             (foo.lisp.vinland/response:status-code-to-keyword 421)))
     (ok (eq :unprocessable-content
             (foo.lisp.vinland/response:status-code-to-keyword 422)))
     (ok (eq :locked
             (foo.lisp.vinland/response:status-code-to-keyword 423)))
     (ok (eq :failed-dependency
             (foo.lisp.vinland/response:status-code-to-keyword 424)))
     (ok (eq :too-early
             (foo.lisp.vinland/response:status-code-to-keyword 425)))
     (ok (eq :upgrade-required
             (foo.lisp.vinland/response:status-code-to-keyword 426)))
     (ok (eq :precondition-required
             (foo.lisp.vinland/response:status-code-to-keyword 428)))
     (ok (eq :too-many-requests
             (foo.lisp.vinland/response:status-code-to-keyword 429)))
     (ok (eq :request-header-fields-too-large
             (foo.lisp.vinland/response:status-code-to-keyword 431)))
     (ok (eq :unavailable-for-legal-reasons
             (foo.lisp.vinland/response:status-code-to-keyword 451)))
     (ok (eq :internal-server-error
             (foo.lisp.vinland/response:status-code-to-keyword 500)))
     (ok (eq :not-implemented
             (foo.lisp.vinland/response:status-code-to-keyword 501)))
     (ok (eq :bad-gateway
             (foo.lisp.vinland/response:status-code-to-keyword 502)))
     (ok (eq :service-unavailable
             (foo.lisp.vinland/response:status-code-to-keyword 503)))
     (ok (eq :gateway-timeout
             (foo.lisp.vinland/response:status-code-to-keyword 504)))
     (ok (eq :http-version-not-supported
             (foo.lisp.vinland/response:status-code-to-keyword 505)))
     (ok (eq :variant-also-negotiates
             (foo.lisp.vinland/response:status-code-to-keyword 506)))
     (ok (eq :insufficient-storage
             (foo.lisp.vinland/response:status-code-to-keyword 507)))
     (ok (eq :loop-detected
             (foo.lisp.vinland/response:status-code-to-keyword 508)))
     (ok (eq :not-extended
             (foo.lisp.vinland/response:status-code-to-keyword 510)))
     (ok (eq :network-authentication-required
             (foo.lisp.vinland/response:status-code-to-keyword 511))))

  (testing
   "signals UNKNOWN-STATUS-ERROR when given an unknown status code"
   (ok (signals (foo.lisp.vinland/response:status-code-to-keyword 499)
                'foo.lisp.vinland/response:unknown-status-error))))

(deftest status-code-to-text
    (testing
     "Translates the given HTTP response status code (integer) to text"
     (ok (string= "Continue"
                  (foo.lisp.vinland/response:status-code-to-text 100)))
     (ok (string= "Switching Protocols"
                  (foo.lisp.vinland/response:status-code-to-text 101)))
     (ok (string= "Processing"
                  (foo.lisp.vinland/response:status-code-to-text 102)))
     (ok (string= "Early Hints"
                  (foo.lisp.vinland/response:status-code-to-text 103)))
     (ok (string= "OK"
                  (foo.lisp.vinland/response:status-code-to-text 200)))
     (ok (string= "Created"
                  (foo.lisp.vinland/response:status-code-to-text 201)))
     (ok (string= "Accepted"
                  (foo.lisp.vinland/response:status-code-to-text 202)))
     (ok (string= "Non-Authoritative Information"
                  (foo.lisp.vinland/response:status-code-to-text 203)))
     (ok (string= "No Content"
                  (foo.lisp.vinland/response:status-code-to-text 204)))
     (ok (string= "Reset Content"
                  (foo.lisp.vinland/response:status-code-to-text 205)))
     (ok (string= "Partial Content"
                  (foo.lisp.vinland/response:status-code-to-text 206)))
     (ok (string= "Multi-Status"
                  (foo.lisp.vinland/response:status-code-to-text 207)))
     (ok (string= "Already Reported"
                  (foo.lisp.vinland/response:status-code-to-text 208)))
     (ok (string= "IM Used"
                  (foo.lisp.vinland/response:status-code-to-text 226)))
     (ok (string= "Multiple Choices"
                  (foo.lisp.vinland/response:status-code-to-text 300)))
     (ok (string= "Moved Permanently"
                  (foo.lisp.vinland/response:status-code-to-text 301)))
     (ok (string= "Found"
                  (foo.lisp.vinland/response:status-code-to-text 302)))
     (ok (string= "See Other"
                  (foo.lisp.vinland/response:status-code-to-text 303)))
     (ok (string= "Not Modified"
                  (foo.lisp.vinland/response:status-code-to-text 304)))
     (ok (string= "Temporary Redirect"
                  (foo.lisp.vinland/response:status-code-to-text 307)))
     (ok (string= "Permanent Redirect"
                  (foo.lisp.vinland/response:status-code-to-text 308)))
     (ok (string= "Bad Request"
                  (foo.lisp.vinland/response:status-code-to-text 400)))
     (ok (string= "Unauthorized"
                  (foo.lisp.vinland/response:status-code-to-text 401)))
     (ok (string= "Payment Required"
                  (foo.lisp.vinland/response:status-code-to-text 402)))
     (ok (string= "Forbidden"
                  (foo.lisp.vinland/response:status-code-to-text 403)))
     (ok (string= "Not Found"
                  (foo.lisp.vinland/response:status-code-to-text 404)))
     (ok (string= "Method Not Allowed"
                  (foo.lisp.vinland/response:status-code-to-text 405)))
     (ok (string= "Not Acceptable"
                  (foo.lisp.vinland/response:status-code-to-text 406)))
     (ok (string= "Proxy Authentication Required"
                  (foo.lisp.vinland/response:status-code-to-text 407)))
     (ok (string= "Request Timeout"
                  (foo.lisp.vinland/response:status-code-to-text 408)))
     (ok (string= "Conflict"
                  (foo.lisp.vinland/response:status-code-to-text 409)))
     (ok (string= "Gone"
                  (foo.lisp.vinland/response:status-code-to-text 410)))
     (ok (string= "Length Required"
                  (foo.lisp.vinland/response:status-code-to-text 411)))
     (ok (string= "Precondition Failed"
                  (foo.lisp.vinland/response:status-code-to-text 412)))
     (ok (string= "Payload Too Large"
                  (foo.lisp.vinland/response:status-code-to-text 413)))
     (ok (string= "URI Too Long"
                  (foo.lisp.vinland/response:status-code-to-text 414)))
     (ok (string= "Unsupported Media Type"
                  (foo.lisp.vinland/response:status-code-to-text 415)))
     (ok (string= "Range Not Satisfiable"
                  (foo.lisp.vinland/response:status-code-to-text 416)))
     (ok (string= "Expectation Failed"
                  (foo.lisp.vinland/response:status-code-to-text 417)))
     (ok (string= "I'm a teapot"
                  (foo.lisp.vinland/response:status-code-to-text 418)))
     (ok (string= "Misdirected Request"
                  (foo.lisp.vinland/response:status-code-to-text 421)))
     (ok (string= "Unprocessable Content"
                  (foo.lisp.vinland/response:status-code-to-text 422)))
     (ok (string= "Locked"
                  (foo.lisp.vinland/response:status-code-to-text 423)))
     (ok (string= "Failed Dependency"
                  (foo.lisp.vinland/response:status-code-to-text 424)))
     (ok (string= "Too Early"
                  (foo.lisp.vinland/response:status-code-to-text 425)))
     (ok (string= "Upgrade Required"
                  (foo.lisp.vinland/response:status-code-to-text 426)))
     (ok (string= "Precondition Required"
                  (foo.lisp.vinland/response:status-code-to-text 428)))
     (ok (string= "Too Many Requests"
                  (foo.lisp.vinland/response:status-code-to-text 429)))
     (ok (string= "Request Header Fields Too Large"
                  (foo.lisp.vinland/response:status-code-to-text 431)))
     (ok (string= "Unavailable For Legal Reasons"
                  (foo.lisp.vinland/response:status-code-to-text 451)))
     (ok (string= "Internal Server Error"
                  (foo.lisp.vinland/response:status-code-to-text 500)))
     (ok (string= "Not Implemented"
                  (foo.lisp.vinland/response:status-code-to-text 501)))
     (ok (string= "Bad Gateway"
                  (foo.lisp.vinland/response:status-code-to-text 502)))
     (ok (string= "Service Unavailable"
                  (foo.lisp.vinland/response:status-code-to-text 503)))
     (ok (string= "Gateway Timeout"
                  (foo.lisp.vinland/response:status-code-to-text 504)))
     (ok (string= "HTTP Version Not Supported"
                  (foo.lisp.vinland/response:status-code-to-text 505)))
     (ok (string= "Variant Also Negotiates"
                  (foo.lisp.vinland/response:status-code-to-text 506)))
     (ok (string= "Insufficient Storage"
                  (foo.lisp.vinland/response:status-code-to-text 507)))
     (ok (string= "Loop Detected"
                  (foo.lisp.vinland/response:status-code-to-text 508)))
     (ok (string= "Not Extended"
                  (foo.lisp.vinland/response:status-code-to-text 510)))
     (ok (string= "Network Authentication Required"
                  (foo.lisp.vinland/response:status-code-to-text 511))))

  (testing
   "signals UNKNOWN-STATUS-ERROR when given an unknown status code"
   (ok (signals (foo.lisp.vinland/response:status-code-to-text 499)
                'foo.lisp.vinland/response:unknown-status-error))))

(deftest status-keyword-to-code
    (testing
     "Translates the given HTTP response status keyword to a code (integer)"
     (ok (= 100
            (foo.lisp.vinland/response:status-keyword-to-code :continue)))
     (ok (= 101
            (foo.lisp.vinland/response:status-keyword-to-code :switching-protocols)))
     (ok (= 102
            (foo.lisp.vinland/response:status-keyword-to-code :processing)))
     (ok (= 103
            (foo.lisp.vinland/response:status-keyword-to-code :early-hints)))
     (ok (= 200
            (foo.lisp.vinland/response:status-keyword-to-code :ok)))
     (ok (= 201
            (foo.lisp.vinland/response:status-keyword-to-code :created)))
     (ok (= 202
            (foo.lisp.vinland/response:status-keyword-to-code :accepted)))
     (ok (= 203
            (foo.lisp.vinland/response:status-keyword-to-code :non-authoritative-information)))
     (ok (= 204
            (foo.lisp.vinland/response:status-keyword-to-code :no-content)))
     (ok (= 205
            (foo.lisp.vinland/response:status-keyword-to-code :reset-content)))
     (ok (= 206
            (foo.lisp.vinland/response:status-keyword-to-code :partial-content)))
     (ok (= 207
            (foo.lisp.vinland/response:status-keyword-to-code :multi-status)))
     (ok (= 208
            (foo.lisp.vinland/response:status-keyword-to-code :already-reported)))
     (ok (= 226
            (foo.lisp.vinland/response:status-keyword-to-code :im-used)))
     (ok (= 300
            (foo.lisp.vinland/response:status-keyword-to-code :multiple-choices)))
     (ok (= 301
            (foo.lisp.vinland/response:status-keyword-to-code :moved-permanently)))
     (ok (= 302
            (foo.lisp.vinland/response:status-keyword-to-code :found)))
     (ok (= 303
            (foo.lisp.vinland/response:status-keyword-to-code :see-other)))
     (ok (= 304
            (foo.lisp.vinland/response:status-keyword-to-code :not-modified)))
     (ok (= 307
            (foo.lisp.vinland/response:status-keyword-to-code :temporary-redirect)))
     (ok (= 308
            (foo.lisp.vinland/response:status-keyword-to-code :permanent-redirect)))
     (ok (= 400
            (foo.lisp.vinland/response:status-keyword-to-code :bad-request)))
     (ok (= 401
            (foo.lisp.vinland/response:status-keyword-to-code :unauthorized)))
     (ok (= 402
            (foo.lisp.vinland/response:status-keyword-to-code :payment-required)))
     (ok (= 403
            (foo.lisp.vinland/response:status-keyword-to-code :forbidden)))
     (ok (= 404
            (foo.lisp.vinland/response:status-keyword-to-code :not-found)))
     (ok (= 405
            (foo.lisp.vinland/response:status-keyword-to-code :method-not-allowed)))
     (ok (= 406
            (foo.lisp.vinland/response:status-keyword-to-code :not-acceptable)))
     (ok (= 407
            (foo.lisp.vinland/response:status-keyword-to-code :proxy-authentication-required)))
     (ok (= 408
            (foo.lisp.vinland/response:status-keyword-to-code :request-timeout)))
     (ok (= 409
            (foo.lisp.vinland/response:status-keyword-to-code :conflict)))
     (ok (= 410
            (foo.lisp.vinland/response:status-keyword-to-code :gone)))
     (ok (= 411
            (foo.lisp.vinland/response:status-keyword-to-code :length-required)))
     (ok (= 412
            (foo.lisp.vinland/response:status-keyword-to-code :precondition-failed)))
     (ok (= 413
            (foo.lisp.vinland/response:status-keyword-to-code :payload-too-large)))
     (ok (= 414
            (foo.lisp.vinland/response:status-keyword-to-code :uri-too-long)))
     (ok (= 415
            (foo.lisp.vinland/response:status-keyword-to-code :unsupported-media-type)))
     (ok (= 416
            (foo.lisp.vinland/response:status-keyword-to-code :range-not-satisfiable)))
     (ok (= 417
            (foo.lisp.vinland/response:status-keyword-to-code :expectation-failed)))
     (ok (= 418
            (foo.lisp.vinland/response:status-keyword-to-code :im-a-teapot)))
     (ok (= 421
            (foo.lisp.vinland/response:status-keyword-to-code :misdirected-request)))
     (ok (= 422
            (foo.lisp.vinland/response:status-keyword-to-code :unprocessable-content)))
     (ok (= 423
            (foo.lisp.vinland/response:status-keyword-to-code :locked)))
     (ok (= 424
            (foo.lisp.vinland/response:status-keyword-to-code :failed-dependency)))
     (ok (= 425
            (foo.lisp.vinland/response:status-keyword-to-code :too-early)))
     (ok (= 426
            (foo.lisp.vinland/response:status-keyword-to-code :upgrade-required)))
     (ok (= 428
            (foo.lisp.vinland/response:status-keyword-to-code :precondition-required)))
     (ok (= 429
            (foo.lisp.vinland/response:status-keyword-to-code :too-many-requests)))
     (ok (= 431
            (foo.lisp.vinland/response:status-keyword-to-code :request-header-fields-too-large)))
     (ok (= 451
            (foo.lisp.vinland/response:status-keyword-to-code :unavailable-for-legal-reasons)))
     (ok (= 500
            (foo.lisp.vinland/response:status-keyword-to-code :internal-server-error)))
     (ok (= 501
            (foo.lisp.vinland/response:status-keyword-to-code :not-implemented)))
     (ok (= 502
            (foo.lisp.vinland/response:status-keyword-to-code :bad-gateway)))
     (ok (= 503
            (foo.lisp.vinland/response:status-keyword-to-code :service-unavailable)))
     (ok (= 504
            (foo.lisp.vinland/response:status-keyword-to-code :gateway-timeout)))
     (ok (= 505
            (foo.lisp.vinland/response:status-keyword-to-code :http-version-not-supported)))
     (ok (= 506
            (foo.lisp.vinland/response:status-keyword-to-code :variant-also-negotiates)))
     (ok (= 507
            (foo.lisp.vinland/response:status-keyword-to-code :insufficient-storage)))
     (ok (= 508
            (foo.lisp.vinland/response:status-keyword-to-code :loop-detected)))
     (ok (= 510
            (foo.lisp.vinland/response:status-keyword-to-code :not-extended)))
     (ok (= 511
            (foo.lisp.vinland/response:status-keyword-to-code :network-authentication-required))))

  (testing
   "signals UNKNOWN-STATUS-ERROR when given an unknown status keyword"
   (ok (signals (foo.lisp.vinland/response:status-keyword-to-code :foo)
                'foo.lisp.vinland/response:unknown-status-error))))

(deftest status-keyword-to-text
    (testing
     "Translates the given HTTP response status code (integer) to text"
     (ok (string= "Continue"
                  (foo.lisp.vinland/response:status-keyword-to-text :continue)))
     (ok (string= "Switching Protocols"
                  (foo.lisp.vinland/response:status-keyword-to-text :switching-protocols)))
     (ok (string= "Processing"
                  (foo.lisp.vinland/response:status-keyword-to-text :processing)))
     (ok (string= "Early Hints"
                  (foo.lisp.vinland/response:status-keyword-to-text :early-hints)))
     (ok (string= "OK"
                  (foo.lisp.vinland/response:status-keyword-to-text :ok)))
     (ok (string= "Created"
                  (foo.lisp.vinland/response:status-keyword-to-text :created)))
     (ok (string= "Accepted"
                  (foo.lisp.vinland/response:status-keyword-to-text :accepted)))
     (ok (string= "Non-Authoritative Information"
                  (foo.lisp.vinland/response:status-keyword-to-text :non-authoritative-information)))
     (ok (string= "No Content"
                  (foo.lisp.vinland/response:status-keyword-to-text :no-content)))
     (ok (string= "Reset Content"
                  (foo.lisp.vinland/response:status-keyword-to-text :reset-content)))
     (ok (string= "Partial Content"
                  (foo.lisp.vinland/response:status-keyword-to-text :partial-content)))
     (ok (string= "Multi-Status"
                  (foo.lisp.vinland/response:status-keyword-to-text :multi-status)))
     (ok (string= "Already Reported"
                  (foo.lisp.vinland/response:status-keyword-to-text :already-reported)))
     (ok (string= "IM Used"
                  (foo.lisp.vinland/response:status-keyword-to-text :im-used)))
     (ok (string= "Multiple Choices"
                  (foo.lisp.vinland/response:status-keyword-to-text :multiple-choices)))
     (ok (string= "Moved Permanently"
                  (foo.lisp.vinland/response:status-keyword-to-text :moved-permanently)))
     (ok (string= "Found"
                  (foo.lisp.vinland/response:status-keyword-to-text :found)))
     (ok (string= "See Other"
                  (foo.lisp.vinland/response:status-keyword-to-text :see-other)))
     (ok (string= "Not Modified"
                  (foo.lisp.vinland/response:status-keyword-to-text :not-modified)))
     (ok (string= "Temporary Redirect"
                  (foo.lisp.vinland/response:status-keyword-to-text :temporary-redirect)))
     (ok (string= "Permanent Redirect"
                  (foo.lisp.vinland/response:status-keyword-to-text :permanent-redirect)))
     (ok (string= "Bad Request"
                  (foo.lisp.vinland/response:status-keyword-to-text :bad-request)))
     (ok (string= "Unauthorized"
                  (foo.lisp.vinland/response:status-keyword-to-text :unauthorized)))
     (ok (string= "Payment Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :payment-required)))
     (ok (string= "Forbidden"
                  (foo.lisp.vinland/response:status-keyword-to-text :forbidden)))
     (ok (string= "Not Found"
                  (foo.lisp.vinland/response:status-keyword-to-text :not-found)))
     (ok (string= "Method Not Allowed"
                  (foo.lisp.vinland/response:status-keyword-to-text :method-not-allowed)))
     (ok (string= "Not Acceptable"
                  (foo.lisp.vinland/response:status-keyword-to-text :not-acceptable)))
     (ok (string= "Proxy Authentication Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :proxy-authentication-required)))
     (ok (string= "Request Timeout"
                  (foo.lisp.vinland/response:status-keyword-to-text :request-timeout)))
     (ok (string= "Conflict"
                  (foo.lisp.vinland/response:status-keyword-to-text :conflict)))
     (ok (string= "Gone"
                  (foo.lisp.vinland/response:status-keyword-to-text :gone)))
     (ok (string= "Length Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :length-required)))
     (ok (string= "Precondition Failed"
                  (foo.lisp.vinland/response:status-keyword-to-text :precondition-failed)))
     (ok (string= "Payload Too Large"
                  (foo.lisp.vinland/response:status-keyword-to-text :payload-too-large)))
     (ok (string= "URI Too Long"
                  (foo.lisp.vinland/response:status-keyword-to-text :uri-too-long)))
     (ok (string= "Unsupported Media Type"
                  (foo.lisp.vinland/response:status-keyword-to-text :unsupported-media-type)))
     (ok (string= "Range Not Satisfiable"
                  (foo.lisp.vinland/response:status-keyword-to-text :range-not-satisfiable)))
     (ok (string= "Expectation Failed"
                  (foo.lisp.vinland/response:status-keyword-to-text :expectation-failed)))
     (ok (string= "I'm a teapot"
                  (foo.lisp.vinland/response:status-keyword-to-text :im-a-teapot)))
     (ok (string= "Misdirected Request"
                  (foo.lisp.vinland/response:status-keyword-to-text :misdirected-request)))
     (ok (string= "Unprocessable Content"
                  (foo.lisp.vinland/response:status-keyword-to-text :unprocessable-content)))
     (ok (string= "Locked"
                  (foo.lisp.vinland/response:status-keyword-to-text :locked)))
     (ok (string= "Failed Dependency"
                  (foo.lisp.vinland/response:status-keyword-to-text :failed-dependency)))
     (ok (string= "Too Early"
                  (foo.lisp.vinland/response:status-keyword-to-text :too-early)))
     (ok (string= "Upgrade Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :upgrade-required)))
     (ok (string= "Precondition Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :precondition-required)))
     (ok (string= "Too Many Requests"
                  (foo.lisp.vinland/response:status-keyword-to-text :too-many-requests)))
     (ok (string= "Request Header Fields Too Large"
                  (foo.lisp.vinland/response:status-keyword-to-text :request-header-fields-too-large)))
     (ok (string= "Unavailable For Legal Reasons"
                  (foo.lisp.vinland/response:status-keyword-to-text :unavailable-for-legal-reasons)))
     (ok (string= "Internal Server Error"
                  (foo.lisp.vinland/response:status-keyword-to-text :internal-server-error)))
     (ok (string= "Not Implemented"
                  (foo.lisp.vinland/response:status-keyword-to-text :not-implemented)))
     (ok (string= "Bad Gateway"
                  (foo.lisp.vinland/response:status-keyword-to-text :bad-gateway)))
     (ok (string= "Service Unavailable"
                  (foo.lisp.vinland/response:status-keyword-to-text :service-unavailable)))
     (ok (string= "Gateway Timeout"
                  (foo.lisp.vinland/response:status-keyword-to-text :gateway-timeout)))
     (ok (string= "HTTP Version Not Supported"
                  (foo.lisp.vinland/response:status-keyword-to-text :http-version-not-supported)))
     (ok (string= "Variant Also Negotiates"
                  (foo.lisp.vinland/response:status-keyword-to-text :variant-also-negotiates)))
     (ok (string= "Insufficient Storage"
                  (foo.lisp.vinland/response:status-keyword-to-text :insufficient-storage)))
     (ok (string= "Loop Detected"
                  (foo.lisp.vinland/response:status-keyword-to-text :loop-detected)))
     (ok (string= "Not Extended"
                  (foo.lisp.vinland/response:status-keyword-to-text :not-extended)))
     (ok (string= "Network Authentication Required"
                  (foo.lisp.vinland/response:status-keyword-to-text :network-authentication-required))))

  (testing
   "signals UNKNOWN-STATUS-ERROR when given an unknown status code"
   (ok (signals (foo.lisp.vinland/response:status-keyword-to-text :foo)
                'foo.lisp.vinland/response:unknown-status-error))))
