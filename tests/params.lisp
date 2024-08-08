(in-package #:foo.lisp.vinland/tests/params)

(defmethod foo.lisp.vinland/params:%validate-params ((route-name (eql 'test-route))
                                                     (request-method (eql :POST))
                                                     params)
  (unless (cdr (assoc "foo" params :test #'string=))
    (error "Invalid parameters"))
  :ok)

(define-test validate-params)

(define-test "Calls %VALIDATE-PARAMS with the current route and request-method"
  :parent validate-params
  (let ((foo.lisp.vinland:*route* 'test-route)
        (foo.lisp.vinland:*request* (foo.lisp.lack/request:make-request '(:request-method :POST))))
    (true (eq :ok (foo.lisp.vinland/params:validate-params '(("foo" . "bar")))))
    (fail (foo.lisp.vinland/params:validate-params '(("baaz" . "quux")))
          'simple-error)))
