(in-package #:<% @var name %>/app)

(defparameter *app*
  (lack:builder
   :request-id
   (:redact :parameters '("username" "password" "token" "code")
            :preserve-cookies '("_sid"))
   (:http-methods :methods '(:HEAD :GET :POST :PUT :PATCH :DELETE :OPTIONS))
   :head
   (lambda (app)
     (declare (type function app))
     (lambda (env)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type list env))
       (if (eq :OPTIONS (getf env :request-method))
           (let ((response (funcall app env)))
             (declare (type list response))
             (unless (getf (second response) :cache-control)
               (rplacd (last (second response)) '(:cache-control "max-age=3600")))
             response)
           (funcall app env))))
   ;; (:redis :pools '((:pool-id :cache
   ;;                   :host "localhost"
   ;;                   :max-open-count 10
   ;;                   :max-idle-count 4)))
   :user
   (:errors :app (foo.lisp.vinland/errors-app/simple/basic:make-app
                  :root <% @var name %>/config:*static-errors-directory*
                  :required-static-response-codes ()
                  :required-handler-response-codes ()
                  :static-file-types <% @var name %>/http-error:*static-file-types*
                  :media-type-fallback "application/json"
                  :handlers <% @var name %>/http-error:*http-errors*)
            :intercept (lambda (condition)
                         (declare (type error condition))
                         (typecase condition
                           (foo.lisp.http-response:http-error (slot-value
                                                               condition
                                                               'foo.lisp.http-response:status-code))
                           (foo.lisp.raven:no-route-error 404))))
   (:debug :special-variables '(foo.lisp.vinland:*route*
                                foo.lisp.vinland:*binding*))
   <% @var name %>/web:*web*))
