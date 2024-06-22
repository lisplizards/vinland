(in-package #:<% @var name %>/app)

(defparameter *app*
  (lack:builder
   (:static :path (lambda (path)
                    (if (ppcre:scan "^(?:/images/|/css/|/js/|.png$|/robot\\.txt$|/favicon\\.ico$)" path)
                        path
                        nil))
            :root <% @var name %>/config:*static-directory*)
   :request-id
   (:redact :parameters '("username" "password" "token" "code")
            :preserve-cookies '("_sid"))
   (:http-methods :methods '(:HEAD :GET :POST :PUT :PATCH :DELETE :OPTIONS))
   :head
   (:security-headers
    :x-frame-options "DENY"
    :x-xss-protection "0"
    :x-content-type-options "nosniff"
    :x-permitted-cross-domain-policies "none"
    :referrer-policy "strict-origin-when-cross-origin"
    :content-security-policy "default-src 'self' data: https://cdn.jsdelivr.net/; script-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; img-src 'self' https://cdn.jsdelivr.net/; style-src 'self' https://cdn.jsdelivr.net/ 'unsafe-inline'; font-src 'self' https://cdn.jsdelivr.net/")
   (:charset :default '(("text/html" . "utf-8")))
   (lambda (app)
     (declare (type function app))
     (lambda (env)
       (declare (optimize (speed 3) (safety 0) (debug 0))
                (type list env))
       (if (eq :OPTIONS (getf env :request-method))
           (let ((response (funcall app env)))
             (declare (type list response))
             (unless (getf (second response) :cache-control)
               (setf (second response)
                     (nconc (second response) '(:cache-control "max-age=3600"))))
             response)
           (funcall app env))))
   ;; (:redis :pools '((:pool-id :cache
   ;;                   :host "localhost"
   ;;                   :max-open-count 10
   ;;                   :max-idle-count 4)))
   <%- @if session-store-redis-pool %>
   (:session :store (lack/middleware/session/store/redis-pool:make-redis-store
                     :host "localhost"
                     :max-open-count 10
                     :max-idle-count 4)
             :state (lack/session/state/cookie:make-cookie-state
                     :cookie-key "_sid"
                     :path "/"
                     :domain "localhost"
                     :expires 3600
                     :httponly t
                     :secure nil
                     :samesite :strict))
   <%- @else %>
   (:session :store (lack/middleware/session/store/memory:make-memory-store)
             :state (lack/session/state/cookie:make-cookie-state
                     :cookie-key "_sid"
                     :path "/"
                     :domain "localhost"
                     :expires 3600
                     :httponly t
                     :secure nil
                     :samesite :strict))
   <%- @endif %>
   :csrf
   :flash
   :user
   (:errors :app (foo.lisp.vinland/errors-app/simple/basic:make-app
                  :root <% @var name %>/config:*static-errors-directory*
                  :required-static-response-codes ()
                  :required-handler-response-codes ()
                  :static-file-types <% @var name %>/http-error:*static-file-types*
                  :handlers <% @var name %>/http-error:*http-errors*)
            :intercept (lambda (condition)
                         (declare (type error condition))
                         (typecase condition
                           (foo.lisp.http-response:http-error (slot-value
                                                               condition
                                                               'foo.lisp.http-response:status-code))
                           (foo.lisp.raven:no-route-error 404)
                           (lack/middleware/session/store/redis-pool:redis-pool-timeout-error 503))))
   (:debug :special-variables '(foo.lisp.vinland:*route*
                                foo.lisp.vinland:*binding*))
   <% @var name %>/web:*web*))
