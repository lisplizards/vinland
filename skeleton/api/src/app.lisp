(in-package #:<% @var name %>/app)

(defparameter *app*
  (lack:builder
   :request-id
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
   :user
   (:backtrace
    :result-on-error `(500
                       (:content-type "text/plain"
                        :content-length 21)
                       ("Internal Server Error")))
   <% @var name %>/web:*web*))
