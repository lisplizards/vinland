;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/handler/simple)

(defstruct route/simple
  (methods () :type keyword-list)
  (accept () :type string-list)
  (provide () :type string-list)
  (before () :type function-list)
  (after () :type function-list)
  (max-uri-length nil :type (or null integer))
  (max-content-length nil :type (or null bignum))
  (get nil :type (or null function))
  (head nil :type (or null function))
  (post nil :type (or null function))
  (put nil :type (or null function))
  (patch nil :type (or null function))
  (delete nil :type (or null function))
  (options nil :type (or null function))
  (trace nil :type (or null function))
  (connect nil :type (or null function)))

(defun define-controller (sym &rest kwargs &key &allow-other-keys)
  "Defines a Raven route handler for the sub-protocol identified by FOO.LISP.VINLAND:ROUTE/SIMPLE."
  (apply #'foo.lisp.raven:define-route sym :meta 'foo.lisp.vinland:route/simple kwargs))

(defmethod foo.lisp.raven:%make-route-metadata ((metadata (eql 'route/simple))
                                                &rest kwargs &key &allow-other-keys)
  (destructuring-bind (&key route-name method provide accept before after
                         max-uri-length max-content-length
                         get head post put patch delete options trace connect
                       &allow-other-keys)
      kwargs
    (check-type route-name symbol)
    (check-type method (or list keyword))
    (check-type provide (or list string))
    (check-type accept (or list string))
    (check-type before (or list function))
    (check-type after (or list function))
    (check-type max-uri-length (or null integer))
    (check-type max-content-length (or null integer))
    (check-type get (or null function))
    (check-type head (or null function))
    (check-type post (or null function))
    (check-type put (or null function))
    (check-type patch (or null function))
    (check-type delete (or null function))
    (check-type options (or null function))
    (check-type trace (or null function))
    (check-type connect (or null function))
    (etypecase method
      (list
       (dolist (method method)
         (check-type method keyword)))
      (keyword
       (setq method (list method))))
    (etypecase provide
      (list
       (dolist (provide-str provide)
         (check-type provide-str string)))
      (string
       (setq provide (list provide))))
    (etypecase accept
      (list
       (dolist (accept-str accept)
         (check-type accept-str string)))
      (string
       (setq accept (list accept))))
    (etypecase before
      (list
       (dolist (before before)
         (check-type before function)))
      (function
       (setq before (list before))))
    (etypecase after
      (list
       (dolist (after after)
         (check-type after function)))
      (function
       (setq after (list after))))
    (when max-uri-length
      (assert (> max-uri-length 0)))
    (when max-content-length
      (assert (> max-content-length -1)))
    (make-route/simple
     :methods method
     :provide provide
     :accept accept
     :before before
     :after after
     :max-uri-length max-uri-length
     :max-content-length max-content-length
     :get get
     :head head
     :post post
     :put put
     :patch patch
     :delete delete
     :options options
     :trace trace
     :connect connect)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmethod foo.lisp.raven:%handle-request ((metadata route/simple) route-name env)
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (type symbol route-name)
             (list env))
    #1=(block nil
         (let ((foo.lisp.vinland:*request* (foo.lisp.lack/request:make-request env))
               (foo.lisp.vinland:*response* (lack/response:make-response 200 ()))
               (foo.lisp.vinland:*route* route-name)
               (foo.lisp.vinland:*binding* (getf env :raven.binding)))
           (declare (type lack/request:request foo.lisp.vinland:*request*)
                    (type lack/response:response foo.lisp.vinland:*response*)
                    (type symbol foo.lisp.vinland:*route*)
                    (type binding-alist foo.lisp.vinland:*binding*)
                    (type string foo.lisp.vinland:*origin*))
           (let ((foo.lisp.vinland:*origin*
                   (let ((uri-scheme (lack/request:request-uri-scheme foo.lisp.vinland:*request*))
                         (server-name (lack/request:request-server-name foo.lisp.vinland:*request*))
                         (server-port (lack/request:request-server-port foo.lisp.vinland:*request*)))
                     (declare (type (simple-array character (*)) uri-scheme server-name)
                              (type integer server-port))
                     (or (case server-port
                           (80 (and (string= "http" uri-scheme)
                                    (format nil "~A://~A" uri-scheme server-name)))
                           (443 (and (string= "https" uri-scheme)
                                     (format nil "~A://~A" uri-scheme server-name))))
                         (format nil "~A://~A:~D" uri-scheme server-name server-port)))))
             (let ((request-uri (lack/request:request-uri foo.lisp.vinland:*request*)))
               (declare (type string request-uri))
               (and (route/simple-max-uri-length metadata)
                    (> (length request-uri)
                       (route/simple-max-uri-length metadata))
                    (error 'foo.lisp.vinland/web:client-error :status-code 414)))
             (let ((request-method (lack/request:request-method foo.lisp.vinland:*request*)))
               (declare (type keyword request-method))
               (unless (member request-method (route/simple-methods metadata) :test #'eq)
                 (error 'foo.lisp.vinland/web:client-error :status-code 405))
               (let ((handler (or (case request-method
                                    (:GET (route/simple-get metadata))
                                    (:HEAD (or (route/simple-head metadata)
                                               (route/simple-get metadata)))
                                    (:POST (route/simple-post metadata))
                                    (:PUT (route/simple-put metadata))
                                    (:PATCH (route/simple-patch metadata))
                                    (:DELETE (route/simple-delete metadata))
                                    (:OPTIONS (or (route/simple-options metadata)
                                                  #'(lambda (env)
                                                      (when (getf env :lack.session.options)
                                                        (setf (getf env :lack.session.options)
                                                              '(:new-session nil :expire nil)))
                                                      (return
                                                        `(204
                                                          (:allow ,(format nil "~{~A~^, ~}"
                                                                           (route/simple-methods metadata))
                                                           :content-length 0
                                                           :set-cookie nil)
                                                          (""))))))
                                    (:TRACE (route/simple-trace metadata))
                                    (:CONNECT (route/simple-connect metadata))
                                    (t
                                     (error 'foo.lisp.vinland/web:server-error :status-code 501)))
                                  (error 'foo.lisp.vinland/web:server-error :status-code 500))))
                 (declare (type function handler))
                 (cond
                   ((and (member request-method '(:GET :HEAD) :test #'eq)
                         (route/simple-provide metadata))
                    (unless (foo.lisp.lack/request/content-negotiation:negotiate-media-type
                             foo.lisp.vinland:*request*
                             (route/simple-provide metadata))
                      (error 'foo.lisp.vinland/web:client-error :status-code 406)))
                   ((and (member request-method '(:POST :PUT :PATCH) :test #'eq)
                         (route/simple-accept metadata))
                    (let* ((content-type (lack/request:request-content-type foo.lisp.vinland:*request*))
                           (content-media-type (and content-type
                                                    (lack/media-type:make-media-type content-type))))
                      (declare (type (or null string) content-type)
                               (type (or null lack/media-type:media-type) content-media-type))
                      (unless (and content-media-type
                                   (some #'(lambda (media-type-str)
                                             (declare (type string media-type-str)
                                                      (type lack/media-type:media-type content-media-type))
                                             (lack/media-type:match-media-type (lack/media-type:make-media-type
                                                                                media-type-str)
                                                                               content-media-type))
                                         (route/simple-accept metadata)))
                        (error 'foo.lisp.vinland/web:client-error :status-code 415)))))
                 (let ((max-content-length (route/simple-max-content-length metadata))
                       (content-length (or (lack/request:request-content-length foo.lisp.vinland:*request*)
                                           0)))
                   (declare (type (or null bignum) max-content-length)
                            (type (or null bignum) content-length))
                   (and max-content-length
                        content-length
                        (= content-length (max content-length max-content-length))
                        (error 'foo.lisp.vinland/web:client-error :status-code 413)))
                 (handler-case (progn
                                 (foo.lisp.lack/request:request-query-parameters
                                  foo.lisp.vinland:*request*)
                                 (foo.lisp.lack/request:request-body-parameters
                                  foo.lisp.vinland:*request*))
                   (error (e)
                     (declare (ignore e))
                     (error 'foo.lisp.vinland/web:client-error :status-code 400)))
                 (let ((metadata-before (route/simple-before metadata))
                       (metadata-after (route/simple-after metadata)))
                   (declare (type function-list metadata-before metadata-after))
                   (let ((result (unwind-protect (catch 'foo.lisp.vinland/web:halt
                                                   (dolist (before metadata-before)
                                                     (declare (type function before))
                                                     (funcall before))
                                                   (funcall handler))
                                   (dolist (after metadata-after)
                                     (declare (type function after))
                                     (funcall after)))))
                     (etypecase result
                       (null
                        (lack/response:finalize-response foo.lisp.vinland:*response*))
                       (list
                        (and (lack/response:response-body foo.lisp.vinland:*response*)
                             (error 'foo.lisp.vinland/web:double-render-error))
                        result)
                       (string
                        (and (lack/response:response-body foo.lisp.vinland:*response*)
                             (error 'foo.lisp.vinland/web:double-render-error))
                        (setf (lack/response:response-body foo.lisp.vinland:*response*)
                              result)
                        (lack/response:finalize-response foo.lisp.vinland:*response*))
                       ((vector (unsigned-byte 8))
                        (and (lack/response:response-body foo.lisp.vinland:*response*)
                             (error 'foo.lisp.vinland/web:double-render-error))
                        (setf (lack/response:response-body foo.lisp.vinland:*response*)
                              result)
                        (lack/response:finalize-response foo.lisp.vinland:*response*))
                       (function
                        (and (lack/response:response-body foo.lisp.vinland:*response*)
                             (error 'foo.lisp.vinland/web:double-render-error))
                        result))))))))))

  #-ecl
  (defmethod foo.lisp.raven:%handle-request/fast ((metadata route/simple)
                                                  (route-name symbol)
                                                  (env list))
    (declare (optimize (speed 3) (debug 0) (safety 0))
             (type symbol route-name)
             (list env))
    #1#)

  #-ecl
  (fast-generic-functions:seal-domain #'foo.lisp.raven:%handle-request/fast
                                      '(route/simple symbol list)))
