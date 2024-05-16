;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/web)

(defmacro query-params ()
  `(lack/request:request-query-parameters foo.lisp.vinland:*request*))

(defmacro body-params ()
  `(lack/request:request-body-parameters foo.lisp.vinland:*request*))

(defmacro get-query-param (key)
  `(foo.lisp.params:get-param
    (lack/request:request-query-parameters foo.lisp.vinland:*request*)
    ,key))

(defmacro collect-query-params (key-list)
  `(foo.lisp.params:collect-params
    (lack/request:request-query-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro get-body-param (key)
  `(foo.lisp.params:get-param
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key))

(defmacro collect-body-params (key-list)
  `(foo.lisp.params:collect-params
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro collect-nested-body-params (key-list)
  `(foo.lisp.params:collect-nested-params
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro negotiate (&body clauses)
  (let ((gensym-negotiate (gensym "negotiate"))
        (gensym-accept-type (gensym "accept-type"))
        (clauses clauses))
    (let ((media-type-gensyms (mapcar
                               (lambda (_)
                                 (declare (ignore _))
                                 (gensym "media-type"))
                               clauses))
          (fallback-body (when (symbolp (caar (last clauses))) (cdar (last clauses)))))
      `(block ,gensym-negotiate
         (dolist (,gensym-accept-type (lack/request:request-accept foo.lisp.vinland:*request*))
           ,@(loop for clause in clauses
                   for gensym-media-type in media-type-gensyms
                   unless (symbolp (car clause))
                     collect `(let ((,gensym-media-type
                                      (lack/media-type:make-media-type ,(car clause))))
                                (when (lack/media-type:match-media-type ,gensym-accept-type
                                                                        ,gensym-media-type)
                                  (return-from ,gensym-negotiate
                                    (progn
                                      ,@(cdr clause)))))))
         ,@(if fallback-body
               `(progn ,@fallback-body)
               '((signal 'foo.lisp.vinland/web:client-error :status 406)))))))

(defmacro set-response-status (status)
  (let ((gensym-status (gensym "status"))
        (gensym-status-code (gensym "status-code")))
    `(let ((,gensym-status ,status)
           (,gensym-status-code (etypecase ,gensym-status
                                  (keyword (foo.lisp.vinland/response:status-keyword-to-code
                                            ,gensym-status))
                                  (integer ,gensym-status))))
       (when (typep ,gensym-status-code
                    'foo.lisp.vinland/response:status-code-redirect)
         (signal 'foo.lisp.vinland/web:unauthorized-redirect-error
                  :status-code ,gensym-status-code
                  :location (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                                  :location)))
       (setf (lack/response:response-status foo.lisp.vinland:*response*)
             ,gensym-status-code)
       (values))))

(defmacro set-response-headers (&rest headers)
  `(progn
     (setf (lack/response:response-headers foo.lisp.vinland:*response*)
           (append (lack/response:response-headers foo.lisp.vinland:*response*)
                   (list ,@headers)))
     (values)))

(defmacro binding (name)
  "Retrieves a path binding by keyword."
  (let ((gensym-name (gensym "binding-name")))
    `(let ((,gensym-name ,name))
       (or (cdr (assoc ,gensym-name foo.lisp.vinland:*binding* :test #'eq))
           (signal 'foo.lisp.vinland/web:invalid-binding-error :name ,gensym-name)))))

(defmacro cookie (cookie-name)
  "Retrieves a cookie by its name."
  `(cdr
    (assoc ,cookie-name (lack/request:request-cookies foo.lisp.vinland:*request*)
           :test #'string=)))

(defmacro set-cookies (cookies-alist)
  "Sets cookies from an association list. Cookie name strings are the CARs and cookie options
plists are the CDRs."
  `(progn
     (loop for (name . options) in ,cookies-alist
           do (progn
                (push options (lack/response:response-set-cookies foo.lisp.vinland:*response*))
                (push name (lack/response:response-set-cookies foo.lisp.vinland:*response*))))
     (values)))

(defmacro delete-cookie (name)
  "Deletes the cookie by NAME by setting another cookie of the same name that expires at
UNIX epoch time."
  `(set-cookies (list (cons ,name (list :value ""
                                        :path "/"
                                        :expires 2208988800)))))

(defmacro set-session-options (options-plist)
  `(progn
     ,@(loop for (key value) on options-plist by #'cddr
             do`(setf (getf (getf (lack/request:request-env foo.lisp.vinland:*request*)
                                  :lack.session.options)
                            ,key)
                      ,value))
     (values)))

(defmacro session (key)
  `(gethash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                       :lack.session)))

(defmacro set-session (key value)
  `(setf (gethash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                             :lack.session))
         ,value))

(defmacro delete-session (key)
  `(remhash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                       :lack.session)))

(defmacro clear-session ()
  `(clrhash (getf (lack/request:request-env foo.lisp.vinland:*request*)
                  :lack.session)))

(defmacro csrf-token ()
  `(lack/middleware/csrf:csrf-token (getf (lack/request:request-env
                                           foo.lisp.vinland:*request*)
                                          :lack.session)))

(defmacro halt (&optional response)
  `(throw 'foo.lisp.vinland/web:halt ,response))

(defmacro client-error (status)
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.vinland/response:status-keyword-to-code status))
                       (integer status))))
    (check-type status-code foo.lisp.vinland/response:status-code-client-error)
    `(signal 'foo.lisp.vinland/web:client-error :status ,status-code)))

(defmacro server-error (status)
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.vinland/response:status-keyword-to-code status))
                       (integer status))))
    (check-type status-code foo.lisp.vinland/response:status-code-server-error)
    `(signal 'foo.lisp.vinland/web:server-error :status ,status-code)))

(defmacro html-safe (html)
  `(foo.lisp.vinland/web:make-html-safe :value ,html))

(defmacro render (&key (status 200) headers view args)
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.vinland/response:status-keyword-to-code status))
                       (integer status))))
    (check-type status-code foo.lisp.vinland/response:status-code)
    `(foo.lisp.vinland/web:respond :status ,status-code
                                   :headers ,headers
                                   :render #'(lambda () (apply ,view ,args)))))

(define-condition unauthorized-redirect-error (error)
  ((status-code :initarg :status-code
                :type foo.lisp.vinland/response:status-code-redirect)
   (other-host-p :initarg :other-host)
   (location :initarg :location)
   (origin :initarg :origin))
  (:report (lambda (condition stream)
             (with-slots (status-code location)
                 condition
               (format stream "Unauthorized redirect: ~A: ~A"
                       status-code
                       location)))))

(define-condition double-render-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Attempted to set the response body more than once"))))

(define-condition invalid-binding-error (error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (format stream "Invalid binding: ~A" name)))))

(define-condition client-error (simple-error)
  ((status :initarg :status
           :type foo.lisp.vinland/response:status-code-client-error))
  (:report (lambda (condition stream)
             (with-slots (status) condition
               (format stream "Client error: ~A" status)))))

(define-condition server-error (simple-error)
  ((status :initarg :status
           :type foo.lisp.vinland/response:status-code-server-error))
  (:report (lambda (condition stream)
             (with-slots (status) condition
               (format stream "Server error: ~A" status)))))

(defstruct html-safe
  (value "" :type string))

(defun respond (&key (status 200) render headers)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or integer keyword) status)
           (type (or null function) render)
           (type list headers))
  (when headers
    (setf (lack/response:response-headers foo.lisp.vinland:*response*)
          (append (lack/response:response-headers
                   foo.lisp.vinland:*response*)
                  headers)))
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword (foo.lisp.vinland/response:status-keyword-to-code
                            status))
                  (integer status))))
          (when (typep status-code
                       'foo.lisp.vinland/response:status-code-redirect)
            (signal 'unauthorized-redirect-error
                     :status-code status-code
                     :location (getf (lack/response:response-headers
                                      foo.lisp.vinland:*response*)
                                     :location)))
          status-code))
  (when render
    (when (lack/response:response-body foo.lisp.vinland:*response*)
      (error 'foo.lisp.vinland/web:double-render-error))
    (setf (lack/response:response-body foo.lisp.vinland:*response*)
          (funcall render)))
  (values))

(defun redirect (location &key (status 302)
                            headers
                            flash
                            allow-other-host-p)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type  location)
           (type (or integer keyword) status)
           (type list headers flash)
           (type boolean allow-other-host-p))
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword
                   (foo.lisp.vinland/response:status-keyword-to-code
                    status))
                  (integer status))))
          (check-type status-code
                      foo.lisp.vinland/response:status-code-redirect)
          status-code))
  (assert (> (length location) 0)
          nil
          "Empty redirect location")
  (unless (char= #\/ (char location 0))
    (let* ((location-uri (quri:parse-uri location))
           (location-origin (format nil "~A:~A"
                                    (quri:uri-scheme location-uri)
                                    (quri:uri-host location-uri))))
      (unless (or (string= location-origin foo.lisp.vinland:*origin*)
                  allow-other-host-p)
        (signal 'unauthorized-redirect-error
                :origin foo.lisp.vinland:*origin*
                :location location
                :other-host-p t))))
  (setf (lack/response:response-headers foo.lisp.vinland:*response*)
        (append (lack/response:response-headers
                 foo.lisp.vinland:*response*)
                (append headers (list :location location))))
  (when flash
    (foo.lisp.flash/state:flash foo.lisp.vinland:*flash* flash))
  (when (lack/response:response-body foo.lisp.vinland:*response*)
    (error 'foo.lisp.vinland/web:double-render-error))
  (setf (lack/response:response-body foo.lisp.vinland:*response*)
        "")
  (values))

(defun redirect-back (&key (status 302)
                        headers
                        flash
                        default-location
                        allow-other-host-p)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or integer keyword) status)
           (type list headers flash)
           (type (or null string) default-location)
           (type boolean allow-other-host-p))
  (check-type default-location string)
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword
                   (foo.lisp.vinland/response:status-keyword-to-code
                    status))
                  (integer status))))
          (declare (type integer status-code))
          (check-type status-code
                      foo.lisp.vinland/response:status-code-redirect)
          status-code))
  (let* ((referrer (gethash "referer"
                            (lack/request:request-headers
                             foo.lisp.vinland:*request*)))
         (location (if (and referrer
                            (> (length referrer) 0)
                            (or (char= #\/ (char referrer 0))
                                allow-other-host-p))
                       referrer
                       default-location))
         (location-uri (location-uri (quri:parse-uri location)))
         (location-origin (format nil "~A:~A"
                                  (quri:uri-scheme location-uri)
                                  (quri:uri-host location-uri))))
    (declare (type (or null string) referrer)
             (type string location location-origin)
             (type quri:uri location-uri))
    (when (and (not (string= location-origin
                             foo.lisp.vinland:*origin*))
               (null allow-other-host-p))
      (signal 'unauthorized-redirect-error
              :origin foo.lisp.vinland:*origin*
              :location location))
    (setf (lack/response:response-headers foo.lisp.vinland:*response*)
          (append (lack/response:response-headers
                   foo.lisp.vinland:*response*)
                  (append headers (list :location location)))))
  (when flash
    (foo.lisp.flash/state:flash foo.lisp.vinland:*flash* flash))
  (when (lack/response:response-body foo.lisp.vinland:*response*)
    (error 'foo.lisp.vinland/web:double-render-error))
  (setf (lack/response:response-body foo.lisp.vinland:*response*)
        "")
  (values))
