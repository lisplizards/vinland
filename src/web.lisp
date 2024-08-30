;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland/web)

(define-condition unsafe-redirect-error (simple-error)
  ((status-code :initarg :status-code
                :type foo.lisp.http-response:status-code-redirect)
   (location :initarg :location)
   (origin :initarg :origin))
  (:report (lambda (condition stream)
             (with-slots (status-code location)
                 condition
               (format stream
                       "Unsafe redirect: ~A: ~A" status-code location))))
  (:documentation "Error signalled when the server attempted to respond with
a redirect to a different host without explicitly authorizing a redirect to
an different host; may indicate an Open Redirect attack if the redirect
location is provided by user input."))

(define-condition redirect-not-allowed-error (error)
  ((status-code :initarg :status-code
                :type fixnum)
   (location :initarg :location
             :type string))
  (:report (lambda (condition stream)
             (with-slots (status-code location)
                 condition
               (format stream
                       "Redirect not allowed: (Status: ~A, Location: ~A)"
                       status-code location))))
  (:documentation "Error signalled when attempting to redirect from a function
or macro that does not provide mitigation against Open Redirect attacks. Redirects
(status: 3xx) should be performed by function REDIRECT or REDIRECT-BACK."))

(define-condition double-render-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Attempted to set the response body more than once")))
  (:documentation "Error signalled when attempting to set the response body more than once."))

(define-condition invalid-binding-error (error)
  ((name :initarg :name))
  (:report (lambda (condition stream)
             (with-slots (name) condition
               (format stream "Invalid binding: ~A" name))))
  (:documentation "Error signalled when attempting to look up the value for a dynamic
path component that does not exist in the route pattern for the current URL."))

(defstruct html-safe "Box for trusted HTML content."
  (value "" :type string))

(defmacro query-params ()
  "Returns the query parameters association list from the request
struct; a macro."
  `(lack/request:request-query-parameters foo.lisp.vinland:*request*))

(defmacro body-params ()
  "Returns the body parameters association list from the request
struct; a macro."
  `(lack/request:request-body-parameters foo.lisp.vinland:*request*))

(defmacro get-query-param (key)
  "Returns the value associated with KEY, a string, from the request
struct's query-parameters association list. Returns NIL when no such
key exists; a macro."
  `(foo.lisp.params:get-param
    (lack/request:request-query-parameters foo.lisp.vinland:*request*)
    ,key))

(defmacro collect-query-params (key-list)
  "Maps over KEY-LIST, a list of strings representing query parameter
names, returning a list of the values associated with each item or NIL;
a macro.

If GET-QUERY-PARAM were a function, it would be equivalent to calling:
`(MAPCAR #'GET-QUERY-PARAM KEY-LIST)'."
  `(foo.lisp.params:collect-params
    (lack/request:request-query-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro get-body-param (key)
  "Returns the value associated with KEY, a string, from the request
struct's body-parameters association list. Returns NIL when no such key
exists; a macro."
  `(foo.lisp.params:get-param
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key))

(defmacro collect-body-params (key-list)
  "Given KEY-LIST, a list of strings representing body parameter names,
maps over KEY-LIST, returning the value associated with the item or NIL;
a macro.

If GET-BODY-PARAM were a function, it would be equivalent to calling:
`(MAPCAR #'GET-BODY-PARAM KEY-LIST)'."
  `(foo.lisp.params:collect-params
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro collect-nested-body-params (key-list)
  "Maps over KEY-LIST, a list of string-lists representing paths to
parameters in the request body association list, and returns a new list
containing the found value from each sublist or NIL; a macro.

See FOO.LISP.PARAMS:COLLECT-NESTED-PARAMS for more specifics."
  `(foo.lisp.params:collect-nested-params
    (lack/request:request-body-parameters foo.lisp.vinland:*request*)
    ,key-list))

(defmacro negotiate (&body clauses)
  "CASE/COND-like macro to perform content negotiation based on the request
Accept header.

Iterates across each media-type specied in the request Accept header,
for each iteration checking whether any clause is defined that starts
with the given media-type, returning the clause body when there is a match.

When no media-type clause matches the request Accept header and a final clause
is present, returns the fallback clause body; when no fallback clause is present,
signals a CLIENT-ERROR with status 406: Not Acceptable."
  (let ((gensym-negotiate (gensym "negotiate"))
        (gensym-request-accept (gensym "request-accept"))
        (clauses clauses))
    (let ((media-type-gensyms (mapcar
                               (lambda (_)
                                 (declare (ignore _))
                                 (gensym "media-type"))
                               clauses))
          (fallback-body (when (symbolp (caar (last clauses))) (cdar (last clauses)))))
      `(catch ',gensym-negotiate
         (dolist (,gensym-request-accept (lack/request:request-accept foo.lisp.vinland:*request*))
           ,@(loop for clause in clauses
                   for gensym-media-type in media-type-gensyms
                   unless (symbolp (car clause))
                     collect `(let ((,gensym-media-type
                                      (lack/media-type:make-media-type ,(car clause))))
                                (when (lack/media-type:match-media-type ,gensym-request-accept
                                                                        ,gensym-media-type)
                                  (throw ',gensym-negotiate
                                    (progn
                                      ,@(cdr clause)))))))
         ,@(if fallback-body
               `((progn ,@fallback-body))
               '((error 'foo.lisp.vinland/web:client-error :status-code 406)))))))

(defmacro set-response-status (status)
  "Sets the HTTP response status code, a fixnum or keyword.
Returns NIL; a macro.

Signals UNKNOWN-STATUS-ERROR when given a keyword that cannot be
translated to a known HTTP response status code integer.

Signals REDIRECT-NOT-ALLOWED-ERROR when given a redirect (3xx)
specifier, as redirects should be performed with function REDIRECT
or REDIRECT-BACK."
  (let ((gensym-status (gensym "status"))
        (gensym-status-code (gensym "status-code")))
    `(let* ((,gensym-status ,status)
            (,gensym-status-code (etypecase ,gensym-status
                                   (keyword (foo.lisp.http-response:status-keyword-to-code
                                             ,gensym-status))
                                   (fixnum ,gensym-status))))
       (when (typep ,gensym-status-code
                    'foo.lisp.http-response:status-code-redirect)
         (error 'foo.lisp.vinland/web:redirect-not-allowed-error
                :status-code ,gensym-status-code
                :location (getf (lack/response:response-headers foo.lisp.vinland:*response*)
                                :location)))
       (setf (lack/response:response-status foo.lisp.vinland:*response*)
             ,gensym-status-code)
       (values))))

(defmacro set-response-headers (&rest headers)
  "Sets response headers from variadic argument HEADERS, a property list.
Returns NIL; a macro."
  `(let ((response-headers (copy-list (lack/response:response-headers foo.lisp.vinland:*response*))))
     (declare (type list response-headers))
     (loop for (key value) on ',headers by #'cddr
           do (setf (getf response-headers key)
                    value))
     (setf (lack/response:response-headers foo.lisp.vinland:*response*)
           response-headers)
     (values)))

(defmacro append-response-headers (&rest headers)
  "Appends response headers from variadic header HEADERS, a property list.
Returns NIL; a macro."
  (let ((gensym-headers (gensym "headers"))
        (gensym-response-headers (gensym "response-headers")))
    `(let ((,gensym-headers ',headers)
           (,gensym-response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
       (declare (type list ,gensym-headers ,gensym-response-headers))
       (setf (lack/response:response-headers foo.lisp.vinland:*response*)
             (if ,gensym-response-headers
                 (append ,gensym-response-headers ,gensym-headers)
                 ,gensym-headers))
       (values))))

(defmacro binding (name)
  "Given NAME, a keyword representing a dynamic path component for the
current route as specified in FOO.LISP.RAVEN:COMPILE-ROUTER, returns
the actual value found in the URL, a string; a macro.

Signals INVALID-BINDING-ERROR when the binding does not exist.

Example:
If the path specified in FOO.LISP.RAVEN:COMPILE-ROUTER for the current URL
is \"/widgets/:widget-id\", then the path component value can be retrieved
by calling `(BINDING :|widget-id|)'.

Note that case of the dynamic path component name is preserved in the keyword."
  (let ((gensym-binding-name (gensym "binding-name")))
    `(let ((,gensym-binding-name ,name))
       (or (cdr (assoc ,gensym-binding-name foo.lisp.vinland:*binding* :test #'eq))
           (error 'foo.lisp.vinland/web:invalid-binding-error :name ,gensym-binding-name)))))

(defmacro cookie (cookie-name)
  "Looks up the the cookie identified by COOKIE-NAME in the request Cookie
header and returns the URL-decoded cookie value, a string; a macro."
  `(cdr (assoc ,cookie-name (lack/request:request-cookies foo.lisp.vinland:*request*)
               :test #'string=)))

(defmacro set-cookies (cookies-alist)
  "Given an association list COOKIES-ALIST where each key is a unique
cookie-name string and each value is a cookie options property list,
appends to the response struct's SET-COOKIES slot. Returns NIL; a macro.

Cookie options include: DOMAIN, PATH, EXPIRES, SECURE, HTTPONLY, SAMESITE.

At the conclusion of request processing, #'LACK/RESPONSE:FINALIZE-RESPONSE
calls #'LACK/RESPONSE:FINALIZE-COOKIES which adds Set-Cookie headers to the
response for each entry in the response struct's SET-COOKIES slot."
  `(progn
     (loop for (name . options) in ,cookies-alist
           do (progn
                (push options (lack/response:response-set-cookies foo.lisp.vinland:*response*))
                (push name (lack/response:response-set-cookies foo.lisp.vinland:*response*))))
     (values)))

(defmacro delete-cookie (name)
  "Instructs the user-agent to remove cookie NAME by adding a Set-Cookie
header to the response with the same name but with an expiration in the
past, at UNIX epoch. Returns NIL; a macro."
  `(set-cookies (list (cons ,name (list :value ""
                                        :path "/"
                                        :httponly t
                                        :secure nil
                                        :samesite :strict
                                        :expires 2208988800)))))

(defmacro set-session-options (options-plist)
  "Updates environment key LACK.SESSION.OPTIONS, returning NIL; a macro.
Property list OPTIONS-PLIST may contain keys: ID, NEW-SESSION, CHANGE-ID, EXPIRE."
  `(let ((session-options-copy (copy-list (getf (lack/request:request-env foo.lisp.vinland:*request*)
                                                :lack.session.options))))
     (loop for (key value) of-type (keyword t) on ,options-plist by #'cddr
           do (setf (getf session-options-copy key)
                    value))
     (setf (getf (lack/request:request-env foo.lisp.vinland:*request*)
                 :lack.session.options)
           session-options-copy)
     (values)))

(defmacro session (key)
  "Returns the value stored in the Lack session hash-table under KEY; a macro.

KEY may often be a string, but can be of any type suitable as a hash-table key
and serializable by the session backend."
  `(gethash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                       :lack.session)))

(defmacro set-session (key value)
  "Sets VALUE in the Lack session hash-table for KEY; a macro.

KEY may often be a string, but can be of any type suitable as a hash-table key
and serializable by the session backend.

VALUE can be of any type serializable for the session backend."
  `(setf (gethash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                             :lack.session))
         ,value))

(defmacro delete-session (key)
  "Removes the entry from the Lack session hash-table identified by KEY; a macro."
  `(remhash ,key (getf (lack/request:request-env foo.lisp.vinland:*request*)
                       :lack.session)))

(defmacro clear-session ()
  "Empties the Lack session hash-table; a macro."
  `(clrhash (getf (lack/request:request-env foo.lisp.vinland:*request*)
                  :lack.session)))

(defmacro csrf-token ()
  "Returns the anti Cross Site Request Forgery synchronizer token, a
string, from the Lack session; a macro. Embed as a hidden input in
each non-GET HTML form."
  `(lack/middleware/csrf:csrf-token (getf (lack/request:request-env
                                           foo.lisp.vinland:*request*)
                                          :lack.session)))

(defmacro halt (&optional result)
  "Calls THROW with HALT and optional parameter RESULT as an escape from
further request processing; a macro. RESULT can be of any type returnable
by the route handler."
  `(throw 'foo.lisp.vinland/web:halt ,result))

(defmacro client-error (status)
  "Signals CLIENT-ERROR with STATUS, a fixnum or keyword representing
a 4xx range HTTP response status code; a macro.

Signals UNKNOWN-STATUS-ERROR when given a keyword that cannot be translated
to a known HTTP response status code.

Signals TYPE-ERROR when given a non-4xx HTTP response status."
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.http-response:status-keyword-to-code status))
                       (fixnum status))))
    (check-type status-code foo.lisp.http-response:status-code-client-error)
    `(error 'foo.lisp.http-response:client-error :status-code ,status-code)))

(defmacro server-error (status)
  "Signals CLIENT-ERROR with STATUS, a fixnum or keyword representing
a 5xx range HTTP response status code; a macro.

Signals UNKNOWN-STATUS-ERROR when given a keyword that cannot be translated
to a known HTTP response status code.

Signals TYPE-ERROR when given a non-5xx HTTP response status."
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.http-response:status-keyword-to-code status))
                       (fixnum status))))
    (check-type status-code foo.lisp.http-response:status-code-server-error)
    `(error 'foo.lisp.http-response:server-error :status-code ,status-code)))

(defmacro html-safe (html)
  "Returns an HTML-SAFE instance that contains HTML, a string. Used to
indicate that this HTML content is trusted should be rendered without
sanitization by the view layer; a macro.

Warning: never call with HTML from user input; *trusted* HTML only."
  `(foo.lisp.vinland/web:make-html-safe :value ,html))

(defmacro render (&key (status 200) headers view args)
  "Sets the response status code based on STATUS (fixnum or keyword), sets
response headers from HEADERS, a property list, and sets the response body to
the result of calling function VIEW with arguments ARGS. Returns NIL; a macro.

Signals UNSAFE-REDIRECT-ERROR when given a redirect (3xx) response code, as
redirects should be performed with either REDIRECT or REDIRECT-BACK."
  (let ((status-code (etypecase status
                       (keyword (foo.lisp.http-response:status-keyword-to-code status))
                       (fixnum status))))
    (check-type status-code foo.lisp.http-response:status-code)
    `(foo.lisp.vinland/web:respond :status ,status-code
                                   :headers ,headers
                                   :render #'(lambda () (apply ,view ,args)))))

(defmacro route-url (route-name &rest kwargs &key &allow-other-keys)
  "Returns a URL string for ROUTE-NAME by concatenating *ORIGIN* and the result
of calling the ROUTE-PATH."
  `(concatenate 'string foo.lisp.vinland:*origin* (route-path ,route-name ,@kwargs)))

(declaim (ftype (function (&key (:status (or fixnum keyword))
                                (:render function)
                                (:headers list))
                          null)
                respond))
(defun respond (&key (status 200) render headers)
  "Sets the response status code based on STATUS (fixnum or keyword),
sets response headers from HEADERS, a property list, and sets the response
body to the result of calling parameter RENDER, a function. Returns NIL;
a function.

RESPOND should probably not be called directly from route handlers, and
instead a wrapper macro or function should be defined to pass the RENDER
argument (see macro: RENDER).

Signals REDIRECT-NOT-ALLOWED-ERROR when given a redirect (3xx) response code,
as redirects should be performed with either REDIRECT or REDIRECT-BACK."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or fixnum keyword) status)
           (type function render)
           (type list headers))
  (when headers
    (setf (lack/response:response-headers foo.lisp.vinland:*response*)
          (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
            (declare (type list response-headers))
            (if (null response-headers)
                headers
                (append response-headers headers)))))
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword (foo.lisp.http-response:status-keyword-to-code
                            status))
                  (fixnum status))))
          (typecase status-code
            (foo.lisp.http-response:status-code-redirect
             (error 'redirect-not-allowed-error
                    :status-code status-code
                    :location (getf (lack/response:response-headers
                                     foo.lisp.vinland:*response*)
                                    :location)))
            (t
             status-code))))
  (when (lack/response:response-body foo.lisp.vinland:*response*)
    (error 'foo.lisp.vinland/web:double-render-error))
  (setf (lack/response:response-body foo.lisp.vinland:*response*)
        (funcall render))
  (values))

(declaim (ftype (function ((simple-array character (*)) &key (:status (or fixnum keyword))
                                                        (:headers list)
                                                        (:flash list)
                                                        (:allow-other-host boolean))
                          null)
                redirect))
(defun redirect (location &key (status 302)
                            headers
                            flash
                            allow-other-host)
  "Redirects the user-agent to LOCATION; a function.

Sets response status code based from STATUS (a 3xx range fixnum or equivalent
keyword), sets response headers (except for Location) from HEADERS, a property
list, and sets FOO.LISP.VINLAND:*FLASH* from FLASH, a property list containing
arbitrary flash data.

\"Open Redirect\" protection: when the location belongs to a different host and
ALLOW-OTHER-HOST is NIL, signals condition UNSAFE-REDIRECT-ERROR."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or null (simple-array character (*))) location)
           (type (or fixnum keyword) status)
           (type (simple-array character (*)) location)
           (type list headers flash)
           (type boolean allow-other-host))
  (check-type location (simple-array character (*)))
  (assert (> (length location) 0)
          nil
          "Empty redirect location")
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword
                   (foo.lisp.http-response:status-keyword-to-code
                    status))
                  (fixnum status))))
          (check-type status-code
                      foo.lisp.http-response:status-code-redirect)
          status-code))
  (let ((location-host (multiple-value-bind (scheme userinfo host port path query fragment)
                           (quri:parse-uri location)
                         (declare (ignore scheme userinfo port path query fragment))
                         host)))
    (declare (type (or null string)))
    (and location-host
         (null allow-other-host)
         (not (equal location-host
                     (lack/request:request-server-name foo.lisp.vinland:*request*)))
         (error 'unsafe-redirect-error
                :origin foo.lisp.vinland:*origin*
                :location location)))
  (and (getf headers :location)
       (error 'redirect-not-allowed-error
              :response-code (lack/response:response-status foo.lisp.vinland:*response*)
              :location (getf headers :location)))
  (setf (lack/response:response-headers foo.lisp.vinland:*response*)
        (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
          (declare (type list response-headers))
          (if (null response-headers)
              (if headers
                  (append headers (list :location location))
                  (list :location location))
              (append response-headers headers (list :location location)))))
  (when flash
    (foo.lisp.flash/state:flash foo.lisp.vinland:*flash* flash))
  (when (lack/response:response-body foo.lisp.vinland:*response*)
    (error 'foo.lisp.vinland/web:double-render-error))
  (setf (lack/response:response-body foo.lisp.vinland:*response*)
        "")
  (values))

(declaim (ftype (function (&key (:status (or fixnum keyword))
                                (:headers list)
                                (:flash list)
                                (:default-location (or null (simple-array character (*))))
                                (:allow-other-host boolean))
                          null)
                redirect-back))
(defun redirect-back (&key (status 302)
                        headers
                        flash
                        default-location
                        allow-other-host)
  "Redirects the user-agent to the request Referer or, if none is present, falls
back to DEFAULT-LOCATION; a function.

Sets response status code based from STATUS (a 3xx range fixnum or equivalent
keyword), sets response headers (except for Location) from HEADERS, a property
list, and sets FOO.LISP.VINLAND:*FLASH* from FLASH, a property list containing
arbitrary flash data.

\"Open Redirect\" protection: when the location belongs to a different host and
ALLOW-OTHER-HOST is NIL, signals condition UNSAFE-REDIRECT-ERROR."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type (or fixnum keyword) status)
           (type list headers flash)
           (type (or null (simple-array character (*))) default-location)
           (type boolean allow-other-host))
  (check-type default-location string)
  (assert (> (length default-location) 0)
          nil
          "Empty redirect default-location")
  (setf (lack/response:response-status foo.lisp.vinland:*response*)
        (let ((status-code
                (etypecase status
                  (keyword
                   (foo.lisp.http-response:status-keyword-to-code
                    status))
                  (fixnum status))))
          (declare (type fixnum status-code))
          (check-type status-code
                      foo.lisp.http-response:status-code-redirect)
          status-code))
  (let* ((referrer (gethash "referer"
                            (lack/request:request-headers
                             foo.lisp.vinland:*request*)))
         (location (if (and referrer
                            (> (length referrer) 0)
                            (or (char= #\/ (char referrer 0))
                                allow-other-host))
                       referrer
                       default-location)))
    (declare (type (or null simple-string) referrer)
             (type string location))
    (let ((location-host (multiple-value-bind (scheme userinfo host port path query fragment)
                             (quri:parse-uri location)
                           (declare (ignore scheme userinfo port path query fragment))
                           host)))
      (declare (type (or null string)))
      (and location-host
           (null allow-other-host)
           (not (equal location-host
                       (lack/request:request-server-name foo.lisp.vinland:*request*)))
           (error 'unsafe-redirect-error
                  :origin foo.lisp.vinland:*origin*
                  :location location)))
    (and (getf headers :location)
         (error 'redirect-not-allowed-error
                :response-code (lack/response:response-status foo.lisp.vinland:*response*)
                :location (getf headers :location)))
    (setf (lack/response:response-headers foo.lisp.vinland:*response*)
          (let ((response-headers (lack/response:response-headers foo.lisp.vinland:*response*)))
            (declare (type list response-headers))
            (if (null response-headers)
                (if headers
                    (append headers (list :location location))
                    (list :location location))
                (append response-headers headers (list :location location))))))
  (when flash
    (foo.lisp.flash/state:flash foo.lisp.vinland:*flash* flash))
  (when (lack/response:response-body foo.lisp.vinland:*response*)
    (error 'foo.lisp.vinland/web:double-render-error))
  (setf (lack/response:response-body foo.lisp.vinland:*response*)
        "")
  (values))

(defun importmap (imports)
  "Returns a JSON object string that can be used as the contents of an importmap script tag.
IMPORTS, an association list, represents a list of imports (URLs or paths)."
  (declare (type list imports))
  (jojo:to-json (list (cons "imports" imports)) :from :alist))
