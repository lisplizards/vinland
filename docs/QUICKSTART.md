# Vinland Quickstart Guide

This guide aims to provides a brief overview of Vinland's essential features, helping you to get started with the framework and understand where to look for more information. The terms 'controller' and 'route handler' will be used interchangeably.

## Skeleton

### Generate project

Vinland provides a project generator, contained in system `FOO.LISP.VINLAND.SKELETON`. Use of the skeleton is optional but recommended: it's the easiest way to get started.

You can generate a project in one of two flavors, "web" (the default) or "api".

Among other options, you can also choose the test framework ("parachute" or "rove"); the default is "parachute".

Web-flavor example: "web" is the default, but for clarity here we explicitly specify the option.

```common-lisp
(ql:quickload "foo.lisp.vinland.skeleton")

(foo.lisp.vinland.skeleton:make-project
  :name :web-demo
  :license "MIT"
  :author "Erik the Red"
  :flavor "web")
```

API-flavor example: specify a non-default name for the binary executable and rove as the test framework.

```common-lisp
(ql:quickload "foo.lisp.vinland.skeleton")

(foo.lisp.vinland.skeleton:make-project
  :name :api-demo
  :program "api-server"
  :author "Bjarni Herjólfsson"
  :maintainer "Leif Erikson"
  :flavor "api"
  :test-framework "rove")
```

Inspect the complete list of options by running:

```common-lisp
(documentation 'foo.lisp.vinland.skeleton:make-project 'function)
```

For production, you can build a binary executable for your platform by running `(asdf:make :demo-app)`.

In development, you can start your application by calling `CLACK:CLACKUP`.

```common-lisp
(ql:quickload '("demo-app" "clack"))

(clack:clackup demo-app/app:*app* :server :hunchentoot :port 5000 :use-default-middlewares nil :debug nil)
```

### Project structure

The following files are created by default for the web flavor:

```
src/
├── app.lisp
├── cli.lisp
├── component.lisp
├── config.lisp
├── controller.lisp
├── dao.lisp
├── http-error.lisp
├── layout.lisp
├── package.lisp
├── params.lisp
├── store.lisp
├── turbo.lisp
├── user.lisp
├── view.lisp
└── web.lisp
```

Brief summary of these files:

* `app.lisp`: Contains the Lack builder middleware pipeline definition; wraps the Clack application (the `*WEB*` function from web.lisp) in middleware
* `cli.lisp`: Binary executable program entrypoint; defines subcommands used to start the server, match routes, or find a specific route
* `component.lisp`: Defines functions used to render Spinneret UI components; these building block component functions are called from view.lisp or turbo.lisp
* `config.lisp`: Defines special, non-dynamic variables that may be referenced from other packages
* `controller.lisp`: Defines route handlers
* `dao.lisp`: Define database classes or structs
* `http-error.lisp`: Defines functions used to generate static error pages and to handle HTTP errors using the errors middleware
* `layout.lisp`: Defines macros to render HTML documents with Spinneret
* `package.lisp`: Defines all packages
* `params.lisp`: Defines generic function specializers used to validate body or query parameters
* `store.lisp`: Defines functions to manage DAO entities and perform any database operations
* `turbo.lisp`: Defines functions to render Turbo-stream documents; these custom elements contain instructions to declaratively update the DOM; turbo functions may call functions defined in component.lisp
* `user.lisp`: Defines session related functions; used to load the user for the current request
* `view.lisp`: Defines functions to render HTML documents; view functions may call functions defined in component.lisp
* `web.lisp`: Defines routing rules and the Clack application function (`*WEB*`); the `*WEB*` object is not yet wrapped in middleware

Some of these files are empty by default, such as dao.lisp, store.lisp, turbo.lisp, and user.lisp; you can fill them in if you need them. The generation of turbo.lisp can be skipped by passing `:skip-turbo t` to `make-project`.

## Request handling

### web.lisp

File `web.lisp` defines routing rules and the unwrapped Clack application. Let's examine vinland-todo-app's [web.lisp](https://github.com/lisplizards/vinland-todo-app/blob/master/src/web.lisp):

```common-lisp
(in-package #:todo-app/web)

(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root)
     ("/about" ,'about)
     ("/hello-world" ,'hello-world)
     ("/error/server" ,'error/server)
     ("/error/simple" ,'error/simple)
     ("/🧐" ,'🧐)
     ("/login" ,'login)
     ("/register" ,'register)
     ("/action/login" ,'create-login)
     ("/action/register" ,'create-registration)
     ("/action/logout" ,'create-logout)
     ("/lists" ,'todo-lists)
     ("/lists/:todo-list" ,'todo-list)
     ("/lists/:todo-list/items" ,'todo-items)
     ("/lists/:todo-list/items/:todo-item" ,'todo-item))
   :fast-dispatch t)
  "The application router; a lambda function")

(defparameter *web* (funcall *router* :clack)
  "The unwrapped clack application")
```

Special variable `*ROUTER*` names a lambda function that creates a Clack application when called with keyword `:CLACK` (bound to `*WEB*`); in addition, the router lambda can be used to introspect routes.

Calling the router lambda with a `:list-routes` list will match all routes prefixed with the given string; in this case, it will list routing information for the final four routes.

```common-lisp
funcall *router* '(:list-routes "/lists")
```

Similarly, to introspect routing information for a single specific route, you can call the router with a `:find-route` list, in this case matching the `TODO-LIST` route:

```common-lisp
(funcall *router* '(:find-route "/lists/xyz"))
```

You may have noticed that `COMPILE-ROUTER` is defined in package `FOO.LISP.RAVEN`: Raven is a URL dispatching library found [here](https://github.com/lisplizards/raven). All foo.lisp packages can be found under the [lisplizards GitHub organization](https://github.com/orgs/lisplizards/repositories).

### controller.lisp

Vinland controllers are functions, so from your Lisp REPL you can call  `(documentation 'root 'function)` or call `(trace root)` to return documentation or start tracing the handler, respectively.

The route-handler examples in this section are pulled from [controller.lisp](https://github.com/lisplizards/vinland-todo-app/blob/master/src/controller.lisp) of the vinland-todo-app.

The hello-world route handler demonstrates dispatching to a standard function, which must return a Clack response list:

```common-lisp
(export 'hello-world)
(defun hello-world (env)
  "Demonstration of a \"bare\" handler"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (ignore env))
  `(200
    (:content-type "text/plain"
     :content-length 13)
    ("Hello, World.")))
```

The first element is the response status code; the second element is a plist of response headers, and the third, optional, element is the response body, either a list of strings or a `(vector (unsigned-byte 8))`.

You can dispatch to a standard function when you want full control of the request processing, but most of the time it's recommended to call function `FOO.LISP.VINLAND/HANDLER/SIMPLE:DEFINE-CONTROLLER` to define each route handler. `DEFINE-CONTROLLER` does a few things:

* dynamically binds some special variables accessible from handler callbacks
* allows returning one of several return types from handler callbacks
* "fails early" by returning the semantically correct HTTP response status code for a variety of conditions

Special variables dynamically bound from a handler defined with `DEFINE-CONTROLLER` include:

* `FOO.LISP.VINLAND:*REQUEST*`: a `LACK/REQUEST:REQUEST` struct representing the parsed request
* `FOO.LISP.VINLAND:*RESPONSE*`: a `LACK/RESPONSE:RESPONSE` struct representing the response to return to the client
* `FOO.LISP.VINLAND:*ORIGIN*`: a string, like `http://www.acme.example.com`, representing the server origin
* `FOO.LISP.VINLAND:*ROUTE*`: the symbol of the current route-handler
* `FOO.LISP.VINLAND:*BINDING*`: an association list containing any URL bindings (parameters for the matched path pattern)

Route handlers can return any of the following types:

* Clack response list (3 or 2 item list)
* nil
* string
* (vector (unsigned-byte 8))

When a Clack response list is returned, that list represents the response to return to the client, just like when a standard function is used as a route handler. Otherwise, the value of `FOO.LISP.VINLAND:*RESPONSE*` is serialized to a Clack response list; when a string or vector is returned, the return value is set as the response body.

Using `DEFINE-CONTROLLER`, the HTTP errors returned "for free" include:

* 400: the request body could not be parsed
* 405: the request HTTP method is not handled by the controller (but the method is recognized by the server)
* 406: for safe HTTP methods (GET, HEAD), the request Accept header is incompatible with the controller
* 413: the request Content-Length header (referencing the size of the request body) exceeds the maximum configured for the controller
* 415: for unsafe HTTP methods that may take a request body (POST, PUT, PATCH), the request Content-Type header is incompatible with the controller
* 501: the HTTP request method is unrecognized, not handled by the server

In addition, `DEFINE-CONTROLLER` provides automatic handling of OPTIONS requests when `:OPTIONS` is included in keyword parameter `:METHOD`.

Let's start looking at controller definitions.

```common-lisp
(defun cache-control ()
  (set-response-headers :cache-control "private, no-store, must-revalidate"))

(define-controller 'root
  :method '(:HEAD :GET :OPTIONS)
  :before  (list #'cache-control #'require-no-login)
  :GET (lambda ()
         (redirect (route-path 'login)))
  :export t
  :documentation "The root resource redirects to the current user's lists collection resource when
there is a current session, otherwise redirects to the sign-in page")

(define-controller 'login
  :method '(:HEAD :GET :OPTIONS)
  :provide "text/html"
  :before (list #'cache-control #'require-no-login)
  :GET (lambda ()
         (set-response-headers :content-type "text/html")
         (todo-app/view:login))
  :export t
  :documentation "Sign-in page")
```

In the example above, the handlers for the root route and login route (the base path "/" and path "/login", respectively; see the `*ROUTER*` definition) allow for HTTP request methods HEAD, GET, or OPTIONS. When a request is received with a recognized HTTP method not belonging to this list, such as POST, PUT, PATCH, or DELETE, then a 405 HTTP response is returned.

Keyword parameter `EXPORT` (default: NIL) determines whether to export the route symbol. If you use the project structure from the skeleton, you'll want to set `:export t`; otherwise, if you keep your router definition and controllers in the same package, it's not necessary to export the symbols.

The `BEFORE` keyword parameter allows passing a list of functions to call before calling the request method callback; this is useful for running some logic common between method handlers, or running some authorization logic, such as testing that the user is authorized to access the route. Macro `HALT` can be called from a before handler or the method handler to return early from a controller.

Similarly the `AFTER` keyword parameter allows for calling functions after running the request method callback; the return values of any functions specified in `AFTER` is ignored, and this option is meant primarily to provide a way to cleanup any temporary resources that may have been created earlier by the request handler.

The `PROVIDE` keyword parameter, when given, is checked before processing requests for safe HTTP methods, i.e. `HEAD` and `GET`. It can be either a string or a list of strings, each being a media-type that would be found in the request Accept header. When there is no overlap between the value of `PROVIDE` and the request Accept header, a 406 response is returned. In the example above, if a request to the login route does not match "text/html" (which could be "text/*" or "\*/\*"), then a 406 response is returned.

Keyword parameter `ACCEPT` is similar to `PROVIDE`, but checks the request Content-Type header when the request is of an unsafe HTTP method that take a request body, i.e. POST, PUT, or PATCH. When the request Content-Type does not match the given string or list of strings, a 415 response is returned.

Other notable aspects related to the above snippet:

* the `REDIRECT` macro should be used for HTTP redirection (3xx) response codes
* response headers can be set with `SET-RESPONSE-HEADERS`
* function documentation for the controller is defined with the `DOCUMENTATION` keyword parameter
* the login GET handler returns a string, which is used as the response body

Let's look an example of a controller that processes POST requests and specifies the `ACCEPT` parameter to faily early with 415 when an request HTTP method other than POST or OPTIONS is received:

```common-lisp
(define-controller 'create-login
  :method '(:POST :OPTIONS)
  :accept "application/x-www-form-urlencoded"
  :before (list #'cache-control #'require-no-login)
  :POST (lambda ()
          (block nil
            (flet ((fail (&key message username)
                     (declare (type (or html-safe string) message)
                              (type (or null string) username))
                     (return
                       (negotiate
                        ("text/vnd.turbo-stream.html"
                         (flash-now :error message)
                         (render :status :unprocessable-content
                                 :headers '(:content-type "text/vnd.turbo-stream.html")
                                 :view #'todo-app/turbo:login/failure
                                 :args `(:username ,username)))
                        ("text/html"
                         (redirect (route-path 'login)
                                   :flash `(:error ,message)))))))
              (let ((invalid-credential-message "Username/password is invalid"))
                (trivia:ematch
                 (validate-params (body-params))
                 ((list :ok (trivia:plist :username username
                                          :password password))
                  (trivia:ematch
                   (todo-app/store:find-user-by-username username)
                   ((list :ok (list :user user))
                    (unless (bcrypt:password= password
                                              (todo-app/dao:hashed-password
                                               user))
                      (fail :message invalid-credential-message))
                    (set-session-options '(:change-id t :new-session t :expire nil))
                    (set-session :user (make-hash
                                        :initial-contents
                                        (list :id (todo-app/dao:user-id user)
                                              :username username
                                              :time (get-universal-time))))
                    (let ((example-cookie-config (list
                                                  :value (com.inuoe.jzon:stringify
                                                          #{"foo" "bar"
                                                          "baaz" #{"quux" "foobar"}
                                                          "quux" (1 2 3)}
                                                          :stream nil
                                                          :pretty nil)
                                                  :path "/"
                                                  :domain "localhost"
                                                  :expires 1200
                                                  :httponly t
                                                  :secure nil
                                                  :samesite :strict)))
                      (set-cookies `(("_example" . ,example-cookie-config)
                                     ("_foo" . (:value "ok"
                                                :path "/"
                                                :httponly t
                                                :secure nil
                                                :samesite :strict)))))
                    (redirect (if (session :origin)
                                  (session :origin)
                                  (route-path 'todo-lists))
                              :status 303
                              :flash '(:success "Sign-in successful")))
                   ((list :error :not-found)
                    ;; Naively simulate time it takes to run bcrypt iterations.
                    (sleep 0.2)
                    (fail :message invalid-credential-message))))
                 ((list :error (list :validation
                                     (trivia:plist
                                      :unpermitted-keys unpermitted-keys
                                      :missing-keys missing-keys
                                      :invalid-keys invalid-keys)))
                  (when unpermitted-keys
                    (fail :message "Unpermitted parameters detected. Stop."))
                  (when missing-keys
                    (fail :message (format nil "Request is missing the following parameters: ~{~A ~}"
                                           missing-keys)))
                  (when invalid-keys
                    (let ((error-messages (remove nil (collect-params invalid-keys
                                                                      '("username"
                                                                        "password")))))
                      (fail :message (html-safe
                                      (format nil "~{<p>~A</p>~}" error-messages)))))))))))
  :export t
  :documentation "Given a valid username-password combination, creates a user session and redirects to /lists")
```

The route handler above is responsible for processing form submissions to authenticate a user, and when successful, setting some session data and non-session cookies and redirecting.

Notable aspects:

* Macro `NEGOTIATE` is used to negotiate the media-type by checking the request Accept header; the Accept header is traversed, running the body of the first matching clause. In the example above, a Hotwire Turbo document (custom element response) is returned if the request includes "text/vnd.turbo-stream.html"; otherwise, the response falls back to a redirection response.
* A flash message can be set from function `REDIRECT`
* Macro `FLASH-NOW` can be used to set flash data for the current request (and does not store the flash data in the session for the next request)
* Macro `RENDER` can set the HTTP response status code, response headers, and the response body
* Macro `VALIDATE-PARAMS` can be used to call a specialized function to validate parameters for current the route handler, defined in params.lisp: see [params.lisp](https://github.com/lisplizards/vinland-todo-app/blob/master/src/params.lisp) from the todo-app
* Macro `SET-SESSION` can be used to set or update a key in the session hash-table
* Macro `SESSION` can be used to retrieve the value for a key from the session hash-table
* Macro `SET-SESSION-OPTIONS` can be used to set `LACK.SESSION.OPTIONS` in the ENV
* Macro `SET-COOKIES` can be used to add Set-Cookie headers to the response

See Vinland package [FOO.LISP.VINLAND/WEB](https://github.com/lisplizards/vinland/blob/master/src/web.lisp) for macros and functions that can be called from route handlers.

### Path generators

A route generator function is generated for each route when the router is compiled, and the generator functions are dynamically bound on route dispatch. Macro `ROUTE-PATH` is used to call the path-generator functions. For example, `(route-path 'login)` returns "/login". A path with dynamic components (URL binding parameters) can be generated like `(route-path 'todo-item :|todo-list| "xyz" :|todo-item| "abc")`, where the keyword parameters match the dynamic components in the path pattern string; this example would return "/lists/xyz/items/abc". It's also possible to return URLs, which concatenates `*ORIGIN*` and the result of `ROUTE-PATH`, for example `(route-url 'todo-item :|todo-list| "xyz" :|todo-item| "abc")` would return "http://www.acme.example.com/lists/xyz/items/abc" if the `*ORIGIN*` is set to "http://www.acme.example.com".

### Error response customization

Vinland applications include an [errors middleware](https://github.com/lisplizards/lack-middleware-errors) that handles all errors and returns an HTTP error response based on the type of condition signalled, either rendering a static file or calling a handler function. The request is modified and passed to a Vinland controller dedicated to rendering error responses so that all of the normal special variables are bound when rendering HTTP error responses. File [http-error.lisp](https://github.com/lisplizards/vinland-todo-app/blob/master/src/http-error.lisp) included in the skeleton defines special variables [passed to the errors middleware](https://github.com/lisplizards/vinland-todo-app/blob/master/src/app.lisp). There are two types of errors applications currently included with Vinland, [basic](https://github.com/lisplizards/vinland/blob/master/src/errors-app/simple/basic.lisp) and [dynamic-override](https://github.com/lisplizards/vinland/blob/master/src/errors-app/simple/dynamic-override.lisp). In production, it is highly recommended to serve static files from a CDN or reverse proxy instead of Lack middleware.

In addition, the [debug middleware](https://github.com/lisplizards/lack-middleware-debug) can be included in the app.lisp Lack Builder middleware pipeline in development, so that when a condition is signalled, a debug page is rendered that displays the type of condition, the backtrace, any specified special variables, and the Lack ENV.

## Next steps

For further documentation, you can see inspect symbol documentation and study the source code of [Vinland](https://github.com/lisplizards/vinland) and the [To Do app](https://github.com/lisplizards/vinland-todo-app). In particular, you'll want to be familiar with package [FOO.LISP.VINLAND/WEB](https://github.com/lisplizards/vinland/blob/master/src/web.lisp), which defines a number of symbols not covered in this quickstart guide.

You're also welcome to email the author or open an issue on GitHub should you have any unanswered questions.
