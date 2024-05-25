;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.vinland.skeleton)

(defvar *web-skeleton-directory*
  (asdf:system-relative-pathname :foo.lisp.vinland.skeleton #p"skeleton/web/"))

(defvar *api-skeleton-directory*
  (asdf:system-relative-pathname :foo.lisp.vinland.skeleton #p"skeleton/api/"))

(defvar *hotwire-overlay-skeleton-directory*
  (asdf:system-relative-pathname :foo.lisp.vinland.skeleton #p"skeleton/overlay/hotwire/"))

(defvar *flavors* '("web" "api"))

(defvar *test-frameworks* '("rove" "parachute"))

(defvar *session-stores* '("memory" "redis-pool"))

(defun make-project (path &rest params &key name long-name program (version "0.1.0")
                                         description long-description author maintainer email
                                         license homepage bug-tracker git-remote source-control
                                         (flavor "web") skip-shoelace skip-hotwire
                                         (session-store "redis-pool") (test-framework "parachute")
                                         (verbose t))
  "Generate a new project at PATH; supports \"web\" and \"api\" flavors.

* NAME: name of the ASDF system and package prefix; defaults to the directory name or last pathname-name; optional.
* LONG-NAME: long name for the ASDF system definition (default: NIL)
* PROGRAM: filename of the binary executable, if different from NAME; optional.
* VERSION: Version of the ASDF system; required (default: \"0.1.0\").
* DESCRIPTION: brief program description; added to ASDF definition, README.md, and the CLI; optional (default: NIL).
* LONG-DESCRIPTION: extended program description; added to README.md and the CLI; optional (default: NIL).
* AUTHOR: the name of the person authoring the project; optional (default: NIL).
* MAINTAINER: the name of the person maintaining the project; optional (default: NIL).
* EMAIL: the email of the project author or maintainer (default: NIL).
* LICENSE: license name, if any; optional. SPDX identifier recommended: https://spdx.org/licenses/ (default: NIL)
* HOMEPAGE: project website URL; optional (default: NIL).
* BUG-TRACKER: project issue tracker URL; optional (default: NIL).
* GIT-REMOTE: Git remote URI, if managed using Git; optional. Alternative to SOURCE-CONTROL (default: NIL).
* SOURCE-CONTROL: source-control identifier; optional. Alternative to GIT-REMOTE (default: NIL).
* FLAVOR: skeleton type, \"web\" or \"api\"; required (default: \"web\").
* SKIP-SHOELACE: whether to not use Shoelace web components if a \"web\" flavor project, boolean (default: NIL).
* SKIP-HOTWIRE: whether to not use Hotwire Turbo and Stimulus if a \"web\" flavor project, boolean (default: NIL).
* SESSION-STORE: session storage adaptor, for web flavor; \"redis-pool\" or \"memory\" (default: \"redis-pool\")
* TEST-FRAMEWORK: which testing framework to use, one of: \"rove\", \"parachute\" (default: \"parachute\").
* VERBOSE: whether to enable verbose output, boolean (default: T).
"

  (check-type path pathname)
  ;; Coerce to a directory pathname (with trailing slash).
  (when (pathname-name path)
    (setq path
          (make-pathname :directory (append (pathname-directory path)
                                            (list (pathname-name path))))))
  (let* ((last-directory (car (last (pathname-directory path))))
         (name (or name last-directory)))
    (assert (and (stringp name)
                 (> (length name) 0))
            nil
            "NAME must be a non-empty string")
    (setf (getf params :name)
          name)
    (unless program
      (setf (getf params :program)
            name
            program
            name))
    (assert (and (stringp program)
                 (> (length program) 0))
            nil
            "PROGRAM is required and must be a non-empty string")
    (assert (and (not (position #\/ program))
                 (not (position #\\ program)))
            nil
            "PROGRAM is the binary filename and must not include path dividers"))
  (assert (and (stringp version)
               (> (length version) 0))
          nil
          "VERSION is required and must be a non-empty string")
  (check-type version (or null string))
  (check-type description (or null string))
  (check-type long-description (or null string))
  (check-type author (or null string))
  (check-type maintainer (or null string))
  (check-type email (or null string))
  (check-type license (or null string))
  (check-type homepage (or null string))
  (check-type bug-tracker (or null string))
  (check-type git-remote (or null string))
  (check-type source-control (or null string))
  (check-type verbose boolean)
  (check-type skip-hotwire boolean)
  (check-type skip-shoelace boolean)
  (assert (not (and source-control git-remote))
          nil
          "Specify either SOURCE-CONTROL or GIT-REMOTE or neither, not both")
  (assert (not (and skip-hotwire (string= flavor "api")))
          nil
          "SKIP-HOTWIRE invalid option for \"api\" flavor project")
  (assert (not (and skip-shoelace (string= flavor "api")))
          nil
          "SKIP-SHOELACE invalid option for \"api\" flavor project")
  (let ((original-flavor flavor)
        (flavor (string-downcase flavor)))
    (assert (member flavor *flavors* :test #'equal)
            nil
            "Unknown flavor (~A). Expected one of: ~{~A~^ ~}"
            original-flavor
            *flavors*)
    (let ((test-framework-original test-framework)
          (test-framework-downcased (string-downcase test-framework)))
      (assert (member test-framework *test-frameworks* :test #'equal)
              nil
              "Unknown test-framework (~A). Expected one of: ~{~A~^ ~}" test-framework *test-frameworks*)
      (cond
        ((string= "parachute" test-framework-downcased)
         (setf (getf params :parachute) t))
        ((string= "rove" test-framework-downcased)
         (setf (getf params :rove) t))))
    (let ((session-store-downcased (string-downcase session-store)))
      (assert (member session-store *session-stores* :test #'equal)
              nil
              "Unknown session-store (~A). Expected one of: ~{~A~^ ~}" session-store-original *session-stores*)
      (cond
        ((string= "redis-pool" session-store-downcased)
         (setf (getf params :session-store-redis-pool) t))
        ((string= "memory" session-store-downcased)
         (setf (getf params :session-store-memory) t))))
    (let ((cl-project:*skeleton-directory* (cond
                                             ((string= "web" flavor) *web-skeleton-directory*)
                                             ((string= "api" flavor) *api-skeleton-directory*)
                                             (t
                                              (error "Invalid skeleton flavor: ~A" flavor)))))
      (apply #'cl-project:make-project path params))
    (when (string= "web" flavor)
      (unless skip-hotwire
        (let ((cl-project:*skeleton-directory* *hotwire-overlay-skeleton-directory*))
          (apply #'cl-project:make-project path params)))
      (let ((favicon-src (asdf:system-relative-pathname :foo.lisp.vinland.skeleton
                                                        #p"skeleton/favicon.ico"))
            (favicon-target (merge-pathnames (make-pathname
                                              :directory (append (pathname-directory path)
                                                                 (list "static")))
                                             #p"favicon.ico")))
        (format t "writing ~A~%" favicon-target)
        (uiop:copy-file favicon-src favicon-target))
      (let ((lizard-logo-src (asdf:system-relative-pathname
                              :foo.lisp.vinland.skeleton
                              #p"skeleton/lisp-lizard-logo300x100.jpg"))
            (lizard-logo-target (merge-pathnames (make-pathname
                                                  :directory (append (pathname-directory path)
                                                                     (list "static" "images")))
                                                 #p"lisp-lizard-logo300x100.jpg")))
        (format t "writing ~A~%" lizard-logo-target)
        (uiop:copy-file lizard-logo-src lizard-logo-target)))
    (warn "Don't forget to add a license file to your project if you intend to make it publicly available.~%   You may also consider adding a license header to each file.~%   See: https://spdx.org/licenses/~%")
    (format t "~%Grab your sword and shield: your saga starts here :)~%~%~%Run tests: (asdf:test-system :~A)~%~%If using Quicklisp, run the following from the REPL to load the system and start the server:~%~A(ql:quickload '(\"~A\" \"clack\"))~%~A(clack:clackup ~A/app:*app* :server :hunchentoot :port 5000 :use-default-middlewares nil)~%~%" (getf params :name) (string #\Tab) (getf params :name) (string #\Tab) (getf params :name))
    t))
