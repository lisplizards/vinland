(in-package #:cl-user)

<%- @if rove %>
(defpackage #:<% @var name %>/config/tests
  (:use #:cl #:rove #:<% @var name %>/config))

(defpackage #:<% @var name %>/dao/tests
  (:use #:cl #:rove #:<% @var name %>/dao))

(defpackage #:<% @var name %>/store/tests
  (:use #:cl #:rove #:<% @var name %>/store))

(defpackage #:<% @var name %>/component/tests
  (:use #:cl #:rove #:<% @var name %>/component))

(defpackage #:<% @var name %>/layout/tests
  (:use #:cl #:rove #:<% @var name %>/layout))

(defpackage #:<% @var name %>/http-error/tests
  (:use #:cl #:rove #:<% @var name %>/http-error))

(defpackage #:<% @var name %>/view/tests
  (:use #:cl #:rove #:<% @var name %>/view))

<%- @unless skip-hotwire %>
(defpackage #:<% @var name %>/turbo/tests
  (:use #:cl #:rove #:<% @var name %>/turbo))
<%- @endunless %>

(defpackage #:<% @var name %>/user/tests
  (:use #:cl #:rove #:<% @var name %>/user))

(defpackage #:<% @var name %>/controller/tests
  (:use #:cl #:rove #:<% @var name %>/controller))

(defpackage #:<% @var name %>/params/tests
  (:use #:cl #:rove #:<% @var name %>/params))

(defpackage #:<% @var name %>/web/tests
  (:use #:cl #:rove #:<% @var name %>/web))

(defpackage #:<% @var name %>/app/tests
  (:use #:cl #:rove #:<% @var name %>/app))

(defpackage #:<% @var name %>/cli/tests
  (:use #:cl #:rove #:<% @var name %>/cli))
<%- @endif %>
<%- @if parachute %>
(defpackage #:<% @var name %>/config/tests
  (:use #:cl #:parachute #:<% @var name %>/config))

(defpackage #:<% @var name %>/dao/tests
  (:use #:cl #:parachute #:<% @var name %>/dao))

(defpackage #:<% @var name %>/store/tests
  (:use #:cl #:parachute #:<% @var name %>/store))

(defpackage #:<% @var name %>/component/tests
  (:use #:cl #:parachute #:<% @var name %>/component))

(defpackage #:<% @var name %>/layout/tests
  (:use #:cl #:parachute #:<% @var name %>/layout))

(defpackage #:<% @var name %>/http-error/tests
  (:use #:cl #:parachute #:<% @var name %>/http-error))

(defpackage #:<% @var name %>/view/tests
  (:use #:cl #:parachute #:<% @var name %>/view))

<%- @unless skip-hotwire %>
(defpackage #:<% @var name %>/turbo/tests
  (:use #:cl #:parachute #:<% @var name %>/turbo))
<%- @endunless %>

(defpackage #:<% @var name %>/user/tests
  (:use #:cl #:parachute #:<% @var name %>/user))

(defpackage #:<% @var name %>/controller/tests
  (:use #:cl #:parachute #:<% @var name %>/controller))

(defpackage #:<% @var name %>/params/tests
  (:use #:cl #:parachute #:<% @var name %>/params))

(defpackage #:<% @var name %>/web/tests
  (:use #:cl #:parachute #:<% @var name %>/web))

(defpackage #:<% @var name %>/app/tests
  (:use #:cl #:parachute #:<% @var name %>/app))

(defpackage #:<% @var name %>/cli/tests
  (:use #:cl #:parachute #:<% @var name %>/cli))
<%- @endif %>
