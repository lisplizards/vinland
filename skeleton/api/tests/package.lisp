(in-package #:cl-user)

<%- @if rove %>
(defpackage #:<% @var name %>/tests/config
  (:use #:cl #:rove #:<% @var name %>/config))

(defpackage #:<% @var name %>/tests/dao
  (:use #:cl #:rove #:<% @var name %>/dao))

(defpackage #:<% @var name %>/tests/store
  (:use #:cl #:rove #:<% @var name %>/store))

(defpackage #:<% @var name %>/tests/json
  (:use #:cl #:rove #:<% @var name %>/json))

(defpackage #:<% @var name %>/tests/user
  (:use #:cl #:rove #:<% @var name %>/user))

(defpackage #:<% @var name %>/tests/controller
  (:use #:cl #:rove #:<% @var name %>/controller))

(defpackage #:<% @var name %>/tests/params
  (:use #:cl #:rove #:<% @var name %>/params))

(defpackage #:<% @var name %>/tests/web
  (:use #:cl #:rove #:<% @var name %>/web))

(defpackage #:<% @var name %>/tests/app
  (:use #:cl #:rove #:<% @var name %>/app))

(defpackage #:<% @var name %>/tests/cli
  (:use #:cl #:rove #:<% @var name %>/cli))
<%- @endif %>
<%- @if parachute %>
(defpackage #:<% @var name %>/tests/config
  (:use #:cl #:parachute #:<% @var name %>/config))

(defpackage #:<% @var name %>/tests/dao
  (:use #:cl #:parachute #:<% @var name %>/dao))

(defpackage #:<% @var name %>/tests/store
  (:use #:cl #:parachute #:<% @var name %>/store))

(defpackage #:<% @var name %>/tests/json
  (:use #:cl #:parachute #:<% @var name %>/json))

(defpackage #:<% @var name %>/tests/user
  (:use #:cl #:parachute #:<% @var name %>/user))

(defpackage #:<% @var name %>/tests/controller
  (:use #:cl #:parachute #:<% @var name %>/controller))

(defpackage #:<% @var name %>/tests/params
  (:use #:cl #:parachute #:<% @var name %>/params))

(defpackage #:<% @var name %>/tests/web
  (:use #:cl #:parachute #:<% @var name %>/web))

(defpackage #:<% @var name %>/tests/app
  (:use #:cl #:parachute #:<% @var name %>/app))

(defpackage #:<% @var name %>/tests/cli
  (:use #:cl #:parachute #:<% @var name %>/cli))
<%- @endif %>
