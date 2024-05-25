(in-package #:<% @var name %>/web)

(defparameter *router*
  (foo.lisp.raven:compile-router
   `(("/" ,'root))
   :fast-dispatch #-ecl t
                  #+ecl nil))

(defparameter *web* (funcall *router* :clack))
