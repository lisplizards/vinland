(in-package #:<% @var name %>/view)

(defun root ()
  (with-main-layout (:title "<% @var name %> | Vinland app"
                     :main-container-class "center")
    (<% @var name %>/component:flash-container :class "fadein")
    (:div
     (:h1 "<% @var name %>")
     (:p (:em "Vinland app")))
    (:img :src "/images/lisp-lizard-logo300x100.jpg" :alt "Lisp lizard" :class "lisp-lizard-logo")))
