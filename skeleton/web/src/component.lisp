(in-package #:<% @var name %>/component)

(defun flash-container (&key (flash-types '(:notice :success :alert :error))
                          (class ""))
  (unless foo.lisp.vinland:*flash*
    (return-from flash-container
      (spinneret:with-html
          (:div :class (format nil "flash-container ~A" (or class ""))))))
  (spinneret:with-html
      (:div :id "flash-container"
            :class (format nil "flash-container ~A" (or class ""))
            <%- @unless skip-hotwire %>
            :data-turbo-temporary t
            <%- @endunless %>
            (mapcar
             #'(lambda (flash-type)
                 (declare (type keyword flash-type))
                 (let ((message (foo.lisp.flash:get-flash flash-type)))
                   (when message
                     (flash :type flash-type
                            :message message))))
             flash-types))))

<%- @if skip-shoelace %>
(defun flash (&key type message)
  (declare (type keyword type)
           (type (or null string foo.lisp.vinland/web:html-safe) message))
  (assert (not (null message))
          nil
          "Flash message is empty")
  (spinneret:with-html
      (flet ((message ()
               (etypecase message
                 (string message)
                 (foo.lisp.vinland/web:html-safe
                  (:raw
                   (foo.lisp.vinland/web:html-safe-value message))))))
        (ecase type
          (:notice
           (:div :class "flash notice"
                 (message)))
          (:success
           (:div :class "flash success"
                 (message)))
          (:alert
           (:div :class "flash alert"
                 (message)))
          (:error
           (:div :class "flash error"
                 (message)))))))
<%- @else %>
(defun flash (&key type message)
  (declare (type keyword type)
           (type (or null string foo.lisp.vinland/web:html-safe) message))
  (assert (not (null message))
          nil
          "Flash message is empty")
  (spinneret:with-html
      (flet ((message ()
               (etypecase message
                 (string message)
                 (foo.lisp.vinland/web:html-safe
                  (:raw
                   (foo.lisp.vinland/web:html-safe-value message))))))
        (ecase type
          (:notice
           (:sl-alert :variant "primary" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "info-circle")
                      (message)))
          (:success
           (:sl-alert :variant "success" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "check2-circle")
                      (message)))
          (:alert
           (:sl-alert :variant "warning" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "exclamation-triangle")
                      (message)))
          (:error
           (:sl-alert :variant "danger" :open t :closable t :duration "3000"
                      (:sl-icon :slot "icon" :name "exclamation-octagon")
                      (message)))))))
<%- @endif %>

(defun csrf-input ()
  (spinneret:with-html
      (:input :type "hidden"
              :class "hidden"
              :autocomplete "off"
              :name lack/middleware/csrf::*csrf-middleware-token*
              :value (foo.lisp.vinland/web:csrf-token))))

(defun site-header ()
  (spinneret:with-html
    (:header
     (:p "Site header"))))

(defun site-footer ()
  (spinneret:with-html
    (:footer
     (:p "Site footer"))))
