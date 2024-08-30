(in-package #:<% @var name %>/layout)

(defparameter *importmap*
  (foo.lisp.vinland/web:importmap
   '(<%- @unless skip-shoelace %>
     ("@shoelace-style/form" . "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/utilities/form.js")
     <%- @endunless %>
     <%- @unless skip-hotwire %>
     ("@hotwired/turbo" . "https://cdn.jsdelivr.net/npm/@hotwired/turbo@8/+esm")
     ("@hotwired/stimulus" . "https://cdn.jsdelivr.net/npm/@hotwired/stimulus/+esm")
     <%- @endunless %>)))

(defmacro with-main-layout ((&key title
                               links
                               scripts
                               skip-main-js
                               main-container-class)
                            &body body)
  `(spinneret:with-html-string (:doctype)
     (:html
      (:head
       (:meta :name "charset" :content "UTF-8")
       (:meta :name "viewport" :content "width=device-width,initial-scale=1")
       <%- @unless skip-hotwire %>
       (:meta :name "turbo-refresh-method" :content "morph")
       <%- @endunless %>
       (:title ,title)
       <%- @unless skip-shoelace %>
       (:link :rel "stylesheet" :href "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.1/cdn/themes/light.css")
       <%- @endunless %>
       (:link :rel "stylesheet" :type "text/css" :href "/css/main.css")
       ,@(when links
           (loop for link in links
                 collect link))
       (:script :type "importmap" (:raw ,*importmap*))
       <%- @unless skip-shoelace %>
       (:script :type "module"
                :src "https://cdn.jsdelivr.net/npm/@shoelace-style/shoelace@2.15.0/cdn/shoelace-autoloader.js"
                :async t)
       <%- @endunless %>
       ,(unless skip-main-js
          '(:script :type "module" :src "/js/main.js"))
       ,@(when scripts
           (loop for script in scripts
                 collect script)))
      (:body
       (<% @var name %>/component:site-header)
       (:div :class (format nil "main-container ~A"
                            ,(or main-container-class ""))
             ,@body)
       (<% @var name %>/component:site-footer)))))
