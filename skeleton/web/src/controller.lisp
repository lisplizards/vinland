(in-package #:<% @var name %>/controller)

(make-hash:install-hash-reader ())

(define-controller 'root
  :method '(:HEAD :GET :OPTIONS)
  :before ()
  :provide "text/html"
  :GET (lambda ()
         (declare (optimize (speed 0) (safety 3) (debug 3)))
         (flash-now :notice "Your saga starts here.")
         (render :headers '(:content-type "text/html")
                 :view #'<% @var name %>/view:root))
  :export t
  :documentation "Base path resource")
