(in-package #:<% @var name %>/controller)

(define-controller 'root
  :method '(:HEAD :GET :OPTIONS)
  :before ()
  :provide "application/json"
  :GET (lambda ()
         (declare (optimize (speed 0) (safety 3) (debug 3)))
         (set-response-headers :content-type "application/json")
         (<% @var name %>/json:root))
  :export t
  :documentation "Base path resource")
