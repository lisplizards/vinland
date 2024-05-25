(in-package #:<% @var name %>/json)

(make-hash:install-hash-reader ())

(defun root ()
  (com.inuoe.jzon:stringify
   (make-hash
    :initial-contents '("hello" "world"))
   :stream nil
   :pretty nil))
