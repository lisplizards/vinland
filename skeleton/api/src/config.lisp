(in-package #:<% @var name %>/config)

(defvar *system-directory* (asdf:system-source-directory :<% @var name %>))

(defvar *static-errors-directory* (merge-pathnames #p"static/errors/" *system-directory*))
