(in-package #:foo.lisp.vinland/static)

(defun generate-error-files (destination file-types static-errors &key dry-run)
  "Iterates over association list STATIC-ERRORS, writing error files to directory DESTINATION.
Checks association list FILE-TYPES to determine file-type extensions for the media-type keys
defined in STATIC-ERRORS.

Options:
* DESTINATION: root directory; a pathname
* FILE-TYPES: association list: for each cons pair, the key is a media-type string and the datum is a file-type extension string
* STATIC-ERRORS: association list: for each cons pair, the key is a media-type string and the datum is an association list with
                 each key as the HTTP response error code (integer) and the datum as a symbol naming a function that returns a
                 response body.
* DRY-RUN: boolean that determines whether to actually write the files to DESTINATION"
  (check-type destination pathname)
  (assert (uiop:directory-pathname-p destination)
          nil
          "DESTINATION must be a directory")
  (check-type file-types list)
  (check-type static-errors list)
  (check-type dry-run boolean)
  (when dry-run
    (warn "GENERATE-ERROR-FILES called with DRY-RUN mode. Not writing to the filesystem."))
  (dolist (media-type-files-pair static-errors)
    (check-type media-type-files-pair cons)
    (destructuring-bind (media-type . files)
        media-type-files-pair
      (check-type media-type string)
      (check-type files list)
      (let ((extension (cdr (assoc media-type file-types :test #'string=))))
        (check-type extension string)
        (dolist (pair files)
          (destructuring-bind (response-code . handler-sym)
              pair
            (check-type response-code foo.lisp.http-response:status-code-error)
            (check-type handler-sym symbol)
            (assert (fboundp handler-sym))
            (let ((html-content (funcall (symbol-function handler-sym))))
              (assert (stringp html-content)
                      nil
                      "HTML-CONTENT is NIL for handler: ~A" handler-sym)
              (check-type html-content string)
              (let ((output-path (merge-pathnames (make-pathname :name (format nil "~D" response-code)
                                                                 :type extension)
                                                  destination)))
                (format t "writing to ~A~%" output-path)
                (unless dry-run
                  (with-open-file (file output-path
                                        :direction :output
                                        :if-exists :supersede
                                        :if-does-not-exist :create)
                    (write-sequence html-content file))))))))))
  (values))
