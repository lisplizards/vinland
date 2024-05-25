(in-package #:<% @var name %>/params)

(defun string-present-p (val)
  (and (stringp val)
       (> (length val) 0)))

(defun validation-error->list (condition)
  `(:error
    (:validation
     (:unpermitted-keys ,(unpermitted-keys condition)
      :missing-keys ,(missing-keys condition)
      :invalid-keys ,(invalid-keys condition)))))

(defmethod %validate-params ((route (eql '<% @var name %>/controller:root))
                            (request-method (eql :GET))
                            params)
  (declare (ignore route request-method)
           (type symbol route)
           (type list params))
  (handler-case (let ((params
                        (validate
                         (alist
                          (requires "hello")
                          (satisfies "hello"
                                     #'string-present-p
                                     :message "hello is required"))
                         params)))
                  `(:ok (:params ,params)))
    (validation-error (condition)
      (validation-error->list condition))))
