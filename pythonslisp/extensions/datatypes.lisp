; ========================
; struct support
; ------------------------

(defun %struct-field-name (spec)
   "Extract field name from a spec: bare symbol -> symbol; (name default) -> name."
   (if (listp spec) (car spec) spec))

(defun %struct-field-default (spec)
   "Extract default value from a spec: (name default) -> default; bare symbol -> NIL."
   (if (listp spec) (car (cdr spec)) nil))

(defmacro defstruct (typename &rest field-specs)
   "Define a struct type. field-specs are symbols or (name default) pairs.
Generates: make-<typename>, <typename>-p, <typename>-<field> accessors,
setf support for accessors, and copy-<typename>."
   (let* ((field-names   (mapcar %struct-field-name field-specs))
          (make-name     (make-symbol (ustring "MAKE-" typename)))
          (pred-name     (make-symbol (ustring typename "-P")))
          (copy-name     (make-symbol (ustring "COPY-" typename)))
          (key-params    (mapcar (lambda (spec)
                                    (list (%struct-field-name spec)
                                          (%struct-field-default spec)))
                                 field-specs))
          (map-entries   (mapcar (lambda (fname)
                                    (list fname fname))
                                 field-names))
          (copy-entries  (mapcar (lambda (fname)
                                    (list fname
                                          (list (make-symbol (ustring typename "-" fname)) 'old)))
                                 field-names))
          (acc-forms     (mapcar (lambda (fname)
                                    (list 'defun
                                          (make-symbol (ustring typename "-" fname))
                                          '(inst)
                                          (list 'at (list 'quote fname) 'inst)))
                                 field-names))
          (setf-forms    (mapcar (lambda (fname)
                                    (list 'defsetf-internal
                                          (list 'quote (make-symbol (ustring typename "-" fname)))
                                          (list 'quote fname)))
                                 field-names)))
      `(progn
         (defun ,make-name (&key ,@key-params)
            (make-dict (STRUCT-TYPE ',typename) ,@map-entries))
         (defun ,pred-name (obj)
            (and (dictp obj) (= (at 'STRUCT-TYPE obj) ',typename)))
         ,@acc-forms
         ,@setf-forms
         (defun ,copy-name (old)
            (make-dict (STRUCT-TYPE ',typename) ,@copy-entries))
         ',typename)))

