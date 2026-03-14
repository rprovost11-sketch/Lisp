; ========================
; struct support
; ------------------------

(defun %struct-field-name (spec)
   "Extract field name from a spec: bare symbol -> symbol; (name default) -> name."
   (if (listp spec) (car spec) spec))

(defun %struct-field-default (spec)
   "Extract default value from a spec: (name default) -> default; bare symbol -> NIL."
   (if (listp spec) (car (cdr spec)) nil))

(defun %struct-name (typename-form)
   "Extract struct name: bare symbol -> itself; (name opts...) -> name."
   (if (listp typename-form) (car typename-form) typename-form))

(defun %struct-find-opt (opts key)
   "Find value for key in a list of (:key val) option pairs, or NIL."
   (cond ((null opts) nil)
         ((and (listp (car opts)) (= (car (car opts)) key))
          (cadr (car opts)))
         (t (%struct-find-opt (cdr opts) key))))

(defun %struct-include (typename-form)
   "Extract :include parent name from (name (:include parent) ...), or NIL."
   (if (not (listp typename-form))
       nil
       (%struct-find-opt (cdr typename-form) ':include)))

(defun %struct-ancestors (parent-name)
   "Return full ancestor chain (parent grandparent ...) by walking descriptors."
   (if (null parent-name)
       nil
       (let ((parent-desc (eval parent-name)))
         (cons parent-name (at 'INCLUDES parent-desc)))))

(defun %struct-parent-fields (parent-name)
   "Return field specs from parent descriptor, or NIL if no parent."
   (if (null parent-name)
       nil
       (at 'FIELDS (eval parent-name))))

(defmacro defstruct (typename-form &rest field-specs)
   "Define a struct type. An optional docstring may appear as the first
element of field-specs. Remaining field-specs are symbols or (name default)
pairs. The typename may be a bare symbol or (name (:include parent)) to
inherit fields from an existing struct type.
Generates: make-<typename>, <typename>-p, <typename>-<field> accessors,
setf support for accessors, and copy-<typename>.
Binds <typename> in the global namespace to a struct descriptor."
   (let* ((typename        (%struct-name typename-form))
          (include-name    (%struct-include typename-form))
          (ancestors       (%struct-ancestors include-name))
          (parent-fields   (%struct-parent-fields include-name))
          (doc-string      (if (and field-specs (stringp (car field-specs)))
                               (car field-specs)
                               nil))
          (own-specs       (if doc-string (cdr field-specs) field-specs))
          (all-specs       (append parent-fields own-specs))
          (own-names       (mapcar %struct-field-name own-specs))
          (all-names       (mapcar %struct-field-name all-specs))
          (make-name       (make-symbol (ustring "MAKE-" typename)))
          (pred-name       (make-symbol (ustring typename "-P")))
          (copy-name       (make-symbol (ustring "COPY-" typename)))
          (key-params      (mapcar (lambda (spec)
                                      (list (%struct-field-name spec)
                                            (%struct-field-default spec)))
                                   all-specs))
          (map-entries     (mapcar (lambda (fname)
                                      (list fname fname))
                                   all-names))
          (copy-entries    (mapcar (lambda (fname)
                                      (list fname
                                            (list 'at (list 'quote fname) 'old)))
                                   all-names))
          (acc-forms       (mapcar (lambda (fname)
                                      (list 'defun
                                            (make-symbol (ustring typename "-" fname))
                                            '(inst)
                                            (list 'at (list 'quote fname) 'inst)))
                                   own-names))
          (setf-forms      (mapcar (lambda (fname)
                                      (list 'defsetf-internal
                                            (list 'quote (make-symbol (ustring typename "-" fname)))
                                            (list 'quote fname)))
                                   own-names))
          (field-desc-pairs (mapcar (lambda (spec)
                                       (list 'list
                                             (list 'quote (%struct-field-name spec))
                                             (list 'quote (%struct-field-default spec))))
                                    all-specs)))
      `(progn
         (defun ,make-name (&key ,@key-params)
            (make-dict (STRUCT-TYPE ',typename)
                       ,@(if ancestors
                             (list (list 'STRUCT-INCLUDES (list 'quote ancestors)))
                             nil)
                       ,@map-entries))
         (defun ,pred-name (obj)
            (and (dictp obj)
                 (or (= (at 'STRUCT-TYPE obj) ',typename)
                     (if (member ',typename (ignore-errors (at 'STRUCT-INCLUDES obj))) t nil))))
         ,@acc-forms
         ,@setf-forms
         (defun ,copy-name (old)
            (make-dict (STRUCT-TYPE ',typename)
                       ,@(if ancestors
                             (list (list 'STRUCT-INCLUDES (list 'quote ancestors)))
                             nil)
                       ,@copy-entries))
         (setq ,typename
               (make-dict (STRUCT-TYPE '%STRUCT-DESCRIPTOR%)
                          (NAME ',typename)
                          (DOCSTRING ,doc-string)
                          (INCLUDES ',ancestors)
                          (FIELDS (list ,@field-desc-pairs))))
         ',typename)))

