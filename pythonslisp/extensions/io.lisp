;;; Standard CL stream variables
(setq *standard-input*  (stdin))
(setq *standard-output* (stdout))
(setq *error-output*    (stderr))
(setq *terminal-io*     (stdout))
(setq *debug-io*        (stderr))
(setq *query-io*        (stdout))
(setq *trace-output*    (stdout))

;;; Stream predicate aliases (backward compatibility)
(setq readable  input-stream-p)
(setq writable  output-stream-p)
(setq isatty    interactive-stream-p)

(setq closed (lambda (s)
   "Returns T if the stream is closed, NIL if it is open."
   (not (open-stream-p s))))

;;; CL file and string stream macros

(defmacro with-open-file (spec &rest body)
   "Opens a file, binds it to var, evaluates body forms, then closes the file.
spec is (var filespec &rest open-options) where open-options are keyword args
passed directly to open.  Default direction is :input.
Returns the value of the last body form.
File is always closed - safe on throw, return-from, or error."
   (let ((var      (car spec))
         (filespec (car (cdr spec)))
         (options  (cdr (cdr spec))))
      `(let ((,var (open ,filespec ,@options)))
          (unwind-protect
             (progn ,@body)
             (when ,var (close ,var))))))

(defmacro with-output-to-string (var-spec &rest body)
   "Creates a string output stream, evaluates body forms with the first element
of var-spec bound to the stream, then returns the accumulated string content."
   (let ((var (car var-spec)))
      `(let ((,var (make-string-output-stream)))
          ,@body
          (get-output-stream-string ,var))))

(defmacro with-input-from-string (var-spec &rest body)
   "Creates a string input stream from string, binds it to var, evaluates body
forms, closes the stream, then returns the result of the last body form.
var-spec is (var string), (var string start), or (var string start end)."
   (let ((var    (car var-spec))
         (string (car (cdr var-spec)))
         (rest   (cdr (cdr var-spec))))
      `(let ((,var (make-string-input-stream ,string ,@rest)))
          (let ((_wifs_result_ (progn ,@body)))
             (when ,var (close ,var))
             _wifs_result_))))

;;; Interactive I/O

(setq read-prompt (lambda (promptStr)
   "Prompt the user for input and return the user input as a string."
   (write! promptStr)
   (read-line)))
