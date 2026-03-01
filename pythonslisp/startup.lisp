;;; Python's Lisp — system startup script
;;; Loaded automatically by LispInterpreter.reboot() after the standard library.
;;; Contains system-level initialization that is not part of the reusable library.

;;; Adjust Python's recursion limit for deep Lisp recursion
(let ((newLimit 10000))
   (if (< (recursion-limit) newLimit)
       (if (recursion-limit newLimit)
           (writef "- Recursion limit increased to {0:,d}\n" (list newLimit))
           (uwriteln! "- Failed to increase recursion limit."))
       nil))

;;; Standard CL stream variables
(setf *standard-input*  (stdin))
(setf *standard-output* (stdout))
(setf *error-output*    (stderr))
(setf *terminal-io*     (stdout))
(setf *debug-io*        (stderr))
(setf *query-io*        (stdout))
(setf *trace-output*    (stdout))

;;; Stream predicate aliases (backward compatibility)
(alias readable  input-stream-p)
(alias writable  output-stream-p)
(alias isatty    interactive-stream-p)

(defun closed (s)
   "Returns T if the stream is closed, NIL if it is open."
   (not (open-stream-p s)))

;;; CL file and string stream macros

(defmacro with-open-file (spec &rest body)
   "Opens a file, binds it to var, evaluates body forms, then closes the file.
spec is (var filespec &rest open-options) where open-options are keyword args
passed directly to open.  Default direction is :input.
Returns the value of the last body form.
Note: file is closed normally; on error the file may remain open (no unwind-protect)."
   (let ((var      (car spec))
         (filespec (car (cdr spec)))
         (options  (cdr (cdr spec))))
      `(let ((,var (open ,filespec ,@options)))
          (let ((_wof_result_ (progn ,@body)))
             (when ,var (close ,var))
             _wof_result_))))

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

;;; Online help system welcome message
(uwriteln! "- For LOHS (lisp online help system) type \"(help)\" to begin.")
