;;; listener.lisp — Pure Lisp REPL Listener for Python's Lisp
;;;
;;; To start: load this file (it auto-starts at the bottom), or call (lsl-start)
;;;   From Python listener:  ]readsrc pythonslisp/listener.lisp
;;;   From Lisp:             (eval (load "pythonslisp/listener.lisp"))
;;;
;;; LIMITATIONS:
;;;   - Parse errors (malformed syntax) are NOT caught by handler-case and will
;;;     propagate to the outer Python listener, interrupting this REPL.
;;;     Type (lsl-start) to resume — global state (*lsl-log-stream* etc.) persists.
;;;   - ]reboot exits this REPL and reboots the interpreter.  Reload this file
;;;     or type (lsl-start) to restart the Lisp listener.
;;;   - ]test does not reboot between test files (no per-file env isolation).
;;;   - On Windows, readline line-editing (arrow keys, history scroll) is not
;;;     available inside this REPL; readline-add-history still records history.


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Global state  (re-initialised each time this file is loaded)
;;; ─────────────────────────────────────────────────────────────────────────────

(setf *lsl-log-stream*    nil)
(setf *lsl-instrumenting* nil)
(setf *lsl-hist-max*      500)
(setf *lsl-testdir*       "pythonslisp/testing")


;;; ─────────────────────────────────────────────────────────────────────────────
;;; ANSI colour helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-color-p ()
  "T if stdout is an interactive terminal that supports ANSI colour."
  (interactive-stream-p (stdout)))

(defun %lsl-ansi (code str)
  "Wrap str in ANSI escape when colour is enabled; return str unchanged otherwise."
  (if (%lsl-color-p)
      (string-join "" (list "\033[" code "m" str "\033[0m"))
      str))

(defun %lsl-bold-white (s) (%lsl-ansi "1;97" s))
(defun %lsl-bold-green (s) (%lsl-ansi "1;92" s))
(defun %lsl-green      (s) (%lsl-ansi "92"   s))
(defun %lsl-yellow     (s) (%lsl-ansi "93"   s))
(defun %lsl-cyan       (s) (%lsl-ansi "96"   s))
(defun %lsl-red        (s) (%lsl-ansi "91"   s))
(defun %lsl-dim        (s) (%lsl-ansi "2"    s))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Low-level output helpers
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-println (str)
  "Print str+newline to stdout and (if open) to the session log."
  (uwrite-line str)
  (when *lsl-log-stream*
    (uwrite-line *lsl-log-stream* str)))

(defun %lsl-println-screen (str)
  "Print str+newline to stdout only — not written to the session log."
  (uwrite-line str))

(defun %lsl-log-writeln (str)
  "Write str+newline to the session log only."
  (when *lsl-log-stream*
    (uwrite-line *lsl-log-stream* str)))

(defun %lsl-blank ()
  "Print a blank line to stdout and (if open) to the session log."
  (%lsl-println ""))

(defun %lsl-write-result (str-list)
  "Display a list of pre-stringified return values with '==> ' prefix.
str-list is a list of Lisp strings (already prettyprinted).  Writes to
stdout (colourised) and to the session log (plain)."
  (dolist (vs str-list)
    (let* ((pl (string-join "" (list "\n==> " vs)))
           (cl (string-join "" (list "\n"
                                     (%lsl-bold-green "==>")
                                     " "
                                     (%lsl-bold-white vs)))))
      (uwrite-line (if (%lsl-color-p) cl pl))
      (flush)
      (%lsl-log-writeln pl))))

(defun %lsl-write-error (msg)
  "Display error message lines prefixed with '%%% '. Writes to stdout and log."
  (dolist (line (string-lines msg))
    (let ((pl (string-join "" (list "%%% " line))))
      (uwrite-line (%lsl-red pl))
      (flush)
      (%lsl-log-writeln pl))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; String utilities
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-starts-with (str prefix)
  "T if str begins with prefix."
  (and (>= (length str) (length prefix))
       (eql (search prefix str) 0)))

(defun %lsl-rstrip (str)
  "Remove trailing whitespace from str."
  (string-right-trim " \t\n\r" str))

(defun %lsl-cat (&rest parts)
  "Concatenate strings."
  (string-join "" parts))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Evaluate an input string; return (retval-str output-str err-str secs)
;;; NOTE: ParseError (malformed syntax) is NOT caught — it propagates.
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-eval-expr (input)
  "Evaluate input string with output capture and error handling.
Returns a 4-element list: (retval-string output-string err-string elapsed-secs).
retval-string is the newline-joined prettyprinted values (programmer mode).
ParseError from malformed syntax escapes this function uncaught."
  (let ((out-strm (make-string-output-stream))
        (retval   "")
        (errmsg   "")
        (t0       (get-internal-real-time)))
    (let ((*standard-output* out-strm))
      (handler-case
          (let* ((ast     (parse input))
                 (results (eval-for-display ast)))
            ;; results is a Lisp list of values; stringify each for display/comparison
            (setq retval (string-join "\n" (mapcar string results))))
        (t (e)
           (setq errmsg (condition-message e)))))
    (let* ((elapsed (float (/ (- (get-internal-real-time) t0)
                              (internal-time-units-per-second))))
           (output  (%lsl-rstrip (get-output-stream-string out-strm))))
      (list retval output errmsg elapsed))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Session log parser
;;; Returns a list of (expr output retval errmsg) entries.
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-parse-log (text)
  "Parse session log text into a list of (expr output retval errmsg) 4-lists."
  (let* ((lines  (string-lines text))
         (result nil)
         (rem    lines))

    ;; Skip to the first '>>> ' line
    (while (and rem (not (%lsl-starts-with (car rem) ">>> ")))
      (setq rem (cdr rem)))

    (while rem
      (let ((expr   "")
            (output "")
            (retval "")
            (errmsg ""))

        ;; Parse expression — rem currently points at a '>>> ' line
        (setq expr (%lsl-rstrip (subseq (car rem) 4)))
        (setq rem  (cdr rem))

        ;; Accumulate '... ' continuation lines
        (while (and rem (%lsl-starts-with (car rem) "..."))
          (setq expr (%lsl-cat expr "\n" (subseq (car rem) 4)))
          (setq rem  (cdr rem)))

        ;; Accumulate output lines (everything before '==> ', '%%% ', or '>>> ')
        (while (and rem
                    (not (%lsl-starts-with (car rem) "==> "))
                    (not (string= (%lsl-rstrip (car rem)) "==>"))
                    (not (%lsl-starts-with (car rem) "%%% "))
                    (not (%lsl-starts-with (car rem) ">>> ")))
          (setq output (%lsl-cat output (car rem) "\n"))
          (setq rem    (cdr rem)))

        ;; Parse return value
        (when (and rem
                   (or (%lsl-starts-with (car rem) "==> ")
                       (string= (%lsl-rstrip (car rem)) "==>")))
          (setq retval (if (> (length (car rem)) 4)
                           (subseq (car rem) 4)
                           ""))
          (setq rem (cdr rem))
          ;; Continuation retval lines (until next marker or >>> )
          (while (and rem
                      (not (%lsl-starts-with (car rem) "==> "))
                      (not (string= (%lsl-rstrip (car rem)) "==>"))
                      (not (%lsl-starts-with (car rem) "... "))
                      (not (%lsl-starts-with (car rem) ">>> "))
                      (not (%lsl-starts-with (car rem) "%%% ")))
            (if (%lsl-starts-with (car rem) ";")
                (setq expr   (%lsl-cat expr   "\n" (car rem)))
                (setq retval (%lsl-cat retval "\n" (car rem))))
            (setq rem (cdr rem))))

        ;; Parse error message
        (when (and rem (%lsl-starts-with (car rem) "%%% "))
          (setq errmsg (subseq (car rem) 4))
          (setq rem    (cdr rem))
          (while (and rem (%lsl-starts-with (car rem) "%%% "))
            (setq errmsg (%lsl-cat errmsg (subseq (car rem) 4)))
            (setq rem    (cdr rem))))

        ;; Skip to next '>>> ' line for next iteration
        (while (and rem (not (%lsl-starts-with (car rem) ">>> ")))
          (setq rem (cdr rem)))

        ;; Accumulate entry if non-empty
        (when (not (string= (%lsl-rstrip expr) ""))
          (setq result (cons (list (%lsl-rstrip expr)
                                   (%lsl-rstrip output)
                                   (%lsl-rstrip retval)
                                   errmsg)
                             result)))))

    (reverse result)))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Session log restore
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-restore-log (filename verbosity)
  "Read and replay a session log file.
verbosity 0 = quiet (no expression output), 3 = verbose (show expressions)."
  (handler-case
      (let ((text nil))
        (with-open-file (f filename)
          (setq text (readall f)))
        (dolist (entry (%lsl-parse-log text))
          (let ((expr (at 0 entry)))
            (when (> verbosity 0)
              (let ((is-first t))
                (dolist (line (string-lines expr))
                  (if is-first
                      (progn (%lsl-println-screen (%lsl-cat ">>> " line))
                             (setq is-first nil))
                      (%lsl-println-screen (%lsl-cat "... " line))))))
            ;; Evaluate — errors propagate upward
            (eval (parse expr))))
        (%lsl-println-screen (%lsl-green (%lsl-cat "Log file read: " filename))))
    (t (e)
       (%lsl-println-screen (%lsl-cat "Error: " (condition-message e))))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Session log test — returns (result-message num-tests)
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-test-file (filename)
  "Run one test file and print per-test results to stdout.
Returns (result-msg num-tests) list."
  (let ((text nil))
    (handler-case
        (with-open-file (f filename)
          (setq text (readall f)))
      (t (e)
         (%lsl-println-screen (%lsl-cat "   Test file: " filename "..."))
         (%lsl-println-screen (%lsl-cat "File not found: " filename))
         (return-from %lsl-test-file (list "0 TESTS PASSED!" 0))))
    (%lsl-println-screen (%lsl-cat "   Test file: " filename "..."))
    (let ((entries    (%lsl-parse-log text))
          (num-passed 0)
          (expr-num   -1))
      (terpri)
      (dolist (entry entries)
        (setq expr-num (+ expr-num 1))
        (let* ((expr         (at 0 entry))
               (expected-out (at 1 entry))
               (expected-ret (%lsl-rstrip (at 2 entry)))
               (expected-err (at 3 entry))
               (evr          (%lsl-eval-expr expr))
               (actual-ret   (at 0 evr))
               (actual-out   (at 1 evr))
               (actual-err   (at 2 evr)))
          (%lsl-println-screen (%lsl-cat (string (+ expr-num 1)) "> " expr))
          (let* ((ret-ok (string= actual-ret expected-ret))
                 (out-ok (string= actual-out expected-out))
                 (err-ok (string= actual-err expected-err))
                 (passed (and ret-ok out-ok err-ok)))
            (when passed
              (setq num-passed (+ num-passed 1)))
            (when (not ret-ok)
              (%lsl-println-screen
                (%lsl-cat "         RETURN: expected [" expected-ret
                          "] got [" actual-ret "]")))
            (when (not out-ok)
              (%lsl-println-screen
                (%lsl-cat "         OUTPUT: expected [" expected-out
                          "] got [" actual-out "]")))
            (when (not err-ok)
              (%lsl-println-screen
                (%lsl-cat "         ERROR:  expected [" expected-err
                          "] got [" actual-err "]")))
            (%lsl-println-screen (%lsl-cat "         " (if passed "PASSED!" "Failed!")))
            (%lsl-println-screen ""))))
      (let* ((num-tests  (+ expr-num 1))
             (num-failed (- num-tests num-passed))
             (msg        (if (= num-failed 0)
                             (%lsl-cat (string num-tests) " TESTS PASSED!")
                             (%lsl-cat "(" (string num-failed) "/"
                                       (string num-tests) ") Failed."))))
        (%lsl-println-screen msg)
        (list msg num-tests)))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Listener commands
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-cmd-help (args)
  (if args
      (let ((cmd (car args)))
        (cond
          ((string= cmd "close")
           (%lsl-println-screen "Usage: close\n  Close the current logging session."))
          ((string= cmd "continue")
           (%lsl-println-screen "Usage: continue <filename> [V]\n  Read and restore a log, then keep it open for continued logging."))
          ((string= cmd "exit")
           (%lsl-println-screen "Usage: exit\n  Exit the Lisp listener."))
          ((string= cmd "help")
           (%lsl-println-screen "Usage: help [<command>]\n  List commands, or show help for a specific command."))
          ((string= cmd "instrument")
           (%lsl-println-screen "Usage: instrument\n  Toggle on/off evaluation timing display."))
          ((string= cmd "lhistory")
           (%lsl-println-screen "Usage: lhistory [<n>]\n  Get or set the readline history size."))
          ((string= cmd "log")
           (%lsl-println-screen "Usage: log <filename>\n  Begin a new session log."))
          ((string= cmd "quit")
           (%lsl-println-screen "Usage: quit\n  Exit the Lisp listener (same as exit)."))
          ((string= cmd "readlog")
           (%lsl-println-screen "Usage: readlog <filename> [V]\n  Read and execute a session log.  V for verbose output."))
          ((string= cmd "readsrc")
           (%lsl-println-screen "Usage: readsrc <filename>\n  Load and evaluate a Lisp source file."))
          ((string= cmd "reboot")
           (%lsl-println-screen "Usage: reboot\n  Reboot the interpreter (exits this Lisp listener)."))
          ((string= cmd "test")
           (%lsl-println-screen
             "Usage: test [<filename>]\n  Test with a log file, or run the full test suite.\n  NOTE: No per-file reboot — env is cumulative across test files."))
          ((string= cmd "trace")
           (%lsl-println-screen "Usage: trace\n  Toggle global function tracing on/off."))
          (t
           (error (%lsl-cat "No help on '" cmd "'")))))
      (progn
        (%lsl-println-screen "")
        (%lsl-println-screen (%lsl-bold-white "Listener Commands"))
        (%lsl-println-screen (%lsl-bold-white "================="))
        (columnize (list "close" "continue" "exit" "help" "instrument"
                         "lhistory" "log" "quit" "readlog" "readsrc"
                         "reboot" "test" "trace")
                   69)
        (%lsl-println-screen "")
        (%lsl-println-screen "Type ']help <command>' for help on a command.")
        (%lsl-println-screen "Type ']<command> <arg1> <arg2> ...' to execute a command."))))


(defun %lsl-cmd-log (args)
  (when (not (= (length args) 1))
    (error "Usage: log <filename>"))
  (when *lsl-log-stream*
    (error "Already logging.  Close the current log first."))
  (let ((filename (car args)))
    (setf *lsl-log-stream* (open filename :direction :output))
    (let ((ts (now-string)))
      (uwrite-line *lsl-log-stream* ">>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (uwrite-line *lsl-log-stream* (%lsl-cat "... ;;;;;;  Starting Log ( " ts " ): " filename))
      (uwrite-line *lsl-log-stream* "... 0")
      (uwrite-line *lsl-log-stream* "")
      (uwrite-line *lsl-log-stream* "==> 0"))
    (%lsl-println-screen (%lsl-green (%lsl-cat "Logging to: " filename)))))


(defun %lsl-cmd-close (args)
  (when args
    (error "Usage: close"))
  (when (null *lsl-log-stream*)
    (error "Not currently logging."))
  (uwrite-line *lsl-log-stream* ">>> ;;;;;;  Logging ended.")
  (uwrite-line *lsl-log-stream* "... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
  (uwrite-line *lsl-log-stream* "... 0")
  (uwrite-line *lsl-log-stream* "")
  (uwrite-line *lsl-log-stream* "==> 0")
  (close *lsl-log-stream*)
  (setf *lsl-log-stream* nil)
  (%lsl-println-screen (%lsl-green "Logging closed.")))


(defun %lsl-cmd-continue (args)
  (when (null args)
    (error "Usage: continue <filename> [V]"))
  (when *lsl-log-stream*
    (error "A log is already open.  Close it first."))
  (let* ((filename  (car args))
         (rest-args (cdr args))
         (verbosity (if (and rest-args
                             (string= (string-upcase (car rest-args)) "V"))
                        3
                        0)))
    (%lsl-restore-log filename verbosity)
    (setf *lsl-log-stream* (open filename :direction :output :if-exists :append))
    (let ((ts (now-string)))
      (uwrite-line *lsl-log-stream* ">>> ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      (uwrite-line *lsl-log-stream* (%lsl-cat "... ;;;;;;  Continuing Log ( " ts " ): " filename))
      (uwrite-line *lsl-log-stream* "... 0")
      (uwrite-line *lsl-log-stream* "")
      (uwrite-line *lsl-log-stream* "==> 0"))
    (%lsl-println-screen (%lsl-green (%lsl-cat "Continuing log: " filename)))))


(defun %lsl-cmd-readlog (args)
  (when (or (null args) (> (length args) 2))
    (error "Usage: readlog <filename> [V]"))
  (let* ((filename  (car args))
         (rest-args (cdr args))
         (verbosity (if (and rest-args
                             (string= (string-upcase (car rest-args)) "V"))
                        3
                        0)))
    (%lsl-restore-log filename verbosity)))


(defun %lsl-cmd-readsrc (args)
  (when (not (= (length args) 1))
    (error "Usage: readsrc <filename>"))
  (let ((filename (%lsl-rstrip (car args))))
    (handler-case
        (progn
          (eval (load filename))
          (%lsl-println-screen (%lsl-green (%lsl-cat "Source file read: " filename))))
      (t (e)
         (%lsl-println-screen (%lsl-cat "Error loading file: " (condition-message e)))))))


(defun %lsl-cmd-reboot (args)
  "Reboot the interpreter and exit this Lisp listener."
  (when args
    (error "Usage: reboot"))
  (when *lsl-log-stream*
    (error "Please close the log before rebooting."))
  (%lsl-println-screen (%lsl-dim "- Rebooting interpreter..."))
  (interpreter-reboot)
  ;; After reboot the global env is fresh; this REPL's env references the old
  ;; env chain so we exit rather than continue in an inconsistent state.
  (%lsl-println-screen "Interpreter rebooted.")
  (%lsl-println-screen "Load listener.lisp or type (lsl-start) to restart the Lisp listener.")
  (throw 'lsl-exit nil))


(defun %lsl-cmd-test (args)
  (when (> (length args) 1)
    (error "Usage: test [<filename>]"))
  (when *lsl-log-stream*
    (error "Please close the log before running tests."))
  (let* ((filenames (if args
                        args
                        (directory-files *lsl-testdir* ".log")))
         (total-run  0)
         (ts         (now-string "%Y-%m-%d-%H%M%S"))
         (runs-dir   (%lsl-cat *lsl-testdir* "/runs"))
         (run-file   (%lsl-cat runs-dir "/test-" ts ".run")))
    (make-directory runs-dir)
    (%lsl-println-screen "")
    (%lsl-println-screen (%lsl-bold-white "Test Report"))
    (%lsl-println-screen (%lsl-bold-white "==========="))
    (let ((run-strm (open run-file :direction :output)))
      (handler-case
          (dolist (filename filenames)
            (let* ((result   (%lsl-test-file filename))
                   (msg      (at 0 result))
                   (n        (at 1 result))
                   (base     (file-basename filename))
                   (pad      (subseq "                                        "
                                     0 (max 0 (- 40 (length base)))))
                   (line     (%lsl-cat base pad " " msg)))
              (setq total-run (+ total-run n))
              (%lsl-println-screen (if (search "PASSED!" msg)
                                       (%lsl-green line)
                                       (%lsl-red   line)))
              (uwrite-line run-strm line)))
        (t (e)
           (%lsl-println-screen (%lsl-red (%lsl-cat "Test error: " (condition-message e))))))
      (close run-strm))
    (%lsl-println-screen "")
    (%lsl-println-screen (%lsl-cat "Total test files: " (string (length filenames)) "."))
    (%lsl-println-screen (%lsl-cat "Total test cases: " (string total-run) "."))
    (%lsl-println-screen (%lsl-cat "Test output: " run-file))))


(defun %lsl-cmd-trace (args)
  (when args
    (error "Usage: trace"))
  (let ((on (toggle-global-trace)))
    (%lsl-println-screen
      (if on
          (%lsl-cat "Tracing is now " (%lsl-green "ON") ".")
          (%lsl-cat "Tracing is now " (%lsl-yellow "OFF") ".")))))


(defun %lsl-cmd-instrument (args)
  (when args
    (error "Usage: instrument"))
  (setf *lsl-instrumenting* (not *lsl-instrumenting*))
  (%lsl-println-screen
    (if *lsl-instrumenting*
        (%lsl-cat "Instrumenting is now " (%lsl-green "ON") ".")
        (%lsl-cat "Instrumenting is now " (%lsl-yellow "OFF") "."))))


(defun %lsl-cmd-lhistory (args)
  (cond
    ((null args)
     (%lsl-println-screen (%lsl-cat "Current history size: " (string *lsl-hist-max*))))
    ((= (length args) 1)
     (handler-case
         (let ((n (integer (car args))))
           (when (< n 1)
             (error "History size must be a positive integer."))
           (setf *lsl-hist-max* n)
           (readline-set-history-length n)
           (%lsl-println-screen (%lsl-cat "History size set to: " (string n))))
       (t (e)
          (error "Usage: lhistory [<n>]"))))
    (t
     (error "Usage: lhistory [<n>]"))))


(defun %lsl-cmd-exit (args)
  (when args
    (error "Usage: exit"))
  (when *lsl-log-stream*
    (%lsl-cmd-close nil))
  (%lsl-println-screen "Bye.")
  (throw 'lsl-exit nil))

(defun %lsl-cmd-quit (args)
  (%lsl-cmd-exit args))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Command dispatcher
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-dispatch (input)
  "Dispatch a ] command string.  input is the full line beginning with ']'."
  (let* ((body  (subseq input 1))
         (parts (string-split body))
         (cmd   (if parts (string-downcase (car parts)) ""))
         (cargs (cdr parts)))
    (cond
      ((string= cmd "help")       (%lsl-cmd-help       cargs))
      ((string= cmd "log")        (%lsl-cmd-log        cargs))
      ((string= cmd "close")      (%lsl-cmd-close      cargs))
      ((string= cmd "continue")   (%lsl-cmd-continue   cargs))
      ((string= cmd "readlog")    (%lsl-cmd-readlog    cargs))
      ((string= cmd "readsrc")    (%lsl-cmd-readsrc    cargs))
      ((string= cmd "reboot")     (%lsl-cmd-reboot     cargs))
      ((string= cmd "test")       (%lsl-cmd-test       cargs))
      ((string= cmd "trace")      (%lsl-cmd-trace      cargs))
      ((string= cmd "instrument") (%lsl-cmd-instrument cargs))
      ((string= cmd "lhistory")   (%lsl-cmd-lhistory   cargs))
      ((string= cmd "exit")       (%lsl-cmd-exit       cargs))
      ((string= cmd "quit")       (%lsl-cmd-quit       cargs))
      (t
       (error (%lsl-cat "Unknown listener command \"" cmd "\""))))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Process one complete (possibly multi-line) expression
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-process (input lines-rev)
  "Evaluate input string and display results.
lines-rev is the accumulated input lines in reverse order (for log writing).
NOTE: ParseError from malformed input propagates uncaught."
  ;; Write expression to log with >>> / ... prefixes (no trailing newline
  ;; between expr and result, matching the Python listener's log format)
  (when *lsl-log-stream*
    (let* ((ordered    (reverse lines-rev))
           (first-line (car ordered))
           (rest-lines (cdr ordered)))
      (uwrite! *lsl-log-stream* (%lsl-cat ">>> " first-line))
      (dolist (line rest-lines)
        (uwrite! *lsl-log-stream* (%lsl-cat "... " line)))))
  ;; Add to readline history
  (readline-add-history input)
  ;; Evaluate
  (let* ((result  (%lsl-eval-expr input))
         (retval  (at 0 result))
         (output  (at 1 result))
         (errmsg  (at 2 result))
         (elapsed (at 3 result)))
    (cond
      ((not (string= errmsg ""))
       ;; Runtime error
       (%lsl-write-error errmsg)
       (%lsl-blank))
      (t
       ;; Print captured output to screen (not to log — matches Python listener)
       (when (not (string= output ""))
         (uwrite-line output)
         (flush))
       ;; Display return values
       (%lsl-write-result (string-lines retval))
       (%lsl-blank)))
    ;; Timing (screen only, not logged)
    (when *lsl-instrumenting*
      (%lsl-println-screen
        (%lsl-cat "-------------  Total time: " (string elapsed) " sec")))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Welcome banner
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-welcome ()
  (%lsl-println-screen
    (%lsl-cat "Enter '" (%lsl-cyan "]help") "' for listener commands."))
  (%lsl-println-screen
    "Enter any expression to have it evaluated by the interpreter.")
  (%lsl-println-screen
    (%lsl-cat "For online help type '" (%lsl-cyan "(help)") "' to begin."))
  (%lsl-println-screen (%lsl-bold-green "Welcome!"))
  (%lsl-println-screen ""))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Main REPL loop
;;; ─────────────────────────────────────────────────────────────────────────────

(defun %lsl-repl ()
  "Inner REPL loop.  Exits when ]exit/]quit is typed, ]reboot runs,
or EOF is reached on stdin."
  (let ((running   t)
        (lines-rev nil))   ; accumulated input lines, most recent first
    (while running
      (let ((prompt (if (null lines-rev) ">>> " "... "))
            (line   nil))
        ;; Print prompt and read input line
        (uwrite! prompt)
        (flush)
        (handler-case
            (setq line (read-line nil nil nil))
          (t (e)
             ;; Unexpected read error — treat as EOF
             (setq running nil)))

        (cond
          ;; EOF or read error
          ((null line)
           (when *lsl-log-stream*
             (handler-case (%lsl-cmd-close nil) (t (e) nil)))
           (setq running nil))

          ;; Blank line with no accumulated input — ignore
          ((and (string= line "") (null lines-rev))
           nil)

          ;; Blank line with accumulated input — evaluate
          ((string= line "")
           (let ((input (string-join "\n" (reverse lines-rev))))
             (handler-case
                 (if (%lsl-starts-with input "]")
                     (%lsl-dispatch input)
                     (%lsl-process input lines-rev))
               (t (e)
                  (%lsl-write-error (condition-message e))
                  (%lsl-blank))))
           (setq lines-rev nil))

          ;; Non-blank line — accumulate
          (t
           (setq lines-rev (cons line lines-rev))))))))


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Entry point
;;; ─────────────────────────────────────────────────────────────────────────────

(defun lsl-start ()
  "Start the Lisp listener.  Returns when the user types ]exit, ]quit, ]reboot,
or when EOF is reached on stdin.
Global state (*lsl-log-stream*, *lsl-instrumenting*, etc.) persists across
calls to lsl-start, so a listener interrupted by a parse error can be resumed."
  ;; Initialise readline history
  (readline-read-history-file)
  (readline-set-history-length *lsl-hist-max*)
  ;; Print welcome
  (%lsl-welcome)
  ;; Run — catch 'lsl-exit is the non-local exit point for ]exit, ]quit, ]reboot
  (catch 'lsl-exit
    (%lsl-repl))
  ;; Save readline history on exit
  (readline-write-history-file)
  nil)


;;; ─────────────────────────────────────────────────────────────────────────────
;;; Auto-start when this file is loaded
;;; ─────────────────────────────────────────────────────────────────────────────

(lsl-start)
