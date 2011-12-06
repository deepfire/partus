(setq partus-path "/home/deepfire/src/partus/"
      slime-default-lisp 'partus)

;; Citing Python 3.2.2 documentation (Python Setup and Usage/1.1.1):
;; "In non-interactive mode, the entire input is parsed before it is executed."
;; ..therefore we have to (process-send-eof)

(push '(partus ("/usr/bin/python3") :init python-init-command :send-eof t)
;; (push '(partus ("/usr/bin/strace" "-s" "64000" "-o" "/home/deepfire/partus.log" "/usr/bin/python3") :init python-init-command)
;; (push '(partus ("/home/deepfire/Downloads/dtach-0.8/dtach" "-l" "/home/deepfire/partus") :init python-init-command)
      slime-lisp-implementations)
(push 'python-mode slime-lisp-modes)

;; Part of SLIME-SETUP:
(when (member 'python-mode slime-lisp-modes)
  (add-hook 'python-mode-hook 'slime-lisp-mode-hook))

(defun python-init-command (port-filename coding-system)
  "Return a string to initialize python."
  (let ((loader (if (file-name-absolute-p slime-backend)
                    slime-backend
                    (concat slime-path slime-backend)))
        (encoding (slime-coding-system-cl-name coding-system)))
    ;; Return a single form to avoid problems with buffered input.
    (format "import sys; sys.path.append('%s'); from cl import *; import partus; setq('_debug_swank_backend_', t); setq('_swank_debug_p_', t); partus.start_server('%s', coding_system = '%s')\n"
            partus-path port-filename coding-system)))

;;;
;;; Python does not process input from non-TTYs incrementally, hence SEND-EOF
;;;
(defun* slime-start (&key (program inferior-lisp-program) program-args 
                          directory
                          (coding-system slime-net-coding-system)
                          (init 'slime-init-command)
                          name
                          (buffer "*inferior-lisp*")
                          init-function
                          send-eof
                          env)
  "Start a Lisp process and connect to it.
This function is intended for programmatic use if `slime' is not
flexible enough.

PROGRAM and PROGRAM-ARGS are the filename and argument strings
  for the subprocess.
INIT is a function that should return a string to load and start
  Swank. The function will be called with the PORT-FILENAME and ENCODING as
  arguments.  INIT defaults to `slime-init-command'. 
CODING-SYSTEM a symbol for the coding system. The default is 
  slime-net-coding-system
ENV environment variables for the subprocess (see `process-environment').
INIT-FUNCTION function to call right after the connection is established.
BUFFER the name of the buffer to use for the subprocess.
NAME a symbol to describe the Lisp implementation
DIRECTORY change to this directory before starting the process.
"
  (let ((args (list :program program :program-args program-args :buffer buffer 
                    :coding-system coding-system :init init :name name
                    :init-function init-function :send-eof send-eof :env env)))
    (slime-check-coding-system coding-system)
    (when (slime-bytecode-stale-p)
      (slime-urge-bytecode-recompile))
    (let ((proc (slime-maybe-start-lisp program program-args env
                                        directory buffer)))
      (slime-inferior-connect proc args)
      (pop-to-buffer (process-buffer proc)))))

;;;
;;; Python does not process input from non-TTYs incrementally, hence SEND-EOF
;;;
(defun slime-start-swank-server (process args)
  "Start a Swank server on the inferior lisp."
  (destructuring-bind (&key coding-system init send-eof &allow-other-keys) args
    (with-current-buffer (process-buffer process)
      (make-local-variable 'slime-inferior-lisp-args)
      (setq slime-inferior-lisp-args args)
      (let ((str (funcall init (slime-swank-port-file) coding-system)))
        (goto-char (process-mark process)) 
        (insert-before-markers str)
        (process-send-string process str)
        (when send-eof
          (process-send-eof process))))))


;;;
;;; Symbol bounds: SLIME-BOUNDS-OF-SYMBOL-AT-POINT
;;;
(defun partus-beginning-of-symbol ()
  "Move to the beginning of the CL-style symbol at point."
  ;; (while (re-search-backward "\\(\\sw\\|\\s_\\|\\w\\.\\|\\s\\\\|[#@|]\\)\\="
  ;;                            (when (> (point) 2000) (- (point) 2000))
  ;;                            t))
  (while (re-search-backward "\\(\\sw\\|\\s_\\|\\w\\.\\|\\s\\\\|[#@|]\\)\\="
                             (when (> (point) 2000) (- (point) 2000))
                             t))
  (re-search-forward "\\.=#[-+<|]" nil t)
  (when (and (looking-at "@") (eq (char-before) ?\,))
    (forward-char)))

(defun partus-end-of-symbol ()
  "Move to the end of the CL-style symbol at point."
  (re-search-forward "\\=\\(\\sw\\|\\.\\|\\s_\\|#:\\|[@|]\\)*"))

(put 'slime-symbol 'end-op 'partus-end-of-symbol)
(put 'slime-symbol 'beginning-op 'partus-beginning-of-symbol)

(defun bounds-of-thing-at-point (thing)
  "Determine the start and end buffer locations for the THING at point.
THING is a symbol which specifies the kind of syntactic entity you want.
Possibilities include `symbol', `list', `sexp', `defun', `filename', `url',
`email', `word', `sentence', `whitespace', `line', `page' and others.

See the file `thingatpt.el' for documentation on how to define
a symbol as a valid THING.

The value is a cons cell (START . END) giving the start and end positions
of the textual entity that was found."
  (if (get thing 'bounds-of-thing-at-point)
      (funcall (get thing 'bounds-of-thing-at-point))
      (let ((orig (point)))
        (condition-case nil
            (save-excursion
              ;; Try moving forward, then back.
              (funcall ;; First move to end.
               (or (get thing 'end-op)
                   (lambda () (forward-thing thing 1))))
              (funcall ;; Then move to beg.
               (or (get thing 'beginning-op)
                   (lambda () (forward-thing thing -1))))
              (let ((beg (point)))
                (if (not (and beg (> beg orig)))
                    ;; If that brings us all the way back to ORIG,
                    ;; it worked.  But END may not be the real end.
                    ;; So find the real end that corresponds to BEG.
                    (let ((real-end
                           (progn
                             (funcall
                              (or (get thing 'end-op)
                                  (lambda () (forward-thing thing 1))))
                             (point))))
                      (if (and beg real-end (<= beg orig) (<= orig real-end))
                          (cons beg real-end)))
                    (goto-char orig)
                    ;; Try a second time, moving backward first and then forward,
                    ;; so that we can find a thing that ends at ORIG.
                    (funcall ;; First, move to beg.
                     (or (get thing 'beginning-op)
                         (lambda () (forward-thing thing -1))))
                    (funcall ;; Then move to end.
                     (or (get thing 'end-op)
                         (lambda () (forward-thing thing 1))))
                    (let ((end (point))
                          (real-beg
                           (progn
                             (funcall
                              (or (get thing 'beginning-op)
                                  (lambda () (forward-thing thing -1))))
                             (point))))
                      (if (and real-beg end (<= real-beg orig) (<= orig end))
                          (cons real-beg end))))))
          (error nil)))))