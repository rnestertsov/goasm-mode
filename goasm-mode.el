;;; goasm-mode.el --- Go assembly viewer -*- lexical-binding: t; -*-
;; ABOUTME: Emacs minor mode for viewing Go compiler assembly output.
;; ABOUTME: Provides per-function assembly display with source line navigation.

;; Author: Roman Nestertsov
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; URL: https://github.com/rnestertsov/goasm
;; Keywords: languages, go, assembly

;;; Commentary:

;; goasm-mode is a minor mode for Go source buffers that generates and
;; displays compiler assembly output (Plan 9 pseudo-assembly) scoped to
;; the current function, with source-line-to-assembly navigation.
;;
;; Usage:
;;   M-x goasm-minor-mode   - Enable in a Go buffer
;;   C-c C-a                - Generate assembly for current function
;;   C-c C-l                - Jump assembly view to current source line

;;; Code:

(defgroup goasm nil
  "Go assembly viewer."
  :group 'languages
  :prefix "goasm-")

(defcustom goasm-go-command "go"
  "Path to the go executable."
  :type 'string
  :group 'goasm)

(defcustom goasm-build-flags nil
  "Additional flags passed to `go build'."
  :type '(repeat string)
  :group 'goasm)

(defun goasm--parse-functions (output)
  "Parse go build assembly OUTPUT into alist of (func-name . asm-text).
Only extracts STEXT entries (actual function definitions)."
  (let ((result '())
        (lines (split-string output "\n"))
        current-name
        current-lines)
    (dolist (line lines)
      (cond
       ;; Function header: "main.Add STEXT size=16 ..."
       ((string-match "^\\([^ \t]+\\) STEXT " line)
        ;; Save previous function if any
        (when current-name
          (push (cons current-name
                      (string-join (nreverse current-lines) "\n"))
                result))
        (setq current-name (match-string 1 line))
        (setq current-lines (list line)))
       ;; Non-STEXT header: "main.Add.arginfo1 SRODATA ..." — stop current function
       ((string-match "^[^ \t].*\\(SRODATA\\|SDWARFCUINFO\\|SDWARFABSFCN\\|SNOPTRDATA\\)" line)
        (when current-name
          (push (cons current-name
                      (string-join (nreverse current-lines) "\n"))
                result))
        (setq current-name nil)
        (setq current-lines nil))
       ;; Indented line or hex dump — belongs to current function
       ((and current-name (string-match-p "^\t" line))
        (push line current-lines))
       ;; Blank or unrecognized non-indented line — end current function
       ((and current-name (not (string-empty-p line))
             (not (string-match-p "^\t" line)))
        (push (cons current-name
                    (string-join (nreverse current-lines) "\n"))
              result)
        (setq current-name nil)
        (setq current-lines nil))))
    ;; Don't forget the last function
    (when current-name
      (push (cons current-name
                  (string-join (nreverse current-lines) "\n"))
            result))
    (nreverse result)))

(defun goasm--current-function-name ()
  "Return the name of the Go function enclosing point, or nil."
  (let ((original-pos (point)))
    (save-excursion
      (end-of-line)
      (when (re-search-backward
             "^func\\s-+\\(?:(\\s-*[^)]+)\\s-+\\)?\\([A-Za-z_][A-Za-z0-9_]*\\)\\s-*(" nil t)
        (let ((func-name (match-string-no-properties 1))
              (func-start (match-beginning 0)))
          ;; Find the opening brace and its matching close
          (goto-char func-start)
          (when (and (search-forward "{" nil t)
                     (progn (backward-char)
                            (ignore-errors (forward-sexp) t)))
            (let ((func-end (point)))
              (when (<= original-pos func-end)
                func-name))))))))

(defun goasm--parse-line-mapping (asm-text)
  "Build a mapping from source line numbers to assembly buffer line numbers.
Returns an alist of (source-line . (asm-line-1 asm-line-2 ...)).
ASM-TEXT is the assembly text for a single function.
Buffer line numbers are 1-indexed."
  (let ((mapping (make-hash-table :test 'eql))
        (buf-line 1))
    (dolist (line (split-string asm-text "\n"))
      (when (string-match "(.*:\\([0-9]+\\))" line)
        (let ((src-line (string-to-number (match-string 1 line))))
          (puthash src-line
                   (append (gethash src-line mapping) (list buf-line))
                   mapping)))
      (setq buf-line (1+ buf-line)))
    ;; Convert hash-table to alist
    (let (result)
      (maphash (lambda (k v) (push (cons k v) result)) mapping)
      result)))

(defun goasm--run-compiler (directory)
  "Run go build -gcflags '-S' in DIRECTORY and return stderr as a string.
Returns the compiler output (assembly or error messages)."
  (let ((default-directory (file-name-as-directory directory)))
    (with-temp-buffer
      (let ((args (append (list goasm-go-command nil t nil
                                "build" "-gcflags" "-S")
                          goasm-build-flags
                          (list "."))))
        (apply #'call-process args)
        (buffer-string)))))

(defconst goasm-buffer-name "*goasm*"
  "Name of the buffer used to display assembly output.")

(defconst goasm--display-action
  '((display-buffer-reuse-window
     display-buffer-pop-up-window)
    . ((inhibit-same-window . t)))
  "Action alist for `display-buffer' when showing the *goasm* buffer.")

(defvar goasm-output-font-lock-keywords
  '(;; Instructions (mnemonics)
    ("\\<\\(MOVD\\|MOVQ\\|MOVL\\|MOVB\\|MOVW\\|ADD\\|SUB\\|MUL\\|DIV\\|AND\\|OR\\|XOR\\|SHL\\|SHR\\|CMP\\|TEST\\|JMP\\|JE\\|JNE\\|JZ\\|JNZ\\|JL\\|JG\\|JLE\\|JGE\\|BEQ\\|BNE\\|BGT\\|BLT\\|BGE\\|BLE\\|CALL\\|RET\\|NOP\\|PUSH\\|POP\\|LEA\\|TEXT\\|FUNCDATA\\|PCDATA\\)\\>" . font-lock-keyword-face)
    ;; Registers
    ("\\<\\(R[0-9]+\\|AX\\|BX\\|CX\\|DX\\|SI\\|DI\\|SP\\|BP\\|R[89]\\|R1[0-5]\\|ZR\\)\\>" . font-lock-variable-name-face)
    ;; Addresses: 0x0000 00000
    ("\\(0x[0-9a-f]+\\s-+[0-9]+\\)" . font-lock-constant-face)
    ;; Source references: (file.go:123)
    ("([^)]+:[0-9]+)" . font-lock-comment-face))
  "Font lock keywords for goasm-output-mode.")

(define-derived-mode goasm-output-mode special-mode "GoAsm"
  "Major mode for displaying Go assembly output.
This is a read-only mode for viewing assembly generated by the Go compiler."
  (setq-local font-lock-defaults
              '(goasm-output-font-lock-keywords)))

(defun goasm--display-buffer (content)
  "Display CONTENT in the *goasm* buffer.
Creates the buffer if needed, replaces existing content."
  (let ((buf (get-buffer-create goasm-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (goasm-output-mode)
      (goto-char (point-min)))
    (display-buffer buf goasm--display-action)
    buf))

(defun goasm--find-project-root (directory)
  "Find the Go project root by locating go.mod above DIRECTORY.
Returns the directory containing go.mod, or nil if not found."
  (let ((dir (locate-dominating-file directory "go.mod")))
    (when dir
      (file-name-as-directory (expand-file-name dir)))))

(defun goasm--relativize-paths (asm-text project-root)
  "Replace absolute source paths in ASM-TEXT with paths relative to PROJECT-ROOT."
  (let ((root (regexp-quote project-root)))
    (replace-regexp-in-string
     (concat "(\\(" root "\\)")
     "("
     asm-text
     t)))

(defvar-local goasm--line-mapping nil
  "Alist mapping source line numbers to assembly buffer line numbers.
Set after goasm-show generates assembly.")

(defvar-local goasm--current-asm-function nil
  "Name of the function whose assembly is currently displayed.")

(defun goasm--package-directory ()
  "Return the Go package directory for the current buffer."
  (when buffer-file-name
    (file-name-directory buffer-file-name)))

(defun goasm-show ()
  "Generate and display assembly for the Go function at point.
Runs `go build -gcflags \\='-S\\=' and shows the assembly for the
enclosing function in the *goasm* buffer."
  (interactive)
  (let ((func-name (goasm--current-function-name)))
    (unless func-name
      (user-error "Not inside a function"))
    (let* ((pkg-dir (goasm--package-directory))
           (output (goasm--run-compiler pkg-dir))
           (funcs (goasm--parse-functions output))
           ;; Find the function — compiler uses package-qualified names
           (match (seq-find
                   (lambda (entry)
                     (string-match-p
                      (concat "\\." (regexp-quote func-name) "$")
                      (car entry)))
                   funcs)))
      (unless match
        (user-error "No assembly found for function '%s' (may be inlined or dead-code eliminated)" func-name))
      (let* ((asm-text (cdr match))
             (project-root (goasm--find-project-root pkg-dir))
             (display-text (if project-root
                               (goasm--relativize-paths asm-text project-root)
                             asm-text))
             (mapping (goasm--parse-line-mapping display-text)))
        (setq goasm--line-mapping mapping)
        (setq goasm--current-asm-function func-name)
        (goasm--display-buffer display-text)))))

(defvar goasm--highlight-overlays nil
  "List of overlays used to highlight assembly lines for current source line.")

(defun goasm--clear-highlights ()
  "Remove all goasm highlight overlays."
  (mapc #'delete-overlay goasm--highlight-overlays)
  (setq goasm--highlight-overlays nil))

(defface goasm-highlight-face
  '((t :background "#3a3a5a" :extend t))
  "Face used to highlight assembly lines corresponding to current source line."
  :group 'goasm)

(defun goasm--ensure-compiled ()
  "Compile assembly for the current function if not already done.
Calls `goasm-show' when no assembly exists or when the cursor has
moved to a different function."
  (let ((func-name (goasm--current-function-name)))
    (unless func-name
      (user-error "Not inside a function"))
    (unless (and goasm--line-mapping
                 (equal func-name goasm--current-asm-function))
      (goasm-show))))

(defun goasm-goto-line ()
  "Jump the *goasm* buffer to assembly for the current source line.
Automatically compiles if needed for the current function."
  (interactive)
  (goasm--ensure-compiled)
  (let* ((src-line (line-number-at-pos))
         (asm-lines (cdr (assoc src-line goasm--line-mapping)))
         (buf (get-buffer goasm-buffer-name)))
    (unless buf
      (user-error "No *goasm* buffer — run goasm-show first"))
    (unless asm-lines
      (message "No assembly for source line %d" src-line))
    (when asm-lines
      (with-current-buffer buf
        (goasm--clear-highlights)
        ;; Go to first matching assembly line
        (goto-char (point-min))
        (forward-line (1- (car asm-lines)))
        ;; Highlight all matching lines
        (dolist (asm-line asm-lines)
          (save-excursion
            (goto-char (point-min))
            (forward-line (1- asm-line))
            (let ((ov (make-overlay (line-beginning-position)
                                    (1+ (line-end-position)))))
              (overlay-put ov 'face 'goasm-highlight-face)
              (push ov goasm--highlight-overlays)))))
      ;; Ensure the buffer is visible and scrolled
      (let ((win (or (get-buffer-window buf)
                     (display-buffer buf goasm--display-action))))
        (when win
          (set-window-point win (with-current-buffer buf (point))))))))

(defvar goasm-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-a") #'goasm-show)
    (define-key map (kbd "C-c C-l") #'goasm-goto-line)
    map)
  "Keymap for `goasm-minor-mode'.")

;;;###autoload
(define-minor-mode goasm-minor-mode
  "Minor mode for viewing Go compiler assembly output.
\\{goasm-minor-mode-map}"
  :lighter " GoAsm"
  :keymap goasm-minor-mode-map
  :group 'goasm)

(provide 'goasm-mode)
;;; goasm-mode.el ends here
