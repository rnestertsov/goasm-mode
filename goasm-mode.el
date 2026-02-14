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

(defvar-local goasm--source-buffer nil
  "The Go source buffer associated with this *goasm* output buffer.")

(defvar-local goasm--project-root nil
  "The Go project root directory for resolving relative paths in assembly.")

(defvar-local goasm--arch nil
  "Target architecture (e.g. \"amd64\", \"arm64\") for the *goasm* buffer.
Set during `goasm-show' via `go env GOARCH'.")

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

(defconst goasm--instruction-docs
  '(;; Go pseudo-instructions
    ("TEXT" . "Declare a function entry point")
    ("FUNCDATA" . "Attach runtime metadata to a function (GC, args)")
    ("PCDATA" . "Annotate PC-value table for runtime (stack maps, unsafe points)")
    ("DATA" . "Initialize a memory region with a value")
    ("GLOBL" . "Declare a global symbol with size and flags")
    ("PCALIGN" . "Align next instruction to a power-of-two boundary")
    ;; Data movement
    ("MOV" . "Copy value from source to destination")
    ("LEA" . "Load effective address (compute address without dereference)")
    ("PUSH" . "Push value onto the stack")
    ("POP" . "Pop value from the stack")
    ("XCHG" . "Exchange values between two operands")
    ("BSWAP" . "Byte-swap a register (reverse byte order)")
    ("POPCNT" . "Count the number of set bits")
    ;; Arithmetic
    ("ADD" . "Add source to destination")
    ("SUB" . "Subtract source from destination")
    ("MUL" . "Unsigned multiply")
    ("IMUL" . "Signed multiply")
    ("DIV" . "Unsigned divide")
    ("IDIV" . "Signed divide")
    ("INC" . "Increment by one")
    ("DEC" . "Decrement by one")
    ("NEG" . "Two's complement negate")
    ("NOT" . "Bitwise NOT (one's complement)")
    ;; Bitwise / logical
    ("AND" . "Bitwise AND")
    ("OR" . "Bitwise OR")
    ("XOR" . "Bitwise exclusive OR")
    ("SHL" . "Shift left (logical)")
    ("SHR" . "Shift right (logical)")
    ("SAR" . "Shift right (arithmetic, sign-extending)")
    ;; Comparison / test
    ("CMP" . "Compare two operands (subtract and set flags)")
    ("TEST" . "Bitwise AND and set flags (discard result)")
    ;; Jumps (x86-style)
    ("JMP" . "Unconditional jump")
    ("JE" . "Jump if equal (ZF=1)")
    ("JNE" . "Jump if not equal (ZF=0)")
    ("JZ" . "Jump if zero (ZF=1)")
    ("JNZ" . "Jump if not zero (ZF=0)")
    ("JL" . "Jump if less (signed)")
    ("JG" . "Jump if greater (signed)")
    ("JLE" . "Jump if less or equal (signed)")
    ("JGE" . "Jump if greater or equal (signed)")
    ("JA" . "Jump if above (unsigned)")
    ("JB" . "Jump if below (unsigned)")
    ("JAE" . "Jump if above or equal (unsigned)")
    ("JBE" . "Jump if below or equal (unsigned)")
    ;; Branches (ARM-style)
    ("B" . "Unconditional branch")
    ("BEQ" . "Branch if equal")
    ("BNE" . "Branch if not equal")
    ("BGT" . "Branch if greater than")
    ("BLT" . "Branch if less than")
    ("BGE" . "Branch if greater or equal")
    ("BLE" . "Branch if less or equal")
    ;; Control flow
    ("CALL" . "Call a subroutine (push return address and jump)")
    ("RET" . "Return from subroutine")
    ("NOP" . "No operation")
    ("SYSCALL" . "Invoke operating system service"))
  "Alist of (MNEMONIC . description) for Go assembly instructions.
Covers Go pseudo-instructions, x86, and ARM mnemonics (base forms
without size suffixes).")

(defun goasm--parse-instruction (line)
  "Extract the instruction mnemonic from an assembly LINE.
Returns the mnemonic string or nil for non-instruction lines."
  (when (and (stringp line)
             (not (string-empty-p line))
             (string-match ")\t\\([A-Z][A-Z0-9]*\\)" line))
    (match-string 1 line)))

(defun goasm--instruction-doc (mnemonic)
  "Return a short description for MNEMONIC, or nil if unknown.
Tries an exact match first, then strips size suffixes
\(B, W, L, Q, D, S, SS, SD) to find the base instruction."
  (or (cdr (assoc mnemonic goasm--instruction-docs))
      ;; Strip size suffixes: try longest first (SS, SD) then single char
      (let ((base nil))
        (cond
         ((string-match "\\`\\(.*[A-Z]\\)\\(?:SS\\|SD\\)\\'" mnemonic)
          (setq base (match-string 1 mnemonic)))
         ((string-match "\\`\\(.*[A-Z]\\)[BWLQDS]\\'" mnemonic)
          (setq base (match-string 1 mnemonic))))
        (when base
          (cdr (assoc base goasm--instruction-docs))))))

(defun goasm--eldoc-function ()
  "Return a short doc string for the instruction on the current line.
Intended for use as `eldoc-documentation-function'."
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (mnemonic (goasm--parse-instruction line)))
    (when mnemonic
      (let ((desc (goasm--instruction-doc mnemonic)))
        (when desc
          (format "%s - %s" mnemonic desc))))))

(defconst goasm--pseudo-instructions
  '("TEXT" "FUNCDATA" "PCDATA" "DATA" "GLOBL" "PCALIGN")
  "Go assembler pseudo-instructions documented at go.dev/doc/asm.")

(defun goasm--detect-arch ()
  "Detect the target architecture by running `go env GOARCH'.
Returns a string like \"amd64\" or \"arm64\"."
  (string-trim
   (with-temp-buffer
     (call-process goasm-go-command nil t nil "env" "GOARCH")
     (buffer-string))))

(defun goasm-describe-instruction ()
  "Open documentation for the instruction on the current line.
For Go pseudo-instructions, opens go.dev/doc/asm.
For x86/amd64, opens felixcloutier.com/x86/{instruction}.
For ARM64, opens the ARM developer documentation index."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (line-end-position)))
         (mnemonic (goasm--parse-instruction line)))
    (unless mnemonic
      (user-error "No instruction on this line"))
    (let ((url (cond
                ;; Go pseudo-instructions
                ((member mnemonic goasm--pseudo-instructions)
                 "https://go.dev/doc/asm")
                ;; ARM64 — link to index (per-instruction URLs are complex)
                ((equal goasm--arch "arm64")
                 "https://developer.arm.com/documentation/ddi0602/latest/")
                ;; x86/amd64/386 — strip suffix, lowercase
                (t
                 (let ((base (or (and (goasm--instruction-doc mnemonic)
                                      mnemonic)
                                 ;; Try stripping suffix to get base
                                 (let ((b nil))
                                   (cond
                                    ((string-match "\\`\\(.*[A-Z]\\)\\(?:SS\\|SD\\)\\'" mnemonic)
                                     (setq b (match-string 1 mnemonic)))
                                    ((string-match "\\`\\(.*[A-Z]\\)[BWLQDS]\\'" mnemonic)
                                     (setq b (match-string 1 mnemonic))))
                                   b)
                                 mnemonic)))
                   (format "https://www.felixcloutier.com/x86/%s"
                           (downcase base)))))))
      (browse-url url))))

(defun goasm-goto-source ()
  "Jump from the current assembly line to the corresponding source line.
Parses the source reference (file.go:123) from the current line and
switches to the source buffer at that line."
  (interactive)
  (let ((line (buffer-substring-no-properties
               (line-beginning-position) (line-end-position))))
    (unless (string-match "(\\([^)]+\\.go\\):\\([0-9]+\\))" line)
      (user-error "No source reference on this line"))
    (let* ((file (match-string 1 line))
           (line-num (string-to-number (match-string 2 line)))
           (source-buf goasm--source-buffer)
           (project-root goasm--project-root)
           (full-path (when project-root
                        (expand-file-name file project-root))))
      ;; Prefer the stored source buffer; fall back to opening the file
      (let ((target-buf (cond
                         ((and source-buf (buffer-live-p source-buf))
                          source-buf)
                         ((and full-path (file-exists-p full-path))
                          (find-file-noselect full-path))
                         (t (user-error "Cannot find source file '%s'" file)))))
        (let ((win (or (get-buffer-window target-buf)
                       (display-buffer target-buf goasm--display-action))))
          (when win
            (select-window win)
            (goto-char (point-min))
            (forward-line (1- line-num))))))))

(defvar goasm-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-l") #'goasm-goto-source)
    (define-key map (kbd "C-c C-d") #'goasm-describe-instruction)
    map)
  "Keymap for `goasm-output-mode'.")

(define-derived-mode goasm-output-mode special-mode "GoAsm"
  "Major mode for displaying Go assembly output.
This is a read-only mode for viewing assembly generated by the Go compiler."
  (setq-local font-lock-defaults
              '(goasm-output-font-lock-keywords))
  (setq-local eldoc-documentation-function #'goasm--eldoc-function))

(defun goasm--display-buffer (content &optional source-buf project-root arch)
  "Display CONTENT in the *goasm* buffer.
Creates the buffer if needed, replaces existing content.
SOURCE-BUF is the Go source buffer for reverse navigation.
PROJECT-ROOT is the project root for resolving relative paths.
ARCH is the target architecture string (e.g. \"amd64\")."
  (let ((buf (get-buffer-create goasm-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content))
      (goasm-output-mode)
      (when source-buf
        (setq goasm--source-buffer source-buf))
      (when project-root
        (setq goasm--project-root project-root))
      (when arch
        (setq goasm--arch arch))
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
             (arch (goasm--detect-arch))
             (display-text (if project-root
                               (goasm--relativize-paths asm-text project-root)
                             asm-text))
             (mapping (goasm--parse-line-mapping display-text)))
        (setq goasm--line-mapping mapping)
        (setq goasm--current-asm-function func-name)
        (goasm--display-buffer display-text (current-buffer) project-root arch)))))

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
