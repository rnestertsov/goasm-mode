;;; goasm-mode-test.el --- Tests for goasm-mode -*- lexical-binding: t; -*-
;; ABOUTME: ERT test suite for goasm-mode.
;; ABOUTME: Tests parsing, function detection, line mapping, and buffer display.

;;; Commentary:

;; Run with: emacs -batch -l goasm.el -l goasm-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'goasm-mode)

(defvar goasm-test-fixture-dir
  (expand-file-name "test/fixtures/"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing test fixtures.")

(defun goasm-test-fixture-path (name)
  "Return full path to fixture file NAME."
  (expand-file-name name goasm-test-fixture-dir))

(defun goasm-test-read-fixture (name)
  "Read fixture file NAME and return its contents as a string."
  (with-temp-buffer
    (insert-file-contents (goasm-test-fixture-path name))
    (buffer-string)))

(ert-deftest goasm-test-parse-functions-returns-alist ()
  "Parse functions returns an alist of (name . asm-text) pairs."
  (let* ((output (goasm-test-read-fixture "main-asm-output.txt"))
         (result (goasm--parse-functions output)))
    (should (assoc "main.Add" result))
    (should (assoc "main.Multiply" result))
    (should (assoc "main.main" result))))

(ert-deftest goasm-test-parse-functions-extracts-correct-body ()
  "Each function body contains its assembly lines."
  (let* ((output (goasm-test-read-fixture "main-asm-output.txt"))
         (result (goasm--parse-functions output))
         (add-asm (cdr (assoc "main.Add" result))))
    ;; The Add function should contain the ADD instruction
    (should (string-match-p "ADD" add-asm))
    ;; The Add function should NOT contain Multiply instructions
    (should-not (string-match-p "main\\.Multiply" add-asm))))

(ert-deftest goasm-test-parse-functions-excludes-non-stext ()
  "Parser only extracts STEXT function entries, not SRODATA/SDWARFCUINFO etc."
  (let* ((output (goasm-test-read-fixture "main-asm-output.txt"))
         (result (goasm--parse-functions output)))
    (should-not (assoc "gclocals·FzY36IO2mY0y4dZ1+Izd/w==" result))
    (should-not (assoc "main.Add.arginfo1" result))
    (should-not (assoc "go:cuinfo.producer.main" result))))

(ert-deftest goasm-test-parse-functions-empty-input ()
  "Parser returns nil for empty input."
  (let ((result (goasm--parse-functions "")))
    (should (null result))))

(ert-deftest goasm-test-current-function-name-inside-func ()
  "Detects function name when point is inside a function body."
  (with-temp-buffer
    (insert "package main\n\nfunc Add(a, b int) int {\n\treturn a + b\n}\n")
    (goto-char (point-min))
    (search-forward "return")
    (should (equal (goasm--current-function-name) "Add"))))

(ert-deftest goasm-test-current-function-name-on-func-line ()
  "Detects function name when point is on the func declaration line."
  (with-temp-buffer
    (insert "package main\n\nfunc Add(a, b int) int {\n\treturn a + b\n}\n")
    (goto-char (point-min))
    (search-forward "func Add")
    (should (equal (goasm--current-function-name) "Add"))))

(ert-deftest goasm-test-current-function-name-outside-func ()
  "Returns nil when point is outside any function."
  (with-temp-buffer
    (insert "package main\n\nfunc Add(a, b int) int {\n\treturn a + b\n}\n")
    (goto-char (point-min))
    (end-of-line)
    (should (null (goasm--current-function-name)))))

(ert-deftest goasm-test-current-function-name-method ()
  "Detects method name with receiver."
  (with-temp-buffer
    (insert "package main\n\ntype Calc struct{}\n\nfunc (c Calc) Add(a, b int) int {\n\treturn a + b\n}\n")
    (goto-char (point-min))
    (search-forward "return")
    (should (equal (goasm--current-function-name) "Add"))))

(ert-deftest goasm-test-current-function-name-multiple-funcs ()
  "Detects the correct function when multiple exist."
  (with-temp-buffer
    (insert "package main\n\nfunc Add(a, b int) int {\n\treturn a + b\n}\n\nfunc Sub(a, b int) int {\n\treturn a - b\n}\n")
    (goto-char (point-min))
    (search-forward "a - b")
    (should (equal (goasm--current-function-name) "Sub"))))

(ert-deftest goasm-test-parse-line-mapping-basic ()
  "Builds a mapping from source lines to assembly buffer lines."
  (let* ((asm-text (concat
                    "main.Add STEXT size=16\n"
                    "\t0x0000 00000 (/tmp/foo.go:3)\tTEXT\tmain.Add(SB)\n"
                    "\t0x0000 00000 (/tmp/foo.go:4)\tADD\tR1, R0, R0\n"
                    "\t0x0004 00004 (/tmp/foo.go:4)\tRET\t(R30)\n"))
         (mapping (goasm--parse-line-mapping asm-text)))
    ;; Source line 3 should map to buffer line 2 (1-indexed, line 1 is the header)
    (should (assoc 3 mapping))
    ;; Source line 4 should map to buffer lines 3 and 4
    (should (assoc 4 mapping))
    (should (= 2 (length (cdr (assoc 4 mapping)))))))

(ert-deftest goasm-test-parse-line-mapping-from-fixture ()
  "Line mapping works with real compiler output."
  (let* ((output (goasm-test-read-fixture "main-asm-output.txt"))
         (funcs (goasm--parse-functions output))
         (multiply-asm (cdr (assoc "main.Multiply" funcs)))
         (mapping (goasm--parse-line-mapping multiply-asm)))
    ;; Multiply starts at line 7, loop at line 9, body at 10, return at 12
    (should (assoc 7 mapping))
    (should (assoc 9 mapping))
    (should (assoc 10 mapping))
    (should (assoc 12 mapping))))

(ert-deftest goasm-test-parse-line-mapping-empty ()
  "Returns nil for empty input."
  (should (null (goasm--parse-line-mapping ""))))

(ert-deftest goasm-test-run-compiler-success ()
  "Compiler returns assembly output for valid Go package."
  (let ((output (goasm--run-compiler (goasm-test-fixture-path ""))))
    (should (stringp output))
    (should (string-match-p "STEXT" output))))

(ert-deftest goasm-test-run-compiler-returns-errors ()
  "Compiler returns error text for invalid Go code."
  (let ((temp-dir (make-temp-file "goasm-test" t)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "bad.go" temp-dir)
            (insert "package main\n\nfunc broken( {\n}\n"))
          (with-temp-file (expand-file-name "go.mod" temp-dir)
            (insert "module goasm-test-bad\n\ngo 1.21\n"))
          (let ((output (goasm--run-compiler temp-dir)))
            (should (stringp output))
            ;; Should contain error information, not STEXT
            (should-not (string-match-p "STEXT" output))))
      (delete-directory temp-dir t))))

(ert-deftest goasm-test-output-mode-is-read-only ()
  "The *goasm* buffer uses goasm-output-mode and is read-only."
  (let ((buf (get-buffer-create "*goasm*")))
    (unwind-protect
        (progn
          (goasm--display-buffer "some assembly text\n")
          (with-current-buffer buf
            (should (eq major-mode 'goasm-output-mode))
            (should buffer-read-only)))
      (kill-buffer buf))))

(ert-deftest goasm-test-display-buffer-replaces-content ()
  "Calling display-buffer twice replaces content rather than appending."
  (let ((buf (get-buffer-create "*goasm*")))
    (unwind-protect
        (progn
          (goasm--display-buffer "first content\n")
          (goasm--display-buffer "second content\n")
          (with-current-buffer buf
            (should (string-match-p "second content" (buffer-string)))
            (should-not (string-match-p "first content" (buffer-string)))))
      (kill-buffer buf))))

(ert-deftest goasm-test-display-buffer-creates-buffer ()
  "display-buffer creates *goasm* if it doesn't exist."
  (when (get-buffer "*goasm*")
    (kill-buffer "*goasm*"))
  (unwind-protect
      (progn
        (goasm--display-buffer "test content\n")
        (should (get-buffer "*goasm*"))
        (with-current-buffer "*goasm*"
          (should (string-match-p "test content" (buffer-string)))))
    (when (get-buffer "*goasm*")
      (kill-buffer "*goasm*"))))

(ert-deftest goasm-test-show-produces-assembly ()
  "goasm-show populates *goasm* with assembly for the current function."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          (should (get-buffer "*goasm*"))
          (with-current-buffer "*goasm*"
            ;; Should contain Add's assembly
            (should (string-match-p "main\\.Add" (buffer-string)))
            ;; Should NOT contain Multiply's full assembly
            (should-not (string-match-p "main\\.Multiply STEXT" (buffer-string)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-show-outside-function ()
  "goasm-show signals an error when not inside a function."
  (with-temp-buffer
    (insert "package main\n\nvar x = 42\n")
    (setq buffer-file-name "/tmp/fake.go")
    (goto-char (point-min))
    (should-error (goasm-show) :type 'user-error)))

(ert-deftest goasm-test-goto-line-jumps-to-assembly ()
  "goasm-goto-line scrolls *goasm* to assembly for the current source line."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          ;; Now jump to the line with "return a + b" (line 4)
          (goasm-goto-line)
          (with-current-buffer "*goasm*"
            ;; Point should be on a line containing source line 4
            (let ((current-line (buffer-substring
                                 (line-beginning-position)
                                 (line-end-position))))
              (should (string-match-p ":4)" current-line)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-goto-line-no-mapping ()
  "goasm-goto-line signals error when not inside a function."
  (with-temp-buffer
    (setq goasm--line-mapping nil)
    (should-error (goasm-goto-line) :type 'user-error)))

(ert-deftest goasm-test-goto-line-auto-compiles ()
  "goasm-goto-line compiles automatically when no assembly exists."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          ;; Call goto-line WITHOUT calling goasm-show first
          (goasm-goto-line)
          (should (get-buffer "*goasm*"))
          (with-current-buffer "*goasm*"
            (should (string-match-p "main\\.Add" (buffer-string)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-goto-line-recompiles-on-function-change ()
  "goasm-goto-line recompiles when cursor moves to a different function."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          ;; First, compile Add
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          (with-current-buffer "*goasm*"
            (should (string-match-p "main\\.Add" (buffer-string))))
          ;; Now move to Multiply and call goto-line — should auto-recompile
          (goto-char (point-min))
          (search-forward "result += a")
          (goasm-goto-line)
          (with-current-buffer "*goasm*"
            (should (string-match-p "main\\.Multiply" (buffer-string)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-minor-mode-activates ()
  "goasm-minor-mode can be activated in a buffer."
  (with-temp-buffer
    (goasm-minor-mode 1)
    (should goasm-minor-mode)))

(ert-deftest goasm-test-minor-mode-keybindings ()
  "goasm-minor-mode binds expected keys."
  (with-temp-buffer
    (goasm-minor-mode 1)
    (should (eq (key-binding (kbd "C-c C-a")) 'goasm-show))
    (should (eq (key-binding (kbd "C-c C-l")) 'goasm-goto-line))))

(ert-deftest goasm-test-minor-mode-deactivates ()
  "goasm-minor-mode can be deactivated."
  (with-temp-buffer
    (goasm-minor-mode 1)
    (goasm-minor-mode -1)
    (should-not goasm-minor-mode)))

(ert-deftest goasm-test-find-project-root ()
  "Finds the directory containing go.mod."
  (let ((root (goasm--find-project-root (goasm-test-fixture-path ""))))
    (should root)
    (should (file-exists-p (expand-file-name "go.mod" root)))))

(ert-deftest goasm-test-find-project-root-returns-nil ()
  "Returns nil when no go.mod exists above directory."
  (should (null (goasm--find-project-root "/tmp"))))

(ert-deftest goasm-test-relativize-paths ()
  "Replaces absolute paths with paths relative to project root."
  (let* ((asm-text "\t0x0000 00000 (/Users/foo/myproject/cmd/main.go:3)\tTEXT\n")
         (result (goasm--relativize-paths asm-text "/Users/foo/myproject/")))
    (should (string-match-p "(cmd/main.go:3)" result))
    (should-not (string-match-p "/Users/foo" result))))

(ert-deftest goasm-test-relativize-paths-preserves-non-path-parens ()
  "Relativizing paths does not affect non-source-ref parentheses."
  (let* ((asm-text "\t0x0000 00000 (/Users/foo/proj/main.go:4)\tRET\t(R30)\n")
         (result (goasm--relativize-paths asm-text "/Users/foo/proj/")))
    (should (string-match-p "(R30)" result))
    (should (string-match-p "(main.go:4)" result))))

(ert-deftest goasm-test-show-displays-relative-paths ()
  "goasm-show displays relative paths in assembly output."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          (with-current-buffer "*goasm*"
            ;; Should contain relative path like (main.go:4) not absolute
            (should (string-match-p "(main\\.go:" (buffer-string)))
            ;; Should NOT contain the absolute path prefix
            (should-not (string-match-p "(/" (buffer-string)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-parse-functions-malformed-input ()
  "Parser handles malformed input gracefully."
  (let ((result (goasm--parse-functions "not assembly output at all\nrandom lines\n")))
    (should (null result))))

(ert-deftest goasm-test-parse-functions-partial-output ()
  "Parser handles output that starts mid-function."
  (let* ((partial "\t0x0000 00000 (foo.go:1)\tMOVQ\tAX, BX\nmain.Add STEXT size=16\n\t0x0000 00000 (foo.go:3)\tRET\n")
         (result (goasm--parse-functions partial)))
    ;; Should still parse main.Add
    (should (assoc "main.Add" result))))

(ert-deftest goasm-test-line-mapping-no-source-refs ()
  "Line mapping handles assembly with no source references."
  (let ((mapping (goasm--parse-line-mapping "main.Add STEXT size=16\n\t0x0000 pure-instruction\n")))
    (should (null mapping))))

(ert-deftest goasm-test-end-to-end-workflow ()
  "Full workflow: open Go file, show assembly, navigate to line."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goasm-minor-mode 1)
          ;; Move to Multiply function body (line 10: result += a)
          (goto-char (point-min))
          (search-forward "result += a")
          ;; Generate assembly
          (goasm-show)
          (should (get-buffer "*goasm*"))
          (with-current-buffer "*goasm*"
            (should (string-match-p "main\\.Multiply" (buffer-string))))
          ;; Navigate to source line
          (goasm-goto-line)
          (with-current-buffer "*goasm*"
            ;; Should have highlights
            (should (cl-some (lambda (ov)
                               (eq (overlay-get ov 'face) 'goasm-highlight-face))
                             (overlays-in (point-min) (point-max))))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-goto-line-redisplays-after-quit ()
  "goasm-goto-line re-displays *goasm* buffer when its window was closed."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          ;; Show assembly — creates and displays *goasm* buffer
          (goasm-goto-line)
          (should (get-buffer-window "*goasm*"))
          ;; Simulate pressing q: bury buffer and delete its window
          (let ((win (get-buffer-window "*goasm*")))
            (when win (delete-window win)))
          (should-not (get-buffer-window "*goasm*"))
          ;; Call goto-line again for the SAME function
          (goasm-goto-line)
          ;; Buffer should be visible again
          (should (get-buffer-window "*goasm*")))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-output-mode-keybinding ()
  "C-c C-l is bound to goasm-goto-source in goasm-output-mode."
  (with-temp-buffer
    (goasm-output-mode)
    (should (eq (key-binding (kbd "C-c C-l")) 'goasm-goto-source))))

(ert-deftest goasm-test-goto-source-parses-reference ()
  "goasm-goto-source jumps to the correct source line from assembly."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          ;; Switch to the *goasm* buffer and find a line with source ref
          (with-current-buffer "*goasm*"
            (goto-char (point-min))
            ;; Find a line referencing main.go:4 (the return line)
            (re-search-forward "main\\.go:4)")
            (beginning-of-line)
            (goasm-goto-source))
          ;; Should have jumped to line 4 in the source buffer
          (with-current-buffer (get-file-buffer go-file)
            (should (= 4 (line-number-at-pos)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-goto-source-no-reference ()
  "goasm-goto-source signals error when no source reference on current line."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goto-char (point-min))
          (search-forward "return a + b")
          (goasm-show)
          ;; Switch to the *goasm* buffer, go to the header line (no source ref)
          (with-current-buffer "*goasm*"
            (goto-char (point-min))
            ;; First line is function header: "main.Add STEXT size=..."
            (should-error (goasm-goto-source) :type 'user-error)))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

(ert-deftest goasm-test-goto-source-round-trip ()
  "Round-trip: source -> assembly -> source navigates correctly."
  (when (get-buffer "*goasm*") (kill-buffer "*goasm*"))
  (when (get-file-buffer (goasm-test-fixture-path "main.go"))
    (kill-buffer (get-file-buffer (goasm-test-fixture-path "main.go"))))
  (let ((go-file (goasm-test-fixture-path "main.go")))
    (unwind-protect
        (progn
          (find-file go-file)
          (goasm-minor-mode 1)
          ;; Start at source line 4: "return a + b"
          (goto-char (point-min))
          (search-forward "return a + b")
          (should (= 4 (line-number-at-pos)))
          ;; Jump source -> assembly
          (goasm-goto-line)
          ;; Now in the assembly buffer, jump back to source
          (with-current-buffer "*goasm*"
            ;; Point should be on a line with :4) reference
            (let ((current-line (buffer-substring
                                 (line-beginning-position)
                                 (line-end-position))))
              (should (string-match-p ":4)" current-line)))
            (goasm-goto-source))
          ;; Should be back on line 4 of the source file
          (with-current-buffer (get-file-buffer go-file)
            (should (= 4 (line-number-at-pos)))))
      (when (get-buffer go-file) (kill-buffer (get-file-buffer go-file)))
      (when (get-buffer "*goasm*") (kill-buffer "*goasm*")))))

;;; Instruction parsing and documentation tests

(ert-deftest goasm-test-parse-instruction-from-asm-line ()
  "Extracts mnemonic from a standard assembly line."
  (should (equal "MOVQ"
                 (goasm--parse-instruction
                  "\t0x0000 00000 (main.go:3)\tMOVQ\tAX, BX"))))

(ert-deftest goasm-test-parse-instruction-no-operands ()
  "Extracts mnemonic from an instruction with no operands."
  (should (equal "RET"
                 (goasm--parse-instruction
                  "\t0x0004 00004 (main.go:4)\tRET"))))

(ert-deftest goasm-test-parse-instruction-returns-nil-for-header ()
  "Returns nil for a function header line."
  (should (null (goasm--parse-instruction
                 "main.Add STEXT size=16 args=0x10"))))

(ert-deftest goasm-test-parse-instruction-returns-nil-for-hex-dump ()
  "Returns nil for hex dump lines."
  (should (null (goasm--parse-instruction
                 "\t0x0000 48 8b 44 24 08"))))

(ert-deftest goasm-test-parse-instruction-returns-nil-for-empty ()
  "Returns nil for empty string."
  (should (null (goasm--parse-instruction ""))))

(ert-deftest goasm-test-instruction-doc-exact-match ()
  "Returns description for a known instruction."
  (should (stringp (goasm--instruction-doc "MOV"))))

(ert-deftest goasm-test-instruction-doc-suffix-stripping ()
  "Returns description for MOVQ by stripping Q suffix to find MOV."
  (should (stringp (goasm--instruction-doc "MOVQ"))))

(ert-deftest goasm-test-instruction-doc-unknown ()
  "Returns nil for an unknown instruction."
  (should (null (goasm--instruction-doc "ZZZNONSENSE"))))

(ert-deftest goasm-test-instruction-doc-pseudo-instruction ()
  "Returns description for Go pseudo-instructions."
  (should (stringp (goasm--instruction-doc "TEXT")))
  (should (stringp (goasm--instruction-doc "FUNCDATA")))
  (should (stringp (goasm--instruction-doc "PCDATA"))))

(ert-deftest goasm-test-eldoc-function-returns-string ()
  "Eldoc function returns formatted string on an instruction line."
  (with-temp-buffer
    (insert "\t0x0000 00000 (main.go:3)\tMOVQ\tAX, BX\n")
    (goto-char (point-min))
    (should (stringp (goasm--eldoc-function)))))

(ert-deftest goasm-test-eldoc-function-returns-nil-for-header ()
  "Eldoc function returns nil on non-instruction lines."
  (with-temp-buffer
    (insert "main.Add STEXT size=16\n")
    (goto-char (point-min))
    (should (null (goasm--eldoc-function)))))

(ert-deftest goasm-test-detect-arch-returns-string ()
  "goasm--detect-arch returns a non-empty architecture string."
  (let ((arch (goasm--detect-arch)))
    (should (stringp arch))
    (should (not (string-empty-p arch)))))

(ert-deftest goasm-test-output-mode-keybinding-describe ()
  "C-c C-d is bound to goasm-describe-instruction in goasm-output-mode."
  (with-temp-buffer
    (goasm-output-mode)
    (should (eq (key-binding (kbd "C-c C-d")) 'goasm-describe-instruction))))

(ert-deftest goasm-test-describe-instruction-errors-on-header ()
  "goasm-describe-instruction signals error on non-instruction lines."
  (with-temp-buffer
    (insert "main.Add STEXT size=16\n")
    (goasm-output-mode)
    (goto-char (point-min))
    (should-error (goasm-describe-instruction) :type 'user-error)))

(ert-deftest goasm-test-output-mode-enables-eldoc ()
  "goasm-output-mode sets eldoc-documentation-function."
  (with-temp-buffer
    (goasm-output-mode)
    (should (eq (buffer-local-value 'eldoc-documentation-function (current-buffer))
                #'goasm--eldoc-function))))

;;; goasm-test.el ends here
