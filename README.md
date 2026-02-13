# goasm-mode

An Emacs minor mode for viewing Go compiler assembly output. Generates Plan 9 pseudo-assembly scoped to the current function, with source-line-to-assembly navigation and syntax highlighting.

## Features

- **Per-function assembly** - Generates assembly output for the Go function at point using `go build -gcflags '-S'`
- **Source-to-assembly navigation** - Jump the assembly view to the instructions corresponding to the current source line
- **Assembly highlighting** - Matching assembly lines are highlighted when navigating from source
- **Syntax highlighting** - Assembly instructions, registers, hex addresses, and source references are font-locked in the output buffer
- **Auto-recompilation** - Moving to a different function and navigating automatically re-generates assembly

## Requirements

- Emacs 27.1+
- Go compiler (`go` on PATH)
- A Go project with a `go.mod` file

## Installation

### Manual

Clone the repository and add it to your load path:

```elisp
(add-to-list 'load-path "/path/to/goasm-mode")
(require 'goasm)
```

### use-package

```elisp
(use-package goasm
  :load-path "/path/to/goasm-mode"
  :hook (go-mode . goasm-minor-mode))
```

## Usage

Enable the minor mode in a Go buffer:

```
M-x goasm-minor-mode
```

Or add it to your `go-mode` hook:

```elisp
(add-hook 'go-mode-hook #'goasm-minor-mode)
```

### Key Bindings

| Key       | Command          | Description                                       |
|-----------|------------------|---------------------------------------------------|
| `C-c C-a` | `goasm-show`     | Generate assembly for the function at point        |
| `C-c C-l` | `goasm-goto-line`| Scroll assembly buffer to the current source line  |

### Workflow

1. Open a Go source file and enable `goasm-minor-mode`
2. Place your cursor inside a function
3. Press `C-c C-a` to generate and display the assembly in a `*goasm*` buffer
4. Move to different lines in your source and press `C-c C-l` to see the corresponding assembly instructions highlighted

## Customization

| Variable             | Default | Description                              |
|----------------------|---------|------------------------------------------|
| `goasm-go-command`   | `"go"`  | Path to the Go executable                |
| `goasm-build-flags`  | `nil`   | Additional flags passed to `go build`    |

## Development

### Running Tests

```bash
make test    # Run the ERT test suite
make lint    # Byte-compile and check for warnings
make clean   # Remove compiled files
```

## License

See [LICENSE](LICENSE) for details.

## Author

Roman Nestertsov
