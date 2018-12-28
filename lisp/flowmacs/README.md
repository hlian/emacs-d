<div align="center">
  <h1>flowmacs</h1>
  <i>Emacs minor mode to help you check your flow types</i>
</div>

### Installation

Clone the package:

```
$ git clone https://github.com/CodyReichert/flowmacs /path/to/flowmacs
```

Enable with your mode of choice:

```elisp
(add-to-list 'load-path "/path/to/flowmacs/")

(require 'flowmacs)

(add-hook 'web-mode-hook 'flowmacs-mode)
```

##### Custom `flow-bin`

Update the `flowmacs/+flow+` variable to an absolute path to your flow
binary. For example, to use `flow-bin` from the local node_modules:

```lisp
(defun my/set-flowmacs-flow ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory) "node_modules"))
         (flow (and root (expand-file-name "node_modules/.bin/flow" root))))
    (when (and flow (file-executable-p flow))
      ;; Set binary path
      (setq-local flowmacs/+flow+ flow))))

(add-hook 'flowmacs-hook 'my/set-flowmacs-flow)
```

### Features

Custom variables:

- `flowmacs/+flow+`: Absolute path to the flow binary
- `flowmacs/+flow-buffer+`: Name of the buffer to print flow output.

Flowmacs exposes a few helpful functions:

- `(flowmacs/start)`: Start flow
- `(flowmacs/stop)`: Stop flow
- `(flowmacs/status)`: Run flow status and print errors to the Flow
  Output buffer.
- `(flowmacs/type-at-pos)`: Print the type signature of the value or
  function under point to the minibuffer.
- `(flowmacs/find-refs)`: Print a list of references to the value or
  function under point.
- `(flowmacs/suggest-types)`: Insert types suggested by flow into the
  current buffer.
- `(flowmacs/jump-to-def)`: Jump to the definition of the value of
  function under point.

### Contributing

Please do!

- [ ] Add support for more flow cli commands
- [ ] Improve `shell-command-to-string` to use $PATH
- [ ] Submit recipe to mela

### License

MIT

---

> Let's connect:  &nbsp;&middot;&nbsp;
> [Medium](http://medium.com/@CodyReichert) &nbsp;&middot;&nbsp;
> [GitHub](https://github.com/assertible) &nbsp;&middot;&nbsp;
> [Twitter](https://twitter.com/CodyReichert)
