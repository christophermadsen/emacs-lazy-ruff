# emacs-lazy-ruff
This is a native (elisp) Emacs implementation for using the blazingly fast
[Ruff](https://docs.astral.sh/ruff) formatter/linter within Emacs through shell
commands. **Lazy-ruff** offers an (hopefully) easy approach to formatting and
linting your Python code with Ruff, whether it's within org-babel code blocks,
selected regions or on entire Python major mode buffers.

From my experience **lazy-ruff** operates MUCH faster than any other formatter
or linter that I've used (thanks to **Ruff** of course), but that is a purely
subjective experience as I have not run any benchmarks what-so-ever.

### Notice
I barely know any (emacs-)lisp, so if you enjoy the package then any
constructive criticism and contribution on further development would be very
much appreciated.

# Prerequisites
The **Ruff** command-line tool must be installed and be available in your
system's PATH, for more information see:
https://docs.astral.sh/ruff/installation/

# Installation
## Manually
You can download the `lazy-ruff.el` elisp file, put it in a place in your
`.emacs.d` directory and load the package by adding the following to your
`init.el` or a personalization file of your choice.

``` emacs-lisp
(add-to-list 'load-path "/path/to/lazy-ruff-directory")
(require 'lazy-ruff)
```

## Using use-package (WORK IN PROGRESS)
You can also install **lazy-ruff** using the
[use-package](https://github.com/jwiegley/use-package) package.

``` emacs-lisp
(use-package lazy-ruff)
```

I recommend directly adding the save hook and handy keybinding during the use-package call:

``` emacs-lisp
(use-package lazy-ruff
  :bind (("C-c f" . ruff-format-dispatch))
  :hook (python-mode . setup-ruff-save-hook))
```

# Usage
## Keybindings
Use the `ruff-format-dispatch` function for your keybinding, this function will
help automatically detect the context your cursor/pointer is in and apply the
best fitting **lazy-ruff** function. Here is an example:

``` emacs-lisp
(global-set-key (kbd "C-c f") 'ruff-format-dispatch)
```

You can of course also set up keybindings for each individual function
depending on your use-case, but if you use `ruff-format-dispatch`, let's say
for the `C-c f` keymap then the formatter/linter will figure out which function
to call depending on if you have a region marked, you're inside an org-babel
code block or you're operating on a whole Python major mode buffer.

## Format/lint on file save
**Lazy-ruff** provides a function for running the formatter/linter on save for
Python buffers/files, to use this add the following the your Emacs config:

``` emacs-lisp
;; Apply the setup-ruff-save-hook function to python-mode buffers.
(add-hook 'python-mode-hook 'setup-ruff-save-hook)
```

## Formatter and linter settings
Defaults have been provided in the package for the **Ruff** call, but you can
dynamically change these to fit your use-case. The defaults are:

``` emacs-lisp
(defvar ruff-check-command
  (concat "ruff check --fix --unsafe-fixes -s "
          "--preview "
          "--line-length=79 "
          "--select ALL "
          "--ignore E266,E402,E731,F403,F405,D100,D104,D401,T203,T201"))

(defvar ruff-only-format-block nil
  "When non-nil (e.g. t), only format the code in a block without linting fixes.")

(defvar ruff-only-format-buffer nil
  "When non-nil (e.g. t), only format the code in a buffer without linting fixes.")

(defvar ruff-only-format-region nil
  "When non-nil (e.g. t), only format the code in a region without linting fixes.")
```

Observe that `ruff-check-command` defines the call to **Ruff** on the CLI. The
defaults used may seem pretty crazy to you, but that is fine, you should modify
them to something that suits you. For the rules that you can select (and
ignore) have a look at [the Ruff
tutorial](https://docs.astral.sh/ruff/tutorial/) and available linting
[rules](https://docs.astral.sh/ruff/rules/).
