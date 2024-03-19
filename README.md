<!-- TOC was generated using markdown-toc package for Emacs -->
<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [emacs-lazy-ruff](#emacs-lazy-ruff)
    - [Prerequisites](#prerequisites)
    - [TL;DR Lazy Kickstart](#tldr-lazy-kickstart)
    - [Installation](#installation)
        - [With `use-package` (Recommended)](#with-use-package-recommended)
        - [Manual Install](#manual-install)
    - [Usage](#usage)
        - [Keybindings](#keybindings)
        - [Format/Lint Automatically On Save](#formatlint-automatically-on-save)
        - [Formatter and Linter Settings](#formatter-and-linter-settings)
        - [I Only Want to Use the Ruff Formatter, Not the Linter](#i-only-want-to-use-the-ruff-formatter-not-the-linter)

<!-- markdown-toc end -->
# emacs-lazy-ruff

`Lazy-ruff` is an Emacs integration of the blazingly fast [ruff
formatter/linter](https://docs.astral.sh/ruff) simply using shell
commands. `lazy-ruff` offers an (hopefully) easy approach to formatting and
linting your Python code with `ruff`, whether it's within `org-babel` code
blocks, specific marked regions or on entire Python major mode buffers.

From my experience `lazy-ruff` operates MUCH faster than any other formatter or
linter that I've used so far (all credits to `ruff` of course), but that is a
purely subjective experience, since I have not run any benchmarks what-so-ever.

I hope this package will help your workflow.

### A notice to potential users
I barely know any (emacs-)lisp, so if you enjoy the package, then any
constructive criticism or contribution to further development would be very
much appreciated :-)

## Prerequisites
The `ruff` command-line tool must be installed and be available in your
system's PATH, for more information see:
https://docs.astral.sh/ruff/installation/

If you have `pip` or `uv` on your system, a simple call on the shell should
suffice:

``` shell
uv pip install ruff
```

## TL;DR Lazy Kickstart

Add the following to your init.el (or other personalization file):

``` emacs-lisp
(use-package lazy-ruff
  :ensure t
  :bind (("C-c f" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config
  (lazy-ruff-mode-global-toggle t)) ;; Enable the lazy-ruff minor mode globally
```

If you don't want to use the `ruff` linter and only use the `ruff`
formatter, [go to this
section](#i-only-want-to-use-the-ruff-formatter-not-the-linter).

## Installation
### With `use-package` (Recommended)

If you have [MELPA](https://melpa.org/#/) added to your Emacs config and
[`use-package`](https://github.com/jwiegley/use-package) installed, you can
install `lazy-ruff` with `use-package`and its `:ensure t` directive:

``` emacs-lisp
(use-package lazy-ruff
    :ensure t) ;; Ensure the package is fetched from MELPA
```

I recommend directly enabling the `lazy-ruff` minor mode globally and adding
the handy keybinding `C-c f` for the *dwim* method during the `use-package`
call:

``` emacs-lisp
(use-package lazy-ruff
  :ensure t
  :bind (("C-c f" . lazy-ruff-lint-format-dwim)) ;; keybinding
  :config
  (lazy-ruff-mode-global-toggle t)) ;; Enable the lazy-ruff minor mode globally
```

### Manual Install
Simply install lazy-ruff with `M-x package-install RET lazy-ruff RET`

or

You can download the `lazy-ruff.el` elisp file, put it in a place in your
`.emacs.d` directory and load the package by adding the following to your
`init.el` or a personalization file of your choice.
``` emacs-lisp
(add-to-list 'load-path "/path/to/lazy-ruff-directory") ;; Only needed if the directory for lazy-ruff.el isn't yet on the load-path
(require 'lazy-ruff)
```

## Usage
### Keybindings

Use the `lazy-ruff-lint-format-dwim` method for your keybinding, this function
will help automatically detect the context your cursor/pointer is in and apply
the best fitting `lazy-ruff` method. Here is an example:

``` emacs-lisp
(global-set-key (kbd "C-c f") 'lazy-ruff-lint-format-dwim)
```

You can also set up keybindings for each individual method depending on your
use-case, but if you use the `lazy-ruff-lint-format-dwim` method then
`lazy-ruff` will automatically know which method to call depending on the
context your cursor/pointer is in (an `org-babel` code block, a
marked/highlighted region, a Python major mode buffer).

### Format/Lint Automatically On Save
`Lazy-ruff` provides a minor mode `lazy-ruff-mode` for automatically using the
formatter/linter *on save* for Python buffers/files. To use this minor mode
globally, add the following line to your Emacs config:

``` emacs-lisp
(lazy-ruff-mode-global-toggle t)
```

You can also toggle it on/off in a buffer by calling `M-x lazy-ruff-mode` or do
it globally by calling `M-x lazy-ruff-mode-global-toggle`

### Formatter and Linter Settings
Defaults have been provided in the package for the `ruff` shell command, but
you can dynamically change these to fit your own use-case. The defaults are:

``` emacs-lisp
;; Default settings
(defvar lazy-ruff-check-command
  (concat "ruff check --fix -s " ;; Make safe linter fixes and run silently
          "--select ALL "  ;; Start with enabling all rules
          "--ignore E266,E402,E731,F403,F405,D100,D104,D401,T203,T201"))  ;; Ignore rules for PEP 8 compliance

(defvar lazy-ruff-only-format-block nil
  "When non-nil (e.g. t), only format the code in a block without linting fixes.")

(defvar lazy-ruff-only-format-buffer nil
  "When non-nil (e.g. t), only format the code in a buffer without linting fixes.")

(defvar lazy-ruff-only-format-region nil
  "When non-nil (e.g. t), only format the code in a region without linting fixes.")
```

Observe that the `lazy-ruff-check-command` defines the <ins>linter</ins> call
to `ruff` on the CLI and the `lazy-ruff-format-command` defines the
<ins>formatter</ins> call. The defaults used may seem crazy to you, but that is
fine, you should modify them to something that suits you. For the rules that
you can select (and ignore) have a look at [the Ruff
tutorial](https://docs.astral.sh/ruff/tutorial/) and available linting
[rules](https://docs.astral.sh/ruff/rules/).

Here is an example of how you can change the call to `ruff check` (the linter,
specifically) using a much more [radical
config](https://docs.astral.sh/ruff/configuration/#full-command-line-interface)
including unsafe fixes, a max line length of 79 and preview rules enabled:

``` emacs-lisp
;; More radical settings
(setq lazy-ruff-check-command
  (concat "ruff check --fix --unsafe-fixes -s "
          "--preview "
          "--line-length=79 "
          "--select ALL "
          "--ignore E266,E402,E731,F403,F405,D100,D104,D401,T203,T201"))
```

Furthermore, removing the `-s` flag and changing it to `-v` or `-q` should open
a new buffer in your Emacs with the shell outputs of your `ruff` call.

### I Only Want to Use the Ruff Formatter, Not the Linter

If you don't want to include the `ruff` linter when using `lazy-ruff` on
code and you're only interested in using the formatter, then you can simply
update the following variables:

``` emacs-lisp
(setq lazy-ruff-only-format-block t) ;; Don't lint in code blocks
(setq lazy-ruff-only-format-region t) ;; Don't lint in a marked region
(setq lazy-ruff-only-format-buffer t) ;; Don't lint in Python major mode buffers
```

We can also add these changes immediately when calling `use-package` with the
`:config` directive:

``` emacs-lisp
(use-package lazy-ruff
  :bind (("C-c f" . lazy-ruff-lint-format-dwim))
  :config
  (setq lazy-ruff-only-format-block t) ;; Don't lint in code blocks
  (setq lazy-ruff-only-format-region t) ;; Don't lint in a marked region
  (setq lazy-ruff-only-format-buffer t)) ;; Don't lint in Python major mode buffers
```
