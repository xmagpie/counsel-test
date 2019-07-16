# counsel-ctest
An emacs package that integrates ivy/counsel with ctest.

This package is inspired by
[helm-ctest](https://github.com/danlamanna/helm-ctest), huge thanks to the
author.

![demo](assets/demo.gif)

# Installation
This package is not yet in MELPA, so currently the only possible way is to
install it manually.

## Manual installation
First, clone this repo. Then, load the package with the method of your
choice (e.g. `load-path`/`require`, `use-package` etc). An example with
`use-package`:

``` emacs-lisp
(use-package counsel-ctest
  :load-path "~/.emacs.local/counsel-ctest"
  :custom
  (counsel-ctest-env "CLICOLOR_FORCE=1"))
```
