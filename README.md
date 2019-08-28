# counsel-test [![Build Status](https://travis-ci.com/xmagpie/counsel-test.svg?branch=master)](https://travis-ci.com/xmagpie/counsel-test) [![MELPA](https://melpa.org/packages/counsel-test-badge.svg)](https://melpa.org/#/counsel-test)
An emacs package that integrates ivy/counsel with different testing frameworks.

This package is inspired by
[helm-ctest](https://github.com/danlamanna/helm-ctest), huge thanks to the
author.

![demo](assets/demo.gif)

**Disclaimer**:
This package is still rather small and simple. It may contain
bugs and other errors. I have been using it for a half of a year already at my
work and it proved to be useful. Now I have decided that **counsel-test** can be
useful for someone else too. The idea is to have core generic functionality for
test discovery and execution in compilation buffer with a collection of
framework integrations on top of that. I plan to enhance this package with
support for other frameworks and add some helpful features in the
future. Feedback is highly appreciated!

# Installation

## [MELPA](https://melpa.org/)

<kbd>M-x package-install [RET] counsel-test [RET]</kbd> or

``` emacs-lisp
(use-package counsel-test
  :ensure t)
```

## Manual installation
First, clone this repo. Then, load the package with the method of your
choice (e.g. `load-path`/`require`, `use-package` etc). An example with
`use-package`:

``` emacs-lisp
(use-package counsel-test
  :load-path "~/.emacs.local/counsel-test")
```

# Usage
Currently, there is a basic support only for two testing frameworks: **ctest**
and **pytest**. There are two separate commands for them: `counsel-test-ctest`
and `counsel-test-pytest` respectively.

In your project it is sufficient to invoke one of them, say `M-x
counsel-test-ctest`. You will be prompted for the directory with ctest
tests. After this, you will see a list of the available tests. You can pick any
test or even a group of tests (with the help of `ivy-mark`). Note that the
directory you have selected won't be cached, so each subsequent call to
`counsel-test-ctest` will ask you for the directory again.

An alternative solution is to add `counsel-test-dir` variable to the project
`dir-locals.el`. This will remove the need to select the test directory manually
again and again or after emacs restarts or during switching between different
projects.

Manual test directory selection will still be available with the help of prefix
argument: `C-u M-x counsel-test-ctest`.

If something goes wrong during test discovery, an error message will be shown in
the echo area and you may view the discover function output in a dedicated
buffer: `*counsel-test-<program>-log*`.

## Customization
This package contains several variables that allow certain customization on the
per-project basis. They have sensible defaults but it is strongly recommended to
set some of them in `dir-locals.el` for each project separately.

#### Common variables
* `counsel-test-dir` - directory from where to run the test discovery and test
  execution commands. Default value is `nil`. When `nil` the user is asked for
  the directory interactively. This is the primary candidate for being a
  dir-local variable. **Note** that currently, when setting this in
  `dir-locals.el` you have to provide the **absolute** path. **Relative** paths
  **do not work** for now.

#### Ctest specific variables
* `counsel-test-ctest-env` - environment variable settings for the ctest tests
  **execution**. The default value is
  `'("CLICOLOR_FORCE=1" "CTEST_OUTPUT_ON_FAILURE=1")` to enable colors and
  test output only in case of failures. More variables can be found in
  [ctest docs](https://cmake.org/cmake/help/latest/manual/ctest.1.html).

* `counsel-test-ctest-cmd` - command that is used to run ctest, defaults to
  "ctest". If you wish to use the custom ctest binary, point this variable to
  it, e.g.  `(setq counsel-ctest-cmd "path/to/custom/ctest-bin)`.  Keep in mind
  that this variable value is forwarded to `call-process` as `program` argument,
  so it should be available on your system.

#### Pytest specific variables
* `counsel-test-pytest-env` - environment variable settings for pytest tests
  **discovery**. The default value is `nil`. This should be a list of
  `"ENV=value"` strings (see `process-environment`). This may be particularly
  handy if you need to update your `PYTHONPATH` before running `pytest`.
* `counsel-test-pytest-cmd` - command that is used to run pytest, defaults to
  "pytest". Can be altered similar to `counsel-test-ctest-cmd`.

## Integration with other frameworks
I plan to add support for other frameworks, but if you can't find what you need,
it is easy to implement the integration yourself. Don't forget to submit
pull-request with your implementation!

To add custom integration, you have to implement two functions:
1. `discover-func` - test discovery function. It does not accept any arguments
   and has to return a list of strings, representing possible tests
   (selections). These will be shown in minibuffer with `ivy`, so you may need
   to make them look user-friendly.
2. `create-cmd-func (selections)` - function that creates a test execution
   command from the provided selections. It has one argument `selections` which
   is a list of strings you pick in minibuffer with `ivy` (a subset from the
   `discover-func` results). This function should return a string representing a
   shell command that will be run in compilation buffer.

After providing these two implementations, say
`counsel-test-framework--discover` and `counsel-test-framework--create-cmd`, you
need to define your entry-point through call to `counsel-test` function:

``` emacs-lisp
(defun counsel-test-framework (arg)
  "Browse and execute framework tests.

With a prefix argument ARG also force prompt user for this directory."
  (interactive "P")

  (counsel-test 'counsel-test-framework--discover
                'counsel-test-framework--create-cmd
                'counsel-test-framework
                arg))
```

And then you are ready to go!

For concrete examples, refer to implementations in *counsel-test-ctest.el* and
*counsel-test-pytest.el*.

## Contributing
Please, feel free to ask any questions and submit bugs, issues or pull
requests. All contributions are welcome!
