;;; elisp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package helpful
  :commands (helpful-at-point)
  :config
  (+pop '(major-mode . helpful-mode)))

(use-package elisp-demos
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
  (advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update))

(use-package elisp-def
  :hook
  (emacs-lisp-mode . elisp-def-mode))

(use-package pcre2el)

;;; elisp.el ends here.
