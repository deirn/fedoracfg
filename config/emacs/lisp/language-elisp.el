;;; lang-elisp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Emacs Lisp
;;; Code:

(use-package helpful
  :commands (helpful-at-point)
  :config
  (+pop '(major-mode . helpful-mode)))

;;; lang-elisp.el ends here.
