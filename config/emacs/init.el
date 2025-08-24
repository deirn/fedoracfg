;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(defun +load (file)
  "Load a FILE relative to `user-emacs-directory'."
  (load (expand-file-name file user-emacs-directory) t))

(+load "elpaca-init")

(defgroup +deirn nil
  "Deirn's custom group."
  :group 'emacs)

(+load "lisp/optimizations")
(+load "lisp/package-utils")
(+load "lisp/keybind-setup")
(+load "lisp/behaviour")
(+load "lisp/ui")
(+load "lisp/frame")
(+load "lisp/window")
(+load "lisp/child-frames")
(+load "lisp/popups")
(+load "lisp/buffer")
(+load "lisp/dashboard")
(+load "lisp/directory")
(+load "lisp/git")
(+load "lisp/terminal")
(+load "lisp/discord")
(+load "lisp/undo")
(+load "lisp/evil")
(+load "lisp/lsp")
(+load "lisp/language")
(+load "lisp/language-elisp")
(+load "lisp/edit-shortcuts")
(+load "lisp/completions")
(+load "lisp/debug")
(+load "lisp/formatting")

;;; init.el ends here.
