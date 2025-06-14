;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "30")
  (error "This requires Emacs 30 and above!"))

(defmacro init! (module)
  `(load (expand-file-name (concat "init-" (symbol-name ,module)) user-emacs-directory)))

(init! 'elpaca)

(init! 'ui)
(init! 'project)
(init! 'editor)
(init! 'minibuffer)

(init! 'magit)

;;; init.el ends here
