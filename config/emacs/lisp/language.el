;;; language.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Languages
;;; Code:

(defvar +--nobreadcrumb nil)
(defun +nobreadcrumb (mode)
  "Disable breadcrumb for MODE."
  (add-to-list '+--nobreadcrumb mode))

(defun +ts (language url &rest conf)
  "Add new treesit LANGUAGE source from URL and extra CONF."
  (add-to-list 'treesit-language-source-alist (cons language (cons url conf))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package fish-mode
  :mode "\\.fish\\'"
  :config
  (+nobreadcrumb 'fish-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-display-remote-images t)
  (markdown-max-image-size '(200 . nil))
  :config
  (+nobreadcrumb 'gfm-mode)
  :hook
  (gfm-view-mode . markdown-display-inline-images))

(use-package sh-script
  :ensure nil
  :config
  (+nobreadcrumb 'bash-ts-mode))

(use-package js
  :ensure nil
  :mode ("\\.[mc]?js\\'" . js-ts-mode))

(use-package systemd
  :ensure (:files (:defaults "*directives.txt"))
  :mode ("\\.service\\'" . systemd-mode)
  :init
  (add-hook 'systemd-mode-hook #'display-line-numbers-mode))

(use-package dart-ts-mode
  :ensure (:host github :repo "50ways2sayhard/dart-ts-mode")
  :mode "\\.dart\\'"
  :config
  (+ts 'dart "https://github.com/UserNobody14/tree-sitter-dart")
  (+lsp 'dart-ts-mode "dart-analysis-server"))

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :config
  (dolist (e svelte-ts-mode-language-source-alist) (apply '+ts e))
  (+lsp 'svelte-ts-mode "svelteserver"))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package text-mode
  :ensure nil
  :mode "readme\\'")

(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode))

;;; language.el ends here.
