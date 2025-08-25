;;; completions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Completions
;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; (enable-recursive-minibuffers t)
  (read-file-name-completion-ignore-case t)
  (read-buffer-completion-ignore-case t)
  (completion-ignore-case t)
  :hook
  (+late . minibuffer-depth-indicate-mode)
  (+late . context-menu-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode text-mode markdown-mode))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep)
  (xref-prompt-for-identifier nil)
  :config
  (define-advice xref--show-xrefs (:before (&rest _) evil-jump-list)
    "Add to evil jump list before showing xrefs."
    (evil-set-jump)))

(use-package consult
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref))

(defun +consult-fd-with-dir ()
  "Run `consult-fd` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-fd)))

(defun +consult-ripgrep-with-dir ()
  "Run `consult-ripgrep` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-ripgrep)))

(use-package embark-consult :after embark)

(use-package vertico
  :after embark
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package nerd-icons-completion
  :hook
  (+late . nerd-icons-completion-mode)
  (marginalia-mode . #'nerd-icons-completion-marginalia-setup))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :defer t
  :hook
  (+late . yas-global-mode ))

(use-package yasnippet-snippets :after yasnippet :defer t)

(use-package imenu-list
  :custom
  (imenu-list-auto-update nil))

(use-package wgrep)

(map! spc
  "s b" '("buffer"       . consult-buffer)
  "s e" '("error"        . consult-flymake)
  "s f" '("fd project"   . consult-fd)
  "s F" '("fd directory" . +consult-fd-with-dir)
  "s i" '("imenu"        . consult-imenu)
  "s j" '("jump"         . evil-collection-consult-jump-list)
  "s l" '("line"         . consult-line)
  "s L" '("line multi"   . consult-line-multi)
  "s r" '("rg project"   . consult-ripgrep)
  "s R" '("rg directory" . +consult-ripgrep-with-dir))

(map! normal
  "g d" #'xref-find-definitions
  "g r" #'xref-find-references)

(map! nil
  :keymaps 'minibuffer-local-map
  "M-a" #'marginalia-cycle)

;;; completions.el ends here.
