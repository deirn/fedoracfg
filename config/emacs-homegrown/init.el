;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29")
  (error "This requires Emacs 29 and above!"))

(load (expand-file-name "init-elpaca" user-emacs-directory))


;;; UI

(use-package doom-themes
  :ensure t
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode))

(use-package emacs
  :ensure nil
  :config

  ;; disable builtin tool bars
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  (setq display-line-numbers-grow-only t   ;; prevents shrinking
        display-line-numbers-width-start t
        display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode)

  ;; highlight current line
  (global-hl-line-mode)

  (which-key-mode)

  ;; disable bell sound
  (setq ring-bell-function 'ignore)

  ;; less jumpy scroll
  (setq scroll-step 1
        scroll-margin 0
        scroll-conservatively 100000
        auto-window-vscroll nil
        scroll-preserve-screen-position t)

  ;; set font
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 105)

  ;; Increase window divider size
  (setq window-divider-default-bottom-width 5
        window-divider-default-right-width 5))

;;; Evil

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package evil-collection :ensure t :config (evil-collection-init))
(use-package vi-tilde-fringe :ensure t :config (global-vi-tilde-fringe-mode))


;;; Minibuffer

(use-package vertico :ensure t :config (vertico-mode))
(use-package marginalia :ensure t :config (marginalia-mode))
(use-package consult :ensure t)
(use-package embark :ensure t)
(use-package embark-consult :ensure t)


;;; Magit

(use-package transient :ensure t)
(use-package magit :ensure t)


;;; Code

(use-package flymake :ensure t)

(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;;; init.el ends here
