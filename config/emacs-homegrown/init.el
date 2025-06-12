;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "29")
  (error "This requires Emacs 29 and above!"))

(load (expand-file-name "init-elpaca" user-emacs-directory))

(defmacro use-package! (name &rest rest)
  `(use-package ,name :ensure t ,@rest))


;;; UI

(use-package! doom-themes
  :config
  (load-theme 'doom-one t))

(use-package! doom-modeline
  :init (doom-modeline-mode 1))

;; disable builtin tool bars
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)

(setq display-line-numbers-grow-only t)   ;; prevents shrinking
(setq display-line-numbers-width-start t)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; highlight current line
(global-hl-line-mode)

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
      window-divider-default-right-width 5)

;;; Evil

(use-package! evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (evil-mode))

(use-package! evil-collection :config (evil-collection-init))
(use-package! vi-tilde-fringe :config (global-vi-tilde-fringe-mode))


;;; Minibuffer

(use-package! vertico :config (vertico-mode))
(use-package! marginalia :config (marginalia-mode))
(use-package! consult)
(use-package! embark)
(use-package! embark-consult)


;;; Magit

(use-package! transient)
(use-package! magit)


;;; Code

(use-package! flycheck :config (global-flycheck-mode))

;;; init.el ends here
