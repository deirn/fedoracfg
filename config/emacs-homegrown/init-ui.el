;;; init-ui.el -*- lexical-binding: t; -*-

(use-package emacs
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'relative)
  ; disable annoying bell sound
  (ring-bell-function 'ignore)
  ; less jumpy scroll
  (scroll-step 1)
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (auto-window-vscroll nil)
  (scroll-preserve-screen-position t)
  (window-divider-default-bottom-width 5)
  (window-divider-default-right-width 5)
  :hook
  (prog-mode . display-line-numbers-mode)
  :config
  ; disable builtin tool bars
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  ; highlight current line
  (global-hl-line-mode)
  (window-divider-mode))

;; set font
(set-face-attribute 'default nil
                    :family "JetBrainsMono Nerd Font"
                    :height 105)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :config (doom-modeline-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;;; init-ui.el ends here
