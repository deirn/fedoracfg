;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "30")
  (error "This requires Emacs 30 and above!"))

(load (expand-file-name "bootstrap-elpaca" user-emacs-directory))

;; language modes
(use-package markdown-mode)
(use-package dart-mode)

(defun -my/enable-lsp-for (modes)
  (dolist (mode modes)
    (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'-my/enable-lsp-bridge)
    (add-hook (intern (concat (symbol-name mode) "-ts-mode-hook")) #'-my/enable-lsp-bridge)))

(-my/enable-lsp-for '(dart typescript))

(use-package nerd-icons)
(use-package transient)
(use-package magit)
(use-package helpful)
(use-package apheleia)

(use-package emacs
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'relative)

  ;; disable annoying bell sound
  (ring-bell-function 'ignore)

  ;; less jumpy scroll
  (scroll-step 1)                     ; scroll one line at a time
  (scroll-margin 2)                   ; start scrolling when 2 lines from bottom
  (scroll-conservatively 100000)      ; never recenter automatically
  (scroll-preserve-screen-position t) ; keep point position when scrolling
  (auto-window-vscroll nil)

  (window-divider-default-bottom-width 5)
  (window-divider-default-right-width 5)

  (indent-tabs-mode nil)
  (tab-width 4)
  (standard-indent 4)

  ;; disable backup `.#' and lockfiles `~'
  (create-lockfiles nil)
  (make-backup-files nil)

  :config
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 105)

  (fset #'yes-or-no-p #'y-or-n-p)

  ;; disable builtin tool bars
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)

  ;; highlight current line
  (global-hl-line-mode)

  ;; auto brackets closing
  (electric-pair-mode)

  (window-divider-mode)
  (which-key-mode)

  :hook
  (prog-mode . display-line-numbers-mode)
  (before-save . delete-trailing-whitespace))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :config
  (doom-modeline-mode))

(use-package solaire-mode
  :config
  (solaire-global-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :init
  (projectile-mode))

(use-package dirvish
  :custom
  (delete-by-moving-to-trash t)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons vc-state file-size file-time))
  (dirvish-side-attributes '(nerd-icons collapse vc-state file-size))

  :init
  (dirvish-override-dired-mode)

  :config
  (dirvish-side-follow-mode)

  :hook
  (dired-mode . dired-omit-mode))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)

  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode))

(use-package evil-nerd-commenter
  :after evil
  :init
  (evilnc-default-hotkeys))

(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook
  (prog-mode . combobulate-mode))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0.24)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match 'separator)

  :init
  (global-corfu-mode))

(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package consult
  :after vertico)

(use-package embark
  :after (vertico marginalia))

(use-package embark-consult
  :after (embark consult))

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :after (cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package flymake
  :hook
  (prog-mode . flymake-mode))

(add-to-list 'load-path (expand-file-name "share/lsp-bridge" (getenv "FEDORACFG")))
(use-package lsp-bridge
  :after (yasnippet markdown-mode orderless)
  :ensure nil
  :custom
  ;; use uv, so it has consistent package version
  (lsp-bridge-python-command "uv")
  (lsp-bridge-enable-hover-diagnostic t)
  ;; use flymake-bridge, see below
  (lsp-bridge-diagnostic-enable-overlays nil)

  ;; Use minibuffer instead of popup for code action
  (lsp-bridge-code-action-preview-delay nil)
  (lsp-bridge-code-action-enable-popup-menu nil)

  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-tabnine nil)
  (acm-candidate-match-function #'orderless-flex))

(use-package flymake-bridge
  :after flymake
  :ensure (:host github :repo "eki3z/flymake-bridge" :main nil))

(defun my/toggle-lsp-bridge ()
  "Toggle lsp-bridge to current buffer."
  (interactive)
  (if (bound-and-true-p lsp-bridge-mode)
      (-my/disable-lsp-bridge)
    (-my/enable-lsp-bridge)))

(defun -my/enable-lsp-bridge ()
  (lsp-bridge-mode 1)
  (corfu-mode -1)
  (flymake-bridge-setup)
  (message "Enabled lsp-bridge, disabled corfu"))

(defun -my/disable-lsp-bridge ()
  (lsp-bridge-mode -1)
  (corfu-mode 1)
  (message "Enabled corfu, disabled lsp-bridge"))

(defun my/has-lsp ()
  "Return whether the current buffer has LSP server."
  (and (bound-and-true-p lsp-bridge-mode)
       (lsp-bridge-has-lsp-server-p)))

(defun my/format-buffer ()
  "Format buffer using apheleia or lsp-bridge."
  (interactive)
  (if (my/has-lsp)
      (call-interactively #'lsp-bridge-code-format)
    (call-interactively #'apheleia-format-buffer)))

(defun my/show-documentation ()
  "Show documentation at point."
  (interactive)
  (if (my/has-lsp)
      (call-interactively #'lsp-bridge-show-documentation)
    (call-interactively #'helpful-at-point)))

(use-package general
  :config
  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    "b"     '(:ignore t                        :which-key "buffers")
    "b b"   '(switch-to-buffer                 :which-key "switch buffer")
    "b k"   '(magit-kill-this-buffer           :which-key "kill buffer")
    "b n"   '(next-buffer                      :which-key "next buffer")
    "b p"   '(previous-buffer                  :which-key "previous buffer")

    "f"     '(:ignore t                        :which-key "files")
    "f f"   '(find-file                        :which-key "find file")
    "f s"   '(save-buffer                      :which-key "save file")
    "f r"   '(recentf-open-files               :which-key "recent files")

    "g"     '(:ignore t                        :which-key "git")
    "g g"   '(magit-status                     :which-key "magit status")

    "c"     '(:ignore t                        :which-key "code")
    "c a"   '(lsp-bridge-code-action           :which-key "action")
    "c d"   '(flymake-show-buffer-diagnostics  :which-key "diagnostics")
    "c D"   '(flymake-show-project-diagnostics :which-key "project diagnostics")
    "c f"   '(my/format-buffer                 :which-key "format")
    "c r"   '(lsp-bridge-rename                :which-key "rename symbol")

    "p"     '(:ignore t                        :which-key "project")
    "p a"   '(projectile-add-known-project     :which-key "switch project")
    "p f"   '(projectile-find-file             :which-key "project file")
    "p p"   '(projectile-switch-project        :which-key "switch project")

    "w"     '(:ignore t                        :which-key "window")
    "w h"   '(evil-window-left                 :which-key "left")
    "w j"   '(evil-window-down                 :which-key "down")
    "w k"   '(evil-window-up                   :which-key "up")
    "w l"   '(evil-window-right                :which-key "right")
    "w m"   '(delete-other-windows             :which-key "maximize")
    "w q"   '(delete-window                    :which-key "kill window")

    "w s"   '(:ignore t                        :which-key "window split")
    "w s h" '(split-window-right               :which-key "horizontal")
    "w s v" '(split-window-below               :which-key "vertical")

    "t"     '(:ignore t                        :which-key "toggle")
    "t n"   '(display-line-numbers-mode        :which-key "line numbers")
    "t l"   '(my/toggle-lsp-bridge             :which-key "lsp")
    )

  (general-def
    :states 'normal
    :keymaps 'override
    "K" #'my/show-documentation
    )

  (general-def
    :states 'visual
    :keymaps 'override
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg
    )
  )

;;; init.el ends here
