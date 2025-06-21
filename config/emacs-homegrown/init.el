;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "30")
  (error "This requires Emacs 30 and above!"))

(load (expand-file-name "bootstrap-elpaca" user-emacs-directory))

; language modes
(use-package markdown-mode)
(use-package dart-mode)

; dependencies
(use-package nerd-icons)
(use-package transient)

(use-package emacs
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)    
  (display-line-numbers-width-start t)  
  (display-line-numbers-type 'relative) 
  
  ; disable annoying bell sound
  (ring-bell-function 'ignore)          

  ; less jumpy scroll
  (scroll-step 1)                     ; scroll one line at a time                       
  (scroll-margin 2)                   ; start scrolling when 2 lines from bottom 
  (scroll-conservatively 100000)      ; never recenter automatically
  (scroll-preserve-screen-position t) ; keep point position when scrolling  
  (auto-window-vscroll nil)   

  (window-divider-default-bottom-width 5)
  (window-divider-default-right-width 5)

  :config
  (set-face-attribute 'default nil
                      :family "JetBrainsMono Nerd Font"
                      :height 105)

  ; disable builtin tool bars
  (tool-bar-mode -1)   
  (scroll-bar-mode -1) 
  (menu-bar-mode -1)   
  
  ; highlight current line
  (global-hl-line-mode) 

  ; auto brackets closing
  (electric-pair-mode)

  (window-divider-mode)

  :hook
  (prog-mode . display-line-numbers-mode))

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
  :after evil)

(use-package vi-tilde-fringe
  :init
  (global-vi-tilde-fringe-mode))

(use-package flymake)

(use-package helpful)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate"))

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
  "Toggle lsp-bridge to current buffer"
  (interactive)
  (if (lsp-bridge-mode)
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

(dolist (mode '(dart typescript-ts))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            #'-my/enable-lsp-bridge))

(use-package which-key
  :ensure nil
  :init
  (which-key-mode))

(use-package magit)

;;; init.el ends here
