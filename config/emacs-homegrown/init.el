;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "30")
  (error "This requires Emacs 30 and above!"))

(load (expand-file-name "bootstrap-elpaca" user-emacs-directory))

(use-package gcmh
  :config
  (gcmh-mode 1))

;; language modes
(use-package markdown-mode :mode "\\.md\\'")
(use-package dart-mode     :mode "\\.dart\\'")

(use-package nerd-icons)
(use-package transient :defer t)
(use-package apheleia :defer t)

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
  (global-hl-line-mode 1)

  ;; auto brackets closing
  (electric-pair-mode 1)

  (window-divider-mode 1)
  (which-key-mode 1)

  :hook
  (prog-mode . display-line-numbers-mode)
  (before-save . delete-trailing-whitespace))

(use-package doom-themes :defer t)
(use-package doom-modeline :defer t)
(use-package solaire-mode :defer t)
(use-package vi-tilde-fringe :defer t)

(run-with-idle-timer
 0.2 nil
 (lambda ()
   (load-theme 'doom-one t)
   (doom-modeline-mode 1)
   (solaire-global-mode 1)
   (global-vi-tilde-fringe-mode 1)))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package projectile
  :init
  (projectile-mode 1))

(use-package dashboard
  :after (projectile)
  :custom
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-projects-backend 'projectile)
  (dashboard-items '((recents  . 5)
                     (projects . 5)))
  (initial-buffer-choice 'dashboard-open)

  :hook
  (window-size-change-functions . my/schedule-dashboard-refresh)

  :config
  (defun my/dashboard-refresh ()
    (with-current-buffer "*dashboard*"
      (let ((inhibit-read-only t))
        (dashboard-insert-startupify-lists t))))

  (defvar my/dashboard-resize-timer nil
    "Idle timer for debounced dashboard refresh.")

  (defun my/schedule-dashboard-refresh (&optional _)
    "Schedule dashboard refresh after resize."
    (when my/dashboard-resize-timer
      (cancel-timer my/dashboard-resize-timer))
    (setq my/dashboard-resize-timer
          (run-with-idle-timer
           0.2 nil
           (lambda ()
             (when (get-buffer "*dashboard*")
               (my/dashboard-refresh)))))))

(use-package dirvish
  :custom
  (delete-by-moving-to-trash t)
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons vc-state file-size file-time))
  (dirvish-side-attributes '(nerd-icons collapse vc-state file-size))
  (dirvish-hide-details '(dirvish-side))
  (dirvish-reuse-session)

  :init
  (dirvish-override-dired-mode)

  :config
  (dirvish-side-follow-mode 1)
  (put 'dired-find-alternate-file 'disabled nil)

  :hook
  (dired-mode . dired-omit-mode))

(use-package diredfl
  :after dirvish
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode))

(use-package magit
  :commands (magit-status))

(use-package helpful
  :commands (helpful-at-point))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)

  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :init
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1)
  (evil-snipe-override-mode 1))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :init
  (evilnc-default-hotkeys))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (global-treesit-auto-mode 1))

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
  (global-corfu-mode 1))

(use-package vertico
  :config
  (vertico-mode 1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

(use-package consult)
(use-package embark)
(use-package embark-consult :after embark)

(use-package orderless
  :custom
  (completion-styles '(orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package yasnippet
  :defer t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets :after yasnippet :defer t)

(use-package cape
  :defer t
  :config
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :after (cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package flymake
  :hook
  (prog-mode . flymake-mode))

(use-package dape
  :commands (dape)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load))

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
  (acm-candidate-match-function #'orderless-flex)

  :hook
  (prog-mode . -my/enable-lsp-bridge))

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
      (progn
        (call-interactively #'lsp-bridge-code-format)
        (message "Formatted using lsp-bridge"))
    (progn
      (call-interactively #'apheleia-format-buffer)
      (message "Formatted using apheleia"))))

(defun my/show-documentation ()
  "Show documentation at point."
  (interactive)
  (if (my/has-lsp)
      (call-interactively #'lsp-bridge-show-documentation)
    (call-interactively #'helpful-at-point)))

(defun my/open-config ()
  "Open Emacs configuration."
  (interactive)
  (find-file (read-file-name "Find file in config: " user-emacs-directory)))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the current one and essential buffers."
  (interactive)
  (let* ((current (current-buffer))
         (target-name '("*dashboard*"
                        "*Help*"
                        "*Ibuffer*"))
         (target-mode '("Helpful"))
         (killed-count 0))
    (dolist (buf (buffer-list))
      (unless (eq buf current)
        (let ((name (buffer-name buf))
              (mode (buffer-local-value 'mode-name buf)))
          (when (or (and (not (string-prefix-p "*" name)) ; keep `*' and ` ' by default
                         (not (string-prefix-p " " name)))
                    (member name target-name)             ; delete explicit ones
                    (member mode target-mode))
            (kill-buffer buf)
            (cl-incf killed-count)
            (message "Killed %s" name)))))
    (message "Killed %d buffer(s)." killed-count)))

(defun my/reload-init ()
  "Reload Emacs init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "init.el reloaded!"))

(use-package general
  :config
  (general-def
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    "b"     '(:ignore t                        :which-key "buffer")
    "b b"   '(switch-to-buffer                 :which-key "switch")
    "b i"   '(ibuffer                          :which-key "switch")
    "b k"   '(magit-kill-this-buffer           :which-key "kill")
    "b K"   '(my/kill-other-buffers            :which-key "kill other buffer")
    "b n"   '(next-buffer                      :which-key "next")
    "b p"   '(previous-buffer                  :which-key "previous")
    "b r"   '(revert-buffer                    :which-key "revert")

    "f"     '(:ignore t                        :which-key "file")
    "f f"   '(find-file                        :which-key "find")
    "f s"   '(save-buffer                      :which-key "save")
    "f r"   '(recentf                          :which-key "recent files")

    "e"     '(:ignore t                        :which-key "emacs")
    "e c"   '(my/open-config                   :which-key "open config")
    "e r"   '(my/reload-init                   :which-key "reload init.el")

    "g"     '(:ignore t                        :which-key "git")
    "g g"   '(magit-status                     :which-key "magit status")

    "c"     '(:ignore t                        :which-key "code")
    "c a"   '(lsp-bridge-code-action           :which-key "action")
    "c d"   '(flymake-show-buffer-diagnostics  :which-key "diagnostics")
    "c D"   '(flymake-show-project-diagnostics :which-key "project diagnostics")
    "c f"   '(my/format-buffer                 :which-key "format")
    "c r"   '(lsp-bridge-rename                :which-key "rename symbol")

    "o"     '(:ignore t                        :which-key "open")
    "o d"   '(dashboard-open                   :which-key "dashboard")
    "o o"   '(dired                            :which-key "dired")
    "o p"   '(dirvish-side                     :which-key "project view")

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

  (general-def
    :keymaps 'dirvish-mode-map
    :states '(normal motion)
    "TAB" #'dirvish-subtree-toggle
    "h"   #'dired-up-directory
    "l"   #'dired-find-alternate-file
    "RET" #'dired-find-alternate-file
    )
  )

;;; init.el ends here
