;;; init.el -*- lexical-binding: t; -*-

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(load (expand-file-name "bootstrap-elpaca" user-emacs-directory))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))

(defcustom my/late-hook nil
  "Hook that runs after startup."
  :type 'hook)

;; language modes
(use-package markdown-mode :mode "\\.md\\'")
(use-package fish-mode     :mode "\\.fish\\'")

(use-package systemd
  :ensure (:files ("*.el" "*directives.txt"))
  :mode ("\\.service\\'" . systemd-mode)
  :init
  (add-hook 'systemd-mode-hook #'display-line-numbers-mode))

(use-package dart-ts-mode
  :ensure (:host github :repo "50ways2sayhard/dart-ts-mode")
  :after lsp-bridge
  :mode "\\.dart\\'"
  :hook (dart-ts-mode . lsp-bridge-mode)
  :config
  (add-to-list 'treesit-language-source-alist '(dart . ("https://github.com/UserNobody14/tree-sitter-dart")))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(dart-ts-mode . "dart-analysis-server")))

(use-package nerd-icons :defer t)
(use-package transient :defer t)
(use-package apheleia :defer t)

(use-package emacs
  :ensure nil
  :custom
  (use-dialog-box nil)
  (confirm-kill-emacs #'yes-or-no-p)

  (custom-file (expand-file-name "custom.el" user-emacs-directory))

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

  (line-spacing 0.03)

  ;; disable backup `.#' and lockfiles `~'
  (create-lockfiles nil)
  (make-backup-files nil)

  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-dont-use-unicode nil)
  (which-key-add-column-padding 2)

  (history-length 1000)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring
                                   regexp-search-ring
                                   extended-command-history))

  :config
  (set-face-attribute 'default nil
                      :family "JetBrains Mono"
                      :height 105)

  (fset #'yes-or-no-p #'y-or-n-p)
  :hook
  (prog-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (before-save . delete-trailing-whitespace)
  (my/late . save-place-mode)
  (my/late . savehist-mode)
  (my/late . global-hl-line-mode)
  (my/late . electric-pair-mode)
  (my/late . which-key-mode)
  (my/late . window-divider-mode))

(use-package doom-themes
  :hook
  (my/late . (lambda () (load-theme 'doom-one t))))

(use-package doom-modeline
  :hook
  (my/late . doom-modeline-mode))

(use-package solaire-mode
  :hook
  (my/late . solaire-global-mode))

(use-package vi-tilde-fringe
  :hook
  (my/late . global-vi-tilde-fringe-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

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
           0.5 nil
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

  :init
  (dirvish-override-dired-mode)

  :config
  (dirvish-side-follow-mode 1)
  (put 'dired-find-alternate-file 'disabled nil)
  (add-to-list 'dirvish-side-window-parameters '(my/dirvish-side . t))

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

;; discord rich presence
(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t)
  :hook
  (my/late . elcord-mode))

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
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook
  (prog-mode . combobulate-mode))

;; (use-package corfu
;;   :custom
;;   (corfu-cycle t)
;;   (corfu-auto t)
;;   (corfu-auto-delay 0.24)
;;   (corfu-auto-prefix 2)
;;   (corfu-quit-no-match 'separator)

;;   :init
;;   (global-corfu-mode 1))

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
  :hook
  (completion-at-point-functions . cape-dabbrev)
  (completion-at-point-functions . cape-file))

;; (use-package yasnippet-capf
;;   :after (cape yasnippet)
;;   :hook
;;   (completion-at-point-functions . yasnippet-capf))

(use-package flymake
  :hook
  (prog-mode . flymake-mode))

(use-package dape
  :after projectile
  :commands (dape)

  :custom
  (dape-buffer-window-arrangement 'left)
  (dape-cwd-function #'projectile-project-root)

  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)

  :config
  (dape-breakpoint-global-mode 1))

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

  (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-enable-completion-in-string t)

  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-tabnine nil)
  (acm-candidate-match-function #'orderless-flex)

  :config
  (global-lsp-bridge-mode))

(use-package flymake-bridge
  :after flymake
  :ensure (:host github :repo "eki3z/flymake-bridge" :main nil))

;; (defun my/toggle-lsp-bridge ()
;;   "Toggle lsp-bridge to current buffer."
;;   (interactive)
;;   (if (bound-and-true-p lsp-bridge-mode)
;;       (-my/disable-lsp-bridge)
;;     (-my/enable-lsp-bridge)))

;; (defun -my/enable-lsp-bridge ()
;;   (lsp-bridge-mode 1)
;;   (corfu-mode -1)
;;   (flymake-bridge-setup)
;;   (message "Enabled lsp-bridge, disabled corfu"))

;; (defun -my/disable-lsp-bridge ()
;;   (lsp-bridge-mode -1)
;;   (corfu-mode 1)
;;   (message "Enabled corfu, disabled lsp-bridge"))

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

(defun my/go-to-def ()
  "Show definition at point."
  (interactive)
  (if (my/has-lsp)
      (call-interactively #'lsp-bridge-find-def)
    (call-interactively #'evil-goto-definition)))

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

(defun my/dirvish-open-file ()
  "Open file in main window, or open directory in current window."
  (interactive)
  (if (not (window-parameter nil 'my/dirvish-side))
      (call-interactively #'dired-find-alternate-file)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (call-interactively #'dired-find-alternate-file)
        (progn
          (select-window (get-mru-window nil t t))
          (find-file file))))))

(use-package general
  :config
  (global-unset-key (kbd "C-SPC"))

  (general-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    "b"     '(:ignore t :which-key "buffer")
    "b b"   '("switch"                . switch-to-buffer)
    "b i"   '("ibuffer"               . ibuffer)
    "b k"   '("kill this"             . kill-current-buffer)
    "b l"   '("last"                  . mode-line-other-buffer)
    "b K"   '("kill others"           . my/kill-other-buffers)
    "b n"   '("next"                  . next-buffer)
    "b p"   '("previous"              . previous-buffer)
    "b r"   '("revert"                . revert-buffer)

    "d"     '(:keymap dape-global-map :package dape :which-key "dape")

    "e"     '(:ignore t :which-key "emacs")
    "e c"   '("open config"           . my/open-config)
    "e r"   '("reload init.el"        . my/reload-init)
    "e R"   '("restart"               . restart-emacs)
    "e q"   '("quit"                  . save-buffers-kill-emacs)

    "f"     '(:ignore t :which-key "file")
    "f f"   '("find"                  . find-file)
    "f s"   '("save"                  . save-buffer)
    "f r"   '("recent files"          . recentf)

    "g"     '(:ignore t :which-key "git")
    "g g"   '("magit status"          . magit-status)

    "c"     '(:ignore t :which-key "code")
    "c a"   '("action"                . lsp-bridge-code-action)
    "c d"   '("definition"            . my/go-to-def)
    "c e"   '("errors"                . flymake-show-buffer-diagnostics)
    "c E"   '("project errors"        . flymake-show-project-diagnostics)
    "c f"   '("format"                . my/format-buffer)
    "c r"   '("rename symbol"         . lsp-bridge-rename)
    "c s"   '("symbol list"           . consult-imenu)

    "o"     '(:ignore t :which-key "open")
    "o d"   '("dashboard"             . dashboard-open)
    "o o"   '("dired"                 . dired)
    "o p"   '("project view"          . dirvish-side)
    "o s"   '("scratch"               . scratch-buffer)

    "p"     '(:ignore t :which-key "project")
    "p a"   '("switch project"        . projectile-add-known-project)
    "p f"   '("project file"          . projectile-find-file)
    "p p"   '("switch project"        . projectile-switch-project)

    "w"     '(:ignore t :which-key "window")
    "w h"   '("left"                  . evil-window-left)
    "w j"   '("down"                  . evil-window-down)
    "w k"   '("up"                    . evil-window-up)
    "w l"   '("right"                 . evil-window-right)
    "w m"   '("maximize"              . delete-other-windows)
    "w q"   '("kill window"           . delete-window)

    "w s"   '(:ignore t :which-key "window split")
    "w s h" '("horizontal"            . split-window-right)
    "w s v" '("vertical"              . split-window-below)

    "t"     '(:ignore t :which-key "toggle")
    "t d"   '("discord rich presence" . elcord-mode)
    "t n"   '("line numbers"          . display-line-numbers-mode)
    ;; "t l"   '("lsp"                   . my/toggle-lsp-bridge)
    "t w"   '("whitespace"            . whitespace-mode)
    )

  (general-def
    "M-<f4>" #'my/quit-emacs)

  (general-def
    :states 'normal
    :keymaps 'override
    "K"   #'my/show-documentation
    "g d" #'my/go-to-def
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
    "l"   #'my/dirvish-open-file
    "RET" #'my/dirvish-open-file
    )
  )

(run-with-idle-timer
 0.2 nil
 (lambda ()
   (run-hooks 'my/late-hook)))

;;; init.el ends here
