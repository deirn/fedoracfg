;;; init.el --- -*- lexical-binding: t; -*-

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(defun my/load (file)
  "Load a FILE."
  (load (expand-file-name file user-emacs-directory) t))

(my/load "elpaca-init")

(use-package benchmark-init
  :config
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

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
  :after lsp-bridge
  :mode "\\.dart\\'"
  :config
  (add-to-list 'treesit-language-source-alist '(dart . ("https://github.com/UserNobody14/tree-sitter-dart")))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(dart-ts-mode . "dart-analysis-server")))

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :after lsp-bridge
  :config
  (dolist (e svelte-ts-mode-language-source-alist)
    (add-to-list 'treesit-language-source-alist e))
  (add-to-list 'lsp-bridge-single-lang-server-mode-list '(svelte-ts-mode . "svelteserver")))

(use-package nix-ts-mode :mode "\\.nix\\'")

(use-package beancount
  :ensure (:host github :repo "beancount/beancount-mode")
  :mode ("\\.beancount\\'" . beancount-mode))

(use-package nerd-icons :defer t)
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

  (tab-always-indent 'complete)
  (truncate-lines t)
  (tab-width 4)
  (standard-indent 4)
  (whitespace-style '(face tabs tab-mark
                           spaces space-mark
                           indentation
                           space-after-tab space-before-tab
                           trailing
                           missing-newline-at-eof))

  ;; disable backup `.#' and lockfiles `~'
  (create-lockfiles nil)
  (make-backup-files nil)

  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-dont-use-unicode nil)
  (which-key-min-display-lines 5)

  (history-length 1000)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring
                                   regexp-search-ring
                                   extended-command-history))

  (read-extended-command-predicate #'command-completion-default-include-p)

  (which-func-display nil)
  :config
  (set-face-attribute 'default nil
                      :family "JetBrainsMono NF"
                      :height 105)
  (set-face-attribute 'fixed-pitch nil :family "JetBrainsMono NF")

  (fset #'yes-or-no-p #'y-or-n-p)
  (setq-default indent-tabs-mode nil)

  ;; Custom functions/hooks for persisting/loading frame geometry upon save/load
  ;; https://www.reddit.com/r/emacs/comments/4ermj9/comment/d237n0i
  (defun my/save-frameg ()
    "Gets the current frame's geometry and saves to `frameg.el'."
    (let ((frameg-left (frame-parameter (selected-frame) 'left))
          (frameg-top (frame-parameter (selected-frame) 'top))
          (frameg-width (frame-parameter (selected-frame) 'width))
          (frameg-height (frame-parameter (selected-frame) 'height))
          (frameg-fullscreen (frame-parameter (selected-frame) 'fullscreen))
          (frameg-file (expand-file-name "frameg.el" user-emacs-directory)))
      (with-temp-buffer
        ;; Turn off backup for this file
        (make-local-variable 'make-backup-files)
        (setq make-backup-files nil)
        (insert
         ";;; This file stores the previous emacs frame's geometry. -*- lexical-binding: t; -*-\n"
         ";;; Last generated " (current-time-string) ".\n"
         "(setq initial-frame-alist\n"
         " '("
         (format " (top . %d)\n" (max frameg-top 0))
         (format " (left . %d)\n" (max frameg-left 0))
         (format " (width . %d)\n" (max frameg-width 0))
         (format " (height . %d)\n" (max frameg-height 0))
         (format " (fullscreen . %s)))\n" frameg-fullscreen))
        (when (file-writable-p frameg-file)
          (write-file frameg-file)))))

  (defun my/load-frameg ()
    "Loads `frameg.el' which should load the previous frame's geometry."
    (let ((frameg-file (expand-file-name "frameg.el" user-emacs-directory)))
      (when (file-readable-p frameg-file)
        (load-file frameg-file))))

  ;; Special work to do ONLY when there is a window system being used
  (when window-system
    (add-hook 'after-init-hook 'my/load-frameg)
    (add-hook 'kill-emacs-hook 'my/save-frameg))

  :hook
  (prog-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode)
  (prog-mode . which-function-mode)
  (before-save . delete-trailing-whitespace)
  (my/late . save-place-mode)
  (my/late . savehist-mode)
  (my/late . global-hl-line-mode)
  (my/late . electric-pair-mode)
  (my/late . which-key-mode)
  (my/late . window-divider-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-encoding 'nondefault)
  :config
  (doom-modeline-def-segment my/breadcrumb
    "Display breadcrumb."
    (when which-func-mode
      (let ((func (eval (plist-get which-func-current :eval))))
        (when (and func (not (string= func "n/a")))
          (concat (doom-modeline-spc)
                  (propertize func 'face 'which-func))))))

  (doom-modeline-def-segment my/lsp
    (when lsp-bridge-mode
      (let ((sep (doom-modeline-spc)))
        (concat sep
                (lsp-bridge--mode-line-format)
                sep))))

  (doom-modeline-segment--major-mode)
  (doom-modeline-def-modeline 'my/modeline
    '(eldoc bar window-state workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(my/breadcrumb compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding my/lsp major-mode process vcs check time))

  (add-hook 'doom-modeline-mode-hook (lambda () (doom-modeline-set-modeline 'my/modeline 'default)))
  :hook
  (my/late . doom-modeline-mode))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package solaire-mode
  :hook
  (my/late . solaire-global-mode))

(use-package vi-tilde-fringe
  :hook
  (my/late . global-vi-tilde-fringe-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package transient
  :custom
  (transient-mode-line-format nil))

(setq my/posframe-y-offset 100
      my/posframe-border-color "#bbc2cf"
      my/posframe-params '((left-fringe 10)
                           (right-fringe 10)))

(defun my/posframe-poshandler (info)
  "Custom poshandler, INFO."
  (let ((top-center (posframe-poshandler-frame-top-center info)))
    (cons (car top-center) my/posframe-y-offset)))

(use-package which-key-posframe
  :custom
  (which-key-posframe-poshandler #'my/posframe-poshandler)
  (which-key-posframe-parameters `(,@my/posframe-params
                                   (z-group . above)))
  :config
  (set-face-background 'which-key-posframe-border my/posframe-border-color)
  :hook
  (my/late . which-key-posframe-mode))

(use-package transient-posframe
  :custom
  (transient-posframe-poshandler #'my/posframe-poshandler)
  (transient-posframe-parameters my/posframe-params)
  :config
  (set-face-background 'transient-posframe-border my/posframe-border-color)
  :hook
  (my/late . transient-posframe-mode))

(use-package vertico-posframe
  :custom
  (vertico-posframe-border-width 1)
  (vertico-posframe-poshandler #'my/posframe-poshandler)
  (vertico-posframe-parameters my/posframe-params)
  (vertico-multiform-commands '((t posframe)))
  :config
  (define-advice vertico-posframe--get-border-color (:override () all-same-color)
    my/posframe-border-color))

(use-package mini-frame
  :custom
  (mini-frame-detach-on-hide nil)
  (mini-frame-show-parameters `((left . 0.5)
                                (top . ,my/posframe-y-offset)
                                (width . 0.6)
                                (no-accept-focus . t)
                                (child-frame-border-width . 1)
                                (background-color . "#21242b")))
  :config
  (add-to-list 'mini-frame-advice-functions 'map-y-or-n-p)
  (add-to-list 'mini-frame-ignore-functions 'completing-read)
  (add-to-list 'mini-frame-ignore-commands 'evil-ex)
  (set-face-background 'child-frame-border my/posframe-border-color)
  :hook
  (my/late . mini-frame-mode))

;; (use-package highlight-indent-guides
;;   :custom
;;   (highlight-indent-guides-method 'character)
;;   (highlight-indent-guides-responsive 'top)

;;   :hook
;;   (prog-mode . highlight-indent-guides-mode))

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
  (dashboard-items '((recents  . 10)
                     (projects . 10)))
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
  (dired-listing-switches "-alh --group-directories-first")
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons vc-state file-size file-time))
  (dirvish-side-attributes '(nerd-icons collapse vc-state file-size))
  (dirvish-hide-details '(dirvish-side))
  (dirvish-mode-line-height 24)

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

(use-package ace-window
  :commands (ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; popup window
(use-package shackle
  :custom
  (shackle-default-size 0.25)
  (shackle-rules '((help-mode :select t :popup t :align right)
                   (helpful-mode :select t :popup t :align right)
                   ("*lsp-bridge-doc*" :select t :popup t :align right)
                   (Man-mode :select t :popup t :align right)
                   (vterm-mode :select t :popup t :align right)
                   (grep-mode :select t :popup t :align right)

                   (flymake-diagnostics-buffer-mode :select t :popup t :align bottom)
                   (lsp-bridge-ref-mode :select t :popup t :align bottom)))
  :hook
  (my/late . shackle-mode))

(use-package magit
  :commands (magit-status))

(use-package git-modes
  :mode
  (("\\.gitignore\\'"     . gitignore-mode)
   ("\\.gitconfig\\'"     . gitconfig-mode)
   ("\\.gitmodules\\'"    . gitconfig-mode)
   ("\\.gitattributes\\'" . gitattributes-mode)))

(use-package helpful
  :commands (helpful-at-point))

(use-package vterm
  :commands (vterm vterm-other-window))

;; discord rich presence
(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t)
  :hook
  (my/late . elcord-mode))

(use-package undo-fu
  :custom
  (undo-limit 256000)
  (undo-strong-limit 2000000)
  (undo-outer-limit 36000000))

(use-package undo-fu-session
  :after undo-fu
  :custom
  (undo-fu-session-compression 'zst)
  :hook
  (my/late . global-undo-fu-session-mode))

(use-package evil
  :after undo-fu
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-respect-visual-line-mode t)

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

(use-package consult)

(use-package embark
  :config
  ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
  (defun embark-which-key-indicator ()
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator))

(use-package embark-consult :after embark)

(use-package vertico
  :after embark
  :config
  (vertico-mode 1)
  (vertico-multiform-mode 1))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode 1))

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

(use-package flymake
  :ensure nil
  :config
  (defvar my/flymake-buffers nil
    "Saved flymake buffers.")

  (define-advice flymake-show-buffer-diagnostics (:before (&rest _) quit-other)
    "Quit other flymake windows before opeing new one."
    (dolist (name my/flymake-buffers)
      (when-let* ((buf (get-buffer name))
                  (win (get-buffer-window buf)))
        (quit-window t win)))
    (setq my/flymake-buffers nil))

  (define-advice flymake-show-buffer-diagnostics (:after (&rest _) save-buffer)
    "Save flymake buffers."
    (add-to-list 'my/flymake-buffers (flymake--diagnostics-buffer-name)))
  :hook
  (prog-mode . flymake-mode))

;; FIXME: https://github.com/konrad1977/flyover/issues/17
;; (use-package flyover
;;   :after (flymake)
;;   :ensure (:host github :repo "konrad1977/flyover")
;;   :custom
;;   (flyover-checkers '(flymake)))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)))
  :config
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake)
  :hook
  (my/late . global-hl-todo-mode))

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

(use-package corfu
  :hook
  (my/late . global-corfu-mode))

(use-package lsp-bridge
  :after (yasnippet markdown-mode orderless)
  :ensure (:host github :repo "manateelazycat/lsp-bridge"
                 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 :build (:not elpaca--byte-compile))
  :custom
  (elpaca-build-steps)
  ;; use uv, so it has consistent package version
  (lsp-bridge-python-command "uv")
  (lsp-bridge-enable-hover-diagnostic t)

  ;; use flymake-bridge, see below
  (lsp-bridge-diagnostic-enable-overlays nil)

  ;; Use minibuffer instead of popup for code action
  (lsp-bridge-code-action-preview-delay nil)
  (lsp-bridge-code-action-enable-popup-menu nil)

  ;; manually enabled below
  (lsp-bridge-enable-mode-line nil)

  ;; (lsp-bridge-enable-completion-in-minibuffer t)
  (lsp-bridge-enable-completion-in-string t)
  (lsp-bridge-symbols-enable-which-func t)

  ;; open reference match on current main window
  (lsp-bridge-ref-open-file-in-request-window t)
  ;; don't delete other main windows
  (lsp-bridge-ref-delete-other-windows nil)
  ;; don't kill match buffers when quiting reference search
  (lsp-bridge-ref-kill-temp-buffer-p nil)

  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-tabnine nil)
  (acm-candidate-match-function #'orderless-flex)

  :config
  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)

  ;; Add to jump list before going to definition, impl, etc
  (advice-add 'lsp-bridge--record-mark-ring :before #'evil-set-jump)

  (define-advice lsp-bridge-ref-open-file (:around (orig-fn &rest args) set-jump)
    "Add to jump list before opening result file."
    (define-advice find-file (:before (&rest _) set-jump-inner) (evil-set-jump))
    (apply orig-fn args)
    (advice-remove 'find-file #'find-file@set-jump-inner))

  (define-advice lsp-bridge--mode-line-format (:filter-return (ret) rocket)
    "Replace `lsp-bridge' mode line string with a rocket icon."
    (when ret
      (propertize (nerd-icons-mdicon "nf-md-rocket") 'face (get-text-property 0 'face ret))))

  (define-advice lsp-bridge--enable (:after () disable-corfu)
    "Disable corfu-mode when lsp-bridge is enabled."
    (corfu-mode -1))

  (define-advice lsp-bridge--disable (:after () enable-corfu)
    "Re-enable corfu-mode when lsp-bridge is disabled."
    (corfu-mode 1))

  (defvar my/lsp-bridge-doc-mode-map (make-sparse-keymap))
  (define-minor-mode my/lsp-bridge-doc-mode
    "Minor mode for *lsp-bridge-doc* buffer.")

  (defun my/setup-lsp-bridge-doc-buffer ()
    "Setup *lsp-bridge-doc* buffer."
    (when-let ((buf (get-buffer "*lsp-bridge-doc*")))
      (with-current-buffer buf
        (unless my/lsp-bridge-doc-mode
          (my/lsp-bridge-doc-mode 1))
        (display-line-numbers-mode -1)
        (general-define-key
         :keymaps 'local
         :states '(normal motion)
         "q" #'quit-window))))
  (add-hook 'buffer-list-update-hook #'my/setup-lsp-bridge-doc-buffer)

  :hook
  (prog-mode . lsp-bridge-mode)
  (conf-mode . lsp-bridge-mode)
  (text-mode . lsp-bridge-mode)
  (lsp-bridge-mode . flymake-bridge-setup))

(use-package flymake-bridge
  :after flymake
  :ensure (:host github :repo "eki3z/flymake-bridge" :main nil))

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
    (evil-set-jump)
    (call-interactively #'evil-goto-definition)))

(defun my/go-to-ref ()
  "Show references at point."
  (interactive)
  (if (my/has-lsp)
      (call-interactively #'lsp-bridge-find-references)
    (call-interactively #'xref-find-references)))

(defun my/open-config ()
  "Open Emacs configuration."
  (interactive)
  (find-file (read-file-name "Find file in config: " user-emacs-directory)))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the current one and essential buffers."
  (interactive)
  (let ((target-name '("*dashboard*"
                       "*Help*"
                       "*Ibuffer*"
                       "*lsp-bridge-doc*"))
        (target-mode '("Helpful"
                       "Custom"
                       "Magit"
                       "Magit Process"
                       "Flymake diagnostics"
                       "Grep"
                       "Backtrace"))
        (killed-count 0))
    (dolist (buf (buffer-list))
      (unless (get-buffer-window buf t)
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

(defun my/is-dirvish-side (&optional window)
  "Returns t if WINDOW is `dirvish-side'."
  (window-parameter window 'my/dirvish-side))

(defun my/dirvish-open-file ()
  "Open file in main window, or open directory in current window."
  (interactive)
  (if (not (my/is-dirvish-side))
      (call-interactively #'dired-find-alternate-file)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (call-interactively #'dired-find-alternate-file)
        (progn
          (select-window (get-mru-window nil t t))
          (find-file file))))))

(defun my/split-window-left ()
  "Split window to the left, focus the left window."
  (interactive)
  (call-interactively #'evil-window-vsplit))

(defun my/split-window-right ()
  "Split window to the right, focus the right window."
  (interactive)
  (call-interactively #'evil-window-vsplit)
  (call-interactively #'evil-window-right))

(defun my/split-window-up ()
  "Split window to the up, focus the up window."
  (interactive)
  (call-interactively #'evil-window-split))

(defun my/split-window-down ()
  "Split window to the down, focus the down window."
  (interactive)
  (call-interactively #'evil-window-split)
  (call-interactively #'evil-window-down))

(defun my/last-focused-window ()
  "Go to the last focused window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun my/window-increase-width ()
  "Increase window width."
  (interactive)
  (if (my/is-dirvish-side)
      (call-interactively #'dirvish-side-increase-width)
    (call-interactively #'evil-window-increase-width)))

(defun my/window-decrease-width ()
  "Decrease window width."
  (interactive)
  (if (my/is-dirvish-side)
      (call-interactively #'dirvish-side-decrease-width)
    (call-interactively #'evil-window-decrease-width)))

(defun my/consult-fd-with-dir ()
  "Run `consult-fd` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-ripgrep)))

(defun my/consult-ripgrep-with-dir ()
  "Run `consult-ripgrep` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-ripgrep)))

(use-package general
  :config
  (global-unset-key (kbd "C-SPC"))

  (general-def
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC"
    "b"     '(:ignore t :which-key "buffer")
    "b b"   '("switch"                . consult-buffer)
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
    "f r"   '("recent files"          . consult-recent-file)

    "g"     '(:ignore t :which-key "git")
    "g g"   '("git"                   . magit-status)
    "g b"   '("blame"                 . magit-blame)

    "c"     '(:ignore t :which-key "code")
    "c a"   '("action"                . lsp-bridge-code-action)
    "c c"   '("compile"               . compile)
    "c d"   '("definition"            . my/go-to-def)
    "c D"   '("definition"            . my/go-to-ref)
    "c f"   '("format"                . my/format-buffer)
    "c r"   '("rename symbol"         . lsp-bridge-rename)
    "c s"   '("symbol list"           . consult-imenu)

    "o"     '(:ignore t :which-key "open")
    "o d"   '("dashboard"             . dashboard-open)
    "o e"   '("error"                 . flymake-show-buffer-diagnostics)
    "o o"   '("dired"                 . dired)
    "o p"   '("project view"          . dirvish-side)
    "o s"   '("scratch"               . scratch-buffer)
    "o t"   '("terminal"              . vterm-other-window)
    "o T"   '("terminal here"         . vterm)

    "p"     '(:ignore t :which-key "project")
    "p a"   '("add"                   . projectile-add-known-project)
    "p c"   '("compile"               . projectile-compile-project)
    "p f"   '("find file"             . projectile-find-file)
    "p p"   '("switch"                . projectile-switch-project)

    "s"     '(:ignore t :which-key "search")
    "s b"   '("buffer"                . consult-buffer)
    "s e"   '("error"                 . consult-flymake)
    "s f"   '("fd project"            . consult-fd)
    "s F"   '("fd directory"          . my/consult-fd-with-dir)
    "s i"   '("imenu"                 . consult-imenu)
    "s l"   '("line"                  . consult-line)
    "s L"   '("line multi"            . consult-line-multi)
    "s r"   '("rg project"            . consult-ripgrep)
    "s R"   '("rg directory"          . my/consult-ripgrep-with-dir)

    "w"     '(:ignore t :which-key "window")
    "w h"   '("left"                  . evil-window-left)
    "w H"   '("split left"            . my/split-window-left)
    "w j"   '("down"                  . evil-window-down)
    "w J"   '("split down"            . my/split-window-down)
    "w k"   '("up"                    . evil-window-up)
    "w K"   '("split up"              . my/split-window-up)
    "w l"   '("right"                 . evil-window-right)
    "w L"   '("split right"           . my/split-window-right)
    "w m"   '("maximize"              . delete-other-windows)
    "w q"   '("kill window"           . delete-window)
    "w w"   '("switch window"         . ace-window)
    "w ="   '("increase width"        . my/window-increase-width)
    "w -"   '("decrease width"        . my/window-decrease-width)
    "w +"   '("increase height"       . my/window-increase-height)
    "w _"   '("decrease height"       . my/window-decrease-height)

    "t"     '(:ignore t :which-key "toggle")
    "t d"   '("discord rich presence" . elcord-mode)
    "t l"   '("lsp"                   . lsp-bridge-mode)
    "t n"   '("line numbers"          . display-line-numbers-mode)
    "t w"   '("wrap"                  . visual-line-mode)
    "t SPC" '("whitespace"            . whitespace-mode)
    )

  (general-def
    "M-<f4>" #'my/quit-emacs
    "M-e"    #'embark-act
    )

  (general-def
    :states 'normal
    :keymaps 'override
    "K"   #'my/show-documentation
    "g d" #'my/go-to-def
    "g r" #'my/go-to-ref
    "] e" #'flymake-goto-next-error
    "[ e" #'flymake-goto-prev-error
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
    "q"   #'my/last-focused-window
    "TAB" #'dirvish-subtree-toggle
    "H"   #'dirvish-subtree-up
    "h"   #'dired-up-directory
    "l"   #'my/dirvish-open-file
    "RET" #'my/dirvish-open-file
    )

  (general-def
    :keymaps 'minibuffer-local-map
    "M-a" #'marginalia-cycle
    )
  )

(my/load "init-private")

(run-with-idle-timer
 0.2 nil
 (lambda ()
   (run-hooks 'my/late-hook)))

;;; init.el ends here
