;;; init.el --- -*- lexical-binding: t; -*-

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(defun +load (file)
  "Load a FILE."
  (load (expand-file-name file user-emacs-directory) t))

(+load "elpaca-init")


;; Optimizations

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


;; Custom Hooks

(defcustom +late-hook nil
  "Hook that runs after startup."
  :type 'hook)

(defmacro late! (&rest body)
  "Delay running BODY until after startup."
  (declare (indent defun))
  `(add-hook '+late-hook #'(lambda () ,@body)))

(defmacro after! (package &rest body)
  "Delay running BODY until PACKAGE loaded."
  (declare (indent defun))
  `(with-eval-after-load ',package ,@body))


;; Key Bindings Setup

(use-package which-key
  :ensure nil
  :custom
  (which-key-idle-delay 0.3)
  (which-key-sort-order 'which-key-key-order-alpha)
  (which-key-sort-uppercase-first nil)
  (which-key-dont-use-unicode nil)
  (which-key-min-display-lines 5)
  :hook
  (+late . which-key-mode))

(use-package general
  :config
  (global-unset-key (kbd "C-SPC"))
  (general-create-definer +spc
    :states '(normal visual motion emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer +normal
    :states 'normal
    :keymaps 'override)

  (general-create-definer +visual
    :states 'visual
    :keymaps 'override))

(defmacro map-spc! (&rest rules)
  "Map RULES for SPC keybinds."
  (declare (indent defun))
  `(late! (+spc ,@rules)))

(defmacro map-normal! (&rest rules)
  "Map RULES for NORMAL keybinds."
  (declare (indent defun))
  `(late! (+normal ,@rules)))

(defmacro map-visual! (&rest rules)
  "Map RULES for VISUAL keybinds."
  (declare (indent defun))
  `(late! (+visual ,@rules)))

(defmacro map! (&rest rules)
  "Map RULES for keychord keybinds."
  (declare (indent defun))
  `(late! (general-def ,@rules)))

(map-spc!
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "code")
  "e" '(:ignore t :which-key "emacs")
  "f" '(:ignore t :which-key "file")
  "g" '(:ignore t :which-key "git")
  "o" '(:ignore t :which-key "open")
  "p" '(:ignore t :which-key "project")
  "s" '(:ignore t :which-key "search")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window")
  )


;; Behaviour

(use-package emacs
  :ensure nil
  :custom
  (use-dialog-box nil)
  (confirm-kill-emacs #'yes-or-no-p)

  (custom-file (expand-file-name "custom.el" user-emacs-directory))

  ;; disable annoying bell sound
  (ring-bell-function 'ignore)

  ;; less jumpy scroll
  (scroll-step 1)                     ; scroll one line at a time
  (scroll-margin 2)                   ; start scrolling when 2 lines from bottom
  (scroll-conservatively 100000)      ; never recenter automatically
  (scroll-preserve-screen-position t) ; keep point position when scrolling
  (auto-window-vscroll nil)

  ;; disable backup `.#' and lockfiles `~'
  (create-lockfiles nil)
  (make-backup-files nil)

  :config
  (fset #'yes-or-no-p #'y-or-n-p)
  (setq-default indent-tabs-mode nil))

(defun +open-config ()
  "Open Emacs configuration."
  (interactive)
  (find-file (read-file-name "Find file in config: " user-emacs-directory)))

(defun +reload-init ()
  "Reload Emacs init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory))
  (message "init.el reloaded!"))

(map-spc!
  "e c" '("open config"    . +open-config)
  "e r" '("reload init.el" . +reload-init)
  "e r" '("restart"        . restart-emacs)
  "e q" '("quit"           . save-buffers-kill-emacs))

(map! "M-<f4>" #'save-buffers-kill-emacs)


;; UI Themes

(set-face-attribute 'default nil
                    :family "JetBrainsMono NF"
                    :height 105)
(set-face-attribute 'fixed-pitch nil :family "JetBrainsMono NF")

(use-package display-line-numbers
  :ensure nil
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t)
  (display-line-numbers-type 'relative)
  :hook
  (prog-mode . display-line-numbers-mode)
  (conf-mode . display-line-numbers-mode)
  (text-mode . display-line-numbers-mode))

(late! (global-hl-line-mode))

(use-package nerd-icons :defer t)

(use-package doom-themes
  :config
  (load-theme 'doom-one t))

(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-buffer-encoding 'nondefault)
  :hook
  (+late . doom-modeline-mode))

(use-package hide-mode-line
  :commands (hide-mode-line-mode))

(use-package solaire-mode
  :hook
  (+late . solaire-global-mode))

(use-package vi-tilde-fringe
  :hook
  (+late . global-vi-tilde-fringe-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package symbol-overlay
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package page-break-lines
  :hook
  (+late . global-page-break-lines-mode))

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  (hl-todo-keyword-faces '(("TODO" warning bold)
                           ("FIXME" error bold)))
  :config
  (add-hook 'flymake-diagnostic-functions #'hl-todo-flymake)
  :hook
  (+late . global-hl-todo-mode))

(map-spc!
  "t m"   '("menu bar"     . menu-bar-mode)
  "t n"   '("line numbers" . display-line-numbers-mode)
  "t w"   '("wrap"         . visual-line-mode)
  "t SPC" '("whitespace"   . whitespace-mode))


;; Frame

;; Custom functions/hooks for persisting/loading frame geometry upon save/load
;; https://www.reddit.com/r/emacs/comments/4ermj9/comment/d237n0i
(defun +save-frameg ()
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

(defun +load-frameg ()
  "Loads `frameg.el' which should load the previous frame's geometry."
  (let ((frameg-file (expand-file-name "frameg.el" user-emacs-directory)))
    (when (file-readable-p frameg-file)
      (load-file frameg-file))))

;; Special work to do ONLY when there is a window system being used
(when window-system
  (add-hook 'after-init-hook '+load-frameg)
  (add-hook 'kill-emacs-hook '+save-frameg))


;; Window

(use-package frame
  :ensure nil
  :custom
  (window-divider-default-bottom-width 5)
  (window-divider-default-right-width 5)
  :hook
  (+late . window-divider-mode))

(use-package ace-window
  :commands (ace-window)
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(defun +last-focused-window ()
  "Go to the last focused window."
  (interactive)
  (let ((win (get-mru-window t t t)))
    (unless win (error "Last window not found"))
    (let ((frame (window-frame win)))
      (select-frame-set-input-focus frame)
      (select-window win))))

(defun +split-window-left ()
  "Split window to the left, focus the left window."
  (interactive)
  (call-interactively #'evil-window-vsplit))

(defun +split-window-right ()
  "Split window to the right, focus the right window."
  (interactive)
  (call-interactively #'evil-window-vsplit)
  (call-interactively #'evil-window-right))

(defun +split-window-up ()
  "Split window to the up, focus the up window."
  (interactive)
  (call-interactively #'evil-window-split))

(defun +split-window-down ()
  "Split window to the down, focus the down window."
  (interactive)
  (call-interactively #'evil-window-split)
  (call-interactively #'evil-window-down))

(defun +window-increase-width ()
  "Increase window width."
  (interactive)
  (if (+is-dirvish-side)
      (call-interactively #'dirvish-side-increase-width)
    (call-interactively #'evil-window-increase-width)))

(defun +window-decrease-width ()
  "Decrease window width."
  (interactive)
  (if (+is-dirvish-side)
      (call-interactively #'dirvish-side-decrease-width)
    (call-interactively #'evil-window-decrease-width)))

(map-spc!
  "w h" '("left"            . evil-window-left)
  "w H" '("split left"      . +split-window-left)
  "w j" '("down"            . evil-window-down)
  "w J" '("split down"      . +split-window-down)
  "w k" '("up"              . evil-window-up)
  "w K" '("split up"        . +split-window-up)
  "w l" '("right"           . evil-window-right)
  "w L" '("split right"     . +split-window-right)
  "w m" '("maximize"        . delete-other-windows)
  "w q" '("kill window"     . delete-window)
  "w w" '("switch window"   . ace-window)
  "w =" '("increase width"  . +window-increase-width)
  "w -" '("decrease width"  . +window-decrease-width)
  "w +" '("increase height" . +window-increase-height)
  "w _" '("decrease height" . +window-decrease-height))


;; Child Frames

(use-package transient
  :custom
  (transient-mode-line-format nil))

(defvar +posframe-y-offset 100)
(defvar +posframe-border-color "#bbc2cf")
(defvar +posframe-params '((left-fringe 10)
                           (right-fringe 10)))

(when (display-graphic-p)
  (defun +posframe-poshandler (info)
    (let ((top-center (posframe-poshandler-frame-top-center info)))
      (cons (car top-center) +posframe-y-offset)))

  (use-package which-key-posframe
    :custom
    (which-key-posframe-parameters `(,@+posframe-params
                                     (z-group . above)))
    :config
    (defun +which-key-posframe-poshandler (info)
      (let ((og-pos (+posframe-poshandler info))
            (vertico-posframe (posframe--find-existing-posframe vertico-posframe--buffer)))
        (if (frame-visible-p vertico-posframe)
            (cons (car og-pos) (+ +posframe-y-offset (frame-pixel-height vertico-posframe) -1))
          og-pos)))
    (setq which-key-posframe-poshandler #'+which-key-posframe-poshandler)

    (set-face-background 'which-key-posframe-border +posframe-border-color)
    :hook
    (+late . which-key-posframe-mode))

  (use-package transient-posframe
    :custom
    (transient-posframe-poshandler #'+posframe-poshandler)
    (transient-posframe-parameters +posframe-params)
    :config
    (set-face-background 'transient-posframe-border +posframe-border-color)
    :hook
    (+late . transient-posframe-mode))

  (use-package vertico-posframe
    :custom
    (vertico-posframe-border-width 1)
    (vertico-posframe-poshandler #'+posframe-poshandler)
    (vertico-posframe-parameters +posframe-params)
    (vertico-multiform-commands '((t posframe)))
    :config
    (define-advice vertico-posframe--get-border-color (:override () all-same-color)
      +posframe-border-color))

  (use-package mini-frame
    :custom
    (mini-frame-detach-on-hide nil)
    (mini-frame-show-parameters `((left . 0.5)
                                  (top . ,+posframe-y-offset)
                                  (width . 0.6)
                                  (no-accept-focus . t)
                                  (child-frame-border-width . 1)
                                  (background-color . "#21242b")))
    :config
    (add-to-list 'mini-frame-advice-functions 'map-y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'completing-read)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (set-face-background 'child-frame-border +posframe-border-color)
    :hook
    (+late . mini-frame-mode)))


;; Side Popups

(defun +pop (pred &optional side width)
  "Add `display-buffer-alist' side window rule for PRED with SIDE and WIDTH."
  (add-to-list 'display-buffer-alist `(,pred
                                       (display-buffer-reuse-window
                                        display-buffer-in-side-window)
                                       (post-command-select-window . t)
                                       (side . ,(or side 'right))
                                       (window-width . ,(or width 0.25)))))

(+pop '(major-mode . help-mode))
(+pop '(major-mode . Info-mode))
(+pop '(major-mode . apropos-mode))
(+pop '(major-mode . grep-mode))
(+pop '(major-mode . Custom-mode))
(+pop '(major-mode . occur-mode))
(+pop '(major-mode . xref--xref-buffer-mode))
(+pop '(major-mode . compilation-mode))
(+pop '(this-command . help))
(+pop '(this-command . customize))
(+pop '(this-command . man))


;; Buffer

(defun +kill-other-buffers ()
  "Kill all other buffers except the current one and essential buffers."
  (interactive)
  (let ((target-name '("*dashboard*"
                       "*Help*"
                       "*Ibuffer*"
                       "*lsp-bridge-doc*"
                       "*compilation*"))
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

(map-spc!
  "b b" '("switch"      . consult-buffer)
  "b i" '("ibuffer"     . ibuffer)
  "b k" '("kill this"   . kill-current-buffer)
  "b l" '("last"        . mode-line-other-buffer)
  "b K" '("kill others" . +kill-other-buffers)
  "b n" '("next"        . next-buffer)
  "b p" '("previous"    . previous-buffer)
  "b r" '("revert"      . revert-buffer)
  "o s" '("scratch"     . scratch-buffer))


;; Dashboard

(use-package dashboard
  :custom
  (inhibit-startup-screen t)
  (dashboard-center-content t)
  (dashboard-vertically-center-content t)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items '((recents  . 10)
                     (projects . 10)))

  :init
  (defun +maybe-open-dashboard ()
    (when (length< command-line-args 2)
      (dashboard-open)
      (when (get-buffer "*scratch*")
        (kill-buffer "*scratch*"))))
  (late! (+maybe-open-dashboard))

  :config
  (defun +dashboard-refresh ()
    (with-current-buffer "*dashboard*"
      (let ((inhibit-read-only t))
        (dashboard-insert-startupify-lists t))))

  (defvar +dashboard-resize-timer nil
    "Idle timer for debounced dashboard refresh.")

  (defun +schedule-dashboard-refresh (&optional _)
    "Schedule dashboard refresh after resize."
    (when +dashboard-resize-timer
      (cancel-timer +dashboard-resize-timer))
    (setq +dashboard-resize-timer
          (run-with-idle-timer
           0.5 nil
           (lambda ()
             (when (get-buffer "*dashboard*")
               (+dashboard-refresh))))))

  (defun +dont-kill-dashboard-buffer ()
    (add-hook 'kill-buffer-query-functions
              (lambda ()
                (if (get-buffer-window (current-buffer) 'visible)
                    (progn (message "Cannot kill *dasboard* when it is open.")
                           nil)
                  t))
              nil t))

  :hook
  (window-size-change-functions . +schedule-dashboard-refresh)
  (dashboard-mode . +dont-kill-dashboard-buffer))

(map-spc! "o d" '("dashboard" . dashboard-open))


;; Directory

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
  (add-to-list 'dirvish-side-window-parameters '(+dirvish-side . t))

  :hook
  (dired-mode . dired-omit-mode))

(use-package diredfl
  :after dirvish
  :hook
  (dired-mode . diredfl-mode)
  (dirvish-directory-view-mode . diredfl-mode))

(defun +is-dirvish-side (&optional window)
  "Returns t if WINDOW is `dirvish-side'."
  (window-parameter window '+dirvish-side))

(defun +dirvish-quit ()
  "Dirvish q key."
  (interactive)
  (if (+is-dirvish-side)
      (+last-focused-window)
    (quit-window)))

(defun +dirvish-open-file ()
  "Open file in main window, or open directory in current window."
  (interactive)
  (if (not (+is-dirvish-side))
      (call-interactively #'dired-find-alternate-file)
    (let ((file (dired-get-file-for-visit)))
      (if (file-directory-p file)
          (call-interactively #'dired-find-alternate-file)
        (progn
          (select-window (get-mru-window nil t t))
          (find-file file))))))

(map-spc!
  "f f" '("find"         . find-file)
  "f s" '("save"         . save-buffer)
  "f r" '("recent files" . consult-recent-file)

  "o o" '("dired"        . dired)
  "o p" '("project view" . dirvish-side)

  "p f" '("find file"    . project-find-file)
  "p p" '("switch"       . project-switch-project))

(map!
  :keymaps 'dirvish-mode-map
  :states '(normal motion)
  "q"   #'+dirvish-quit
  "TAB" #'dirvish-subtree-toggle
  "H"   #'dirvish-subtree-up
  "h"   #'dired-up-directory
  "l"   #'+dirvish-open-file
  "RET" #'+dirvish-open-file)


;; Git

(use-package ediff
  :ensure nil
  :custom
  (ediff-window-setup-function #'ediff-setup-windows-plain)
  (ediff-split-window-function #'split-window-horizontally))

(use-package magit
  :commands (magit-status))

(use-package diff-hl
  :config
  ;; https://github.com/dgutov/diff-hl/issues/116#issuecomment-1573253134
  (let* ((width 2)
         (bitmap (vector (1- (expt 2 width)))))
    (define-fringe-bitmap '+diff-hl-bitmap bitmap 1 width '(top t)))
  (setq diff-hl-fringe-bmp-function (lambda (type pos) '+diff-hl-bitmap))

  (set-face-background 'diff-hl-insert nil)
  (set-face-background 'diff-hl-change nil)
  (set-face-background 'diff-hl-delete nil)
  :hook
  (+late . global-diff-hl-mode))

(use-package git-modes
  :mode
  (("\\.gitignore\\'"     . gitignore-mode)
   ("\\.gitconfig\\'"     . gitconfig-mode)
   ("\\.gitmodules\\'"    . gitconfig-mode)
   ("\\.gitattributes\\'" . gitattributes-mode)))

(map-spc!
  "g b" '("blame" . magit-blame)
  "g d" '("diff"  . magit-diff)
  "g g" '("git"   . magit-status)
  "g h" '("hunk"  . diff-hl-show-hunk))



(use-package helpful
  :commands (helpful-at-point)
  :config
  (+pop '(major-mode . helpful-mode)))


;; Terminal

(use-package vterm
  :commands (vterm vterm-other-window)
  :config
  (+pop '(this-command . vterm-other-window)))

(map-spc!
  "o t" '("terminal"      . vterm-other-window)
  "o T" '("terminal here" . vterm))


;; Discord Rich Presence

(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t)
  :hook
  (+late . elcord-mode))

(map-spc! "t d" '("discord rich presence" . elcord-mode))


;; Undo/History

(use-package savehist
  :ensure nil
  :custom
  (history-length 1000)
  (savehist-autosave-interval 60)
  (savehist-additional-variables '(search-ring
                                   regexp-search-ring
                                   extended-command-history))
  :hook
  (+late . save-place-mode)
  (+late . savehist-mode))

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
  (+late . global-undo-fu-session-mode))


;; Evil

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

(use-package evil-easymotion
  :after evil
  :config
  (evilem-default-keybindings "g s"))

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-nerd-commenter
  :after evil
  :init
  (evilnc-default-hotkeys))

(map-visual!
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)


;; Languages

(defvar +nobreadcrumb nil)

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package fish-mode :mode "\\.fish\\'")

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :config
  (add-to-list '+nobreadcrumb 'gfm-mode))

(use-package sh-script
  :ensure nil
  :config
  (add-to-list '+nobreadcrumb 'bash-ts-mode))

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


;; Editing Shortcuts

(use-package combobulate
  :ensure (:host github :repo "mickeynp/combobulate")
  :hook
  (prog-mode . combobulate-mode))

(use-package embark
  :config
  (+pop '(major-mode . embark-collect-mode))

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

(map-spc! "a" '("act" . embark-act))
(map! "M-e" #'embark-act)


;; Completions

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  :hook
  (+late . context-menu-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  :hook
  (prog-mode text-mode markdown-mode))

(use-package xref
  :ensure nil
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
    (call-interactively #'consult-ripgrep)))

(defun +consult-ripgrep-with-dir ()
  "Run `consult-ripgrep` with universal argument to prompt for directory."
  (interactive)
  (let ((current-prefix-arg '(4))) ; simulate C-u
    (call-interactively #'consult-ripgrep)))

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

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :hook
  (+late . global-corfu-mode))

(use-package kind-icon
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(defun +kind-icon-alias (alias from)
  "Add ALIAS mapping FROM `kind-icon-mapping'."
  (add-to-list 'kind-icon-mapping (cons alias (cdr (assoc from kind-icon-mapping)))))

(use-package imenu-list
  :custom
  (imenu-list-auto-update nil))

(use-package wgrep)

(map-spc!
  "s b" '("buffer"       . consult-buffer)
  "s e" '("error"        . consult-flymake)
  "s f" '("fd project"   . consult-fd)
  "s F" '("fd directory" . +consult-fd-with-dir)
  "s i" '("imenu"        . consult-imenu)
  "s l" '("line"         . consult-line)
  "s L" '("line multi"   . consult-line-multi)
  "s r" '("rg project"   . consult-ripgrep)
  "s R" '("rg directory" . +consult-ripgrep-with-dir))

(map!
  :keymaps 'minibuffer-local-map
  "M-a" #'marginalia-cycle)


;; Debugging

(use-package flymake
  :ensure nil
  :config
  (set-face-italic 'flymake-warning-echo nil)

  (defvar +flymake-buffers nil
    "Saved flymake buffers.")

  (define-advice flymake-show-buffer-diagnostics (:before (&rest _) quit-other)
    "Quit other flymake windows before opeing new one."
    (dolist (name +flymake-buffers)
      (when-let* ((buf (get-buffer name))
                  (win (get-buffer-window buf)))
        (quit-window t win)))
    (setq +flymake-buffers nil))

  (define-advice flymake-show-buffer-diagnostics (:after (&rest _) save-buffer)
    "Save flymake buffers."
    (add-to-list '+flymake-buffers (flymake--diagnostics-buffer-name)))

  (+pop '(major-mode flymake-diagnostics-buffer-mode) 'bottom)
  :hook
  (prog-mode . flymake-mode))

(use-package flymake-popon
  :ensure (:repo "https://codeberg.org/akib/emacs-flymake-popon.git")
  :custom
  (flymake-popon-width 100)
  :config
  (set-face-foreground 'flymake-popon-posframe-border "#323232")
  (setq flymake-popon-posframe-extra-arguments (plist-put flymake-popon-posframe-extra-arguments :background-color "#202329"))

  (define-advice flymake-goto-next-error (:around (orig-fn &rest args) no-message)
    "Disable message when jumping to errors."
    (define-advice message (:override (&rest _) noop))
    (apply orig-fn args)
    (advice-remove 'message #'message@noop))
  :hook
  (flymake-mode . flymake-popon-mode))

(use-package dape
  :commands (dape)
  :custom
  (dape-buffer-window-arrangement 'left)
  :hook
  (kill-emacs . dape-breakpoint-save)
  (after-init . dape-breakpoint-load)
  :config
  (dape-breakpoint-global-mode 1))

(use-package ansi-color
  :ensure nil
  :hook
  (compilation-filter . ansi-color-compilation-filter))

(map-spc!
  "c c" '("compile" . compile)
  "d"   '(:keymap dape-global-map :package dape :which-key "dape")
  "o e" '("error"   . flymake-show-buffer-diagnostics)
  "p c" '("compile" . project-compile))

(map-normal!
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)


;; LSP

(use-package lsp-bridge
  :after (yasnippet markdown-mode orderless kind-icon)
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

  (lsp-bridge-imenu-function 'consult-imenu)

  (acm-enable-capf t)
  (acm-enable-icon t)
  (acm-enable-tabnine nil)
  (acm-candidate-match-function #'orderless-flex)

  :config
  ;; Use the same icons as kind-icon
  (+kind-icon-alias 'feature 'keyword)
  (+kind-icon-alias (intern "special form") 'keyword)
  (+kind-icon-alias 'custom 'variable)
  (+kind-icon-alias 'search 'text)
  (+kind-icon-alias 'face 'color)
  (setq acm-icon-dir (expand-file-name ".cache/svg-lib" user-emacs-directory))
  (setq acm-icon-alist (mapcar (lambda (l)
                                 (let* ((symbol (car l))
                                        (key (if (eq symbol t) t (symbol-name symbol)))
                                        (icon (plist-get l :icon))
                                        (face (plist-get l :face))
                                        (color (face-foreground face nil t)))
                                   (cons key (list "material" icon color))))
                               kind-icon-mapping))

  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)
  (+pop "*lsp-bridge-doc*")
  (+pop '(major-mode . lsp-bridge-ref-mode) 'bottom)

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
      (let ((face (doom-modeline-face (get-text-property 0 'face ret))))
        (propertize (nerd-icons-mdicon "nf-md-rocket") 'face face))))

  (define-advice lsp-bridge--enable (:after () disable-corfu)
    "Disable corfu-mode when lsp-bridge is enabled."
    (corfu-mode -1)
    (unless (member major-mode +nobreadcrumb) (lsp-bridge-breadcrumb-mode 1))
    ;; (when (+has-lsp)
    ;;   (lsp-bridge-semantic-tokens-mode 1))
    )

  (define-advice lsp-bridge--disable (:after () enable-corfu)
    "Re-enable corfu-mode when lsp-bridge is disabled."
    (corfu-mode 1)
    (lsp-bridge-breadcrumb-mode -1)
    (lsp-bridge-kill-process)
    ;; (when lsp-bridge-semantic-tokens-mode
    ;;   (lsp-bridge-semantic-tokens-mode -1))
    )

  (defvar +lsp-bridge-doc-mode-map (make-sparse-keymap))
  (define-minor-mode +lsp-bridge-doc-mode
    "Minor mode for *lsp-bridge-doc* buffer.")

  (defun +setup-lsp-bridge-doc-buffer ()
    "Setup *lsp-bridge-doc* buffer."
    (when-let ((buf (get-buffer "*lsp-bridge-doc*")))
      (with-current-buffer buf
        (unless +lsp-bridge-doc-mode
          (+lsp-bridge-doc-mode 1))
        (display-line-numbers-mode -1)
        (general-define-key
         :keymaps 'local
         :states '(normal motion)
         "q" #'quit-window))))
  (add-hook 'buffer-list-update-hook #'+setup-lsp-bridge-doc-buffer)

  :hook
  (prog-mode . lsp-bridge-mode)
  (conf-mode . lsp-bridge-mode)
  (text-mode . lsp-bridge-mode)
  (lsp-bridge-mode . flymake-bridge-setup))

(use-package flymake-bridge
  :after flymake
  :ensure (:host github :repo "eki3z/flymake-bridge" :main nil))

(defun +has-lsp ()
  "Return whether the current buffer has LSP server."
  (and (bound-and-true-p lsp-bridge-mode)
       (lsp-bridge-has-lsp-server-p)))

(defun +show-documentation ()
  "Show documentation at point."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-show-documentation)
    (call-interactively #'helpful-at-point)))

(defun +go-to-def ()
  "Show definition at point."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-find-def)
    (evil-set-jump)
    (call-interactively #'evil-goto-definition)))

(defun +go-to-ref ()
  "Show references at point."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-xref-find-references)
    (call-interactively #'xref-find-references)))

(defun +show-symbols ()
  "Show symbols in buffer."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-imenu)
    (call-interactively #'consult-imenu)))

(map-spc!
  "c a" '("action"        . lsp-bridge-code-action)
  "c r" '("rename symbol" . lsp-bridge-rename)
  "c s" '("symbol list"   . consult-imenu)
  "t l" '("lsp"           . lsp-bridge-mode))

(map-normal!
  "K"   #'+show-documentation
  "g d" #'+go-to-def
  "g r" #'+go-to-ref)


;; Formatting

(setq-default truncate-lines t
              tab-width 4)

(setq standard-indent 4
      whitespace-style '(face tabs tab-mark
                              spaces space-mark
                              indentation
                              space-after-tab space-before-tab
                              trailing
                              missing-newline-at-eof))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package editorconfig
  :ensure nil
  :hook
  (+late . editorconfig-mode))

(use-package apheleia :defer t)

(defun +format-buffer ()
  "Format buffer using apheleia or lsp-bridge."
  (interactive)
  (if (+has-lsp)
      (progn
        (call-interactively #'lsp-bridge-code-format)
        (message "Formatted using lsp-bridge"))
    (progn
      (call-interactively #'apheleia-format-buffer)
      (message "Formatted using apheleia"))))

(map-spc! "c f" '("format" . +format-buffer))



(+load "init-private")

(run-with-idle-timer
 0.2 nil
 (lambda ()
   (run-hooks '+late-hook)))

;;; init.el ends here
