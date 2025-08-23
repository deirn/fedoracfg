;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;    My Emacs init.
;;; Code:

(when (version< emacs-version "30") (error "This requires Emacs 30 and above!"))

(defun +load (file)
  "Load a FILE relative to `user-emacs-directory'."
  (load (expand-file-name file user-emacs-directory) t))

(+load "elpaca-init")

(defgroup +deirn nil
  "Deirn's custom group."
  :group 'emacs)


;; Optimizations

(use-package benchmark-init
  :config
  (add-hook 'elpaca-after-init-hook 'benchmark-init/deactivate))

(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :init
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize)))


;; Package Utilities

(defun +elpaca-name (&optional interactive)
  "Prompt for package, copy its name to kill ring if INTERACTIVE."
  (interactive (list t))
  (when-let* ((name-symbol (car (elpaca-menu-item)))
              (name (symbol-name name-symbol)))
    (when interactive
      (kill-new name))
    name))

(defun +elpaca-insert-name ()
  "Prompt for package, insert its name at point."
  (interactive)
  (insert (+elpaca-name)))

(defvar +late-hook-ran nil)
(defcustom +late-hook nil
  "Hook that runs after startup."
  :type 'hook)

(defmacro late! (&rest body)
  "Delay running BODY until after startup."
  (declare (indent defun))
  (if (bound-and-true-p +late-hook-ran)
      `(progn ,@body)
    `(add-hook '+late-hook #'(lambda () ,@body))))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (run-with-idle-timer
             0.2 nil
             (lambda ()
               (run-hooks '+late-hook)
               (setq +late-hook-ran t)
               (+load "init-private")
               (+load "custom")))))

(defmacro after! (package &rest body)
  "Delay running BODY until PACKAGE(s) loaded.
Usage:
  (after! pkg (message \"pkg loaded\"))
  (after! (pkg1 pkg2) (message \"pkg1 and pkg2 loaded\"))"
  (declare (indent defun))
  (if (not (listp package))
      `(with-eval-after-load ',package ,@body)
    (let ((pkg (car package)))
      (dolist (next (reverse (cdr package)))
        (setq body `((after! ,next ,@body))))
      `(after! ,pkg ,@body))))


;; Key Bindings Setup

(use-package which-key
  :ensure nil
  :custom
  (which-key-show-prefix 'top)
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
  (general-create-definer +key-spc
    :states '(normal visual motion emacs insert)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "C-SPC")

  (general-create-definer +key-normal
    :states 'normal
    :keymaps 'override)

  (general-create-definer +key-visual
    :states 'visual
    :keymaps 'override))

(defmacro map! (definer &rest rules)
  "Map RULES for DEFINER."
  (declare (indent defun))
  (if (eq definer nil)
      `(late! (general-def ,@rules))
    (let ((definer (intern (concat "+key-" (symbol-name definer)))))
      `(late! (,definer ,@rules)))))

(map! spc
  "b" '(:ignore t :which-key "buffer")
  "c" '(:ignore t :which-key "code")
  "e" '(:ignore t :which-key "emacs")
  "f" '(:ignore t :which-key "file")
  "g" '(:ignore t :which-key "git")
  "o" '(:ignore t :which-key "open")
  "p" '(:ignore t :which-key "project")
  "s" '(:ignore t :which-key "search")
  "t" '(:ignore t :which-key "toggle")
  "w" '(:ignore t :which-key "window"))

(map! spc
  "e k" '("keybinds" . which-key-show-top-level))


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

(map! spc
  "e c" '("open config" . +open-config)
  "e r" '("restart"     . restart-emacs)
  "e q" '("quit"        . save-buffers-kill-emacs))

(map! nil
  "M-<f4>" #'save-buffers-kill-emacs)


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

(use-package nerd-icons :defer t
  :custom
  (nerd-icons-default-adjust 0.1))

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
  :commands (hide-mode-line-mode
             global-hide-mode-line-mode))

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

(use-package perfect-margin
  :commands (perfect-margin-mode)
  :config
  (define-advice perfect-margin--supported-side-window-p (:around (orig-fn win) more-side-window)
    (or (+is-dirvish-side win)
        (funcall orig-fn win)))
  )

(define-minor-mode +zen-mode
  "Toggle Zen Mode."
  :global t
  :init-value nil
  (let ((enable (if +zen-mode 1 -1)))
    (global-hide-mode-line-mode enable)
    (perfect-margin-mode enable)))

(map! spc
  "t m"   '("mode line"        . hide-mode-line-mode)
  "t M-m" '("global mode line" . global-hide-mode-line-mode)
  "t M"   '("menu bar"         . menu-bar-mode)
  "t n"   '("line numbers"     . display-line-numbers-mode)
  "t p"   '("perfect margin"   . perfect-margin-mode)
  "t w"   '("wrap"             . visual-line-mode)
  "t z"   '("zen"              . +zen-mode)
  "t SPC" '("whitespace"       . whitespace-mode))


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

(map! spc
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

(use-package posframe)
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
      (if-let* ((og-pos (+posframe-poshandler info))
                (vertico-buffer (bound-and-true-p vertico-posframe--buffer))
                (vertico-posframe (posframe--find-existing-posframe vertico-buffer))
                (_ (eq (window-buffer (frame-root-window vertico-posframe)) vertico-buffer))
                (_ (frame-visible-p vertico-posframe)))
          (cons (car og-pos) (+ +posframe-y-offset (frame-pixel-height vertico-posframe) -1))
        og-pos))
    (setq which-key-posframe-poshandler #'+which-key-posframe-poshandler)

    (define-advice which-key-posframe--show-buffer (:around (orig-fn dim) fix-dim)
      (setf (car dim)
            (with-current-buffer which-key--buffer
              (count-lines (point-min) (point-max))))
      (setf (cdr dim)
            (max (cdr dim)
                 (with-current-buffer which-key--buffer
                   (save-excursion
                     (goto-char (point-min))
                     (1- (length (thing-at-point 'line t)))))))
      (funcall orig-fn dim))

    (set-face-background 'which-key-posframe-border +posframe-border-color)
    :hook
    (+late . which-key-posframe-mode))

  ;; https://github.com/emacsorphanage/transient-posframe/wiki
  (after! (transient posframe)
    (setq transient-display-buffer-action
          (list
           (lambda (buffer _)
             (posframe-show
              buffer
              :poshandler #'+posframe-poshandler
              :min-width transient-minimal-frame-width
              :lines-truncate t
              :internal-border-color +posframe-border-color
              :internal-border-width 1
              :override-parameters +posframe-params)
             (get-buffer-window transient--buffer t)))))

  (use-package vertico-posframe
    :after vertico
    :custom
    (vertico-posframe-border-width 1)
    (vertico-posframe-poshandler #'+posframe-poshandler)
    (vertico-posframe-parameters +posframe-params)
    (vertico-multiform-commands '((t posframe)))
    :config
    (define-advice vertico-posframe--get-border-color (:override () all-same-color)
      +posframe-border-color)
    :hook
    (+late . vertico-multiform-mode))

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
    (add-to-list 'mini-frame-ignore-commands 'elpaca-ui-search)
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
(+pop '(major-mode . compilation-mode) 'bottom)
(+pop '(this-command . help))
(+pop '(this-command . customize))
(+pop '(this-command . man))


;; Buffer

(use-package nerd-icons-ibuffer
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode))

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

(map! spc
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

(map! spc
  "o d" '("dashboard" . dashboard-open))


;; Directory

(use-package dirvish
  :custom
  (delete-by-moving-to-trash t)
  (dired-listing-switches "-alh --group-directories-first")
  (dirvish-subtree-state-style 'nerd)
  (dirvish-attributes '(nerd-icons vc-state file-size file-time))
  (dirvish-side-attributes '(nerd-icons collapse vc-state file-size))
  (dirvish-hide-details '(dirvish-side))
  (dirvish-header-line-height 24)
  (dirvish-mode-line-height 24)
  (dirvish-collapse-separator "/")
  :config
  (dirvish-side-follow-mode 1)
  (put 'dired-find-alternate-file 'disabled nil)
  (add-to-list 'dirvish-side-window-parameters '(+dirvish-side . t))
  :hook
  (+late . dirvish-override-dired-mode)
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

(map! spc
  "f f" '("find"         . find-file)
  "f s" '("save"         . save-buffer)
  "f r" '("recent files" . consult-recent-file)

  "o o" '("dired"        . dired)
  "o p" '("project view" . dirvish-side)

  "p f" '("find file"    . project-find-file)
  "p p" '("switch"       . project-switch-project))

(map! nil
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

(map! spc
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

(map! spc
  "o t" '("terminal"      . vterm-other-window)
  "o T" '("terminal here" . vterm))


;; Discord Rich Presence

(use-package elcord
  :custom
  (elcord-use-major-mode-as-main-icon t)
  :hook
  (+late . elcord-mode))

(map! spc
  "t d" '("discord rich presence" . elcord-mode))


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
  (evil-want-minibuffer t)
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-fu)
  (evil-respect-visual-line-mode t)
  :config
  (setq evil-insert-state-cursor `(,(face-foreground 'ansi-color-blue)    bar)
        evil-normal-state-cursor `(,(face-foreground 'ansi-color-green)   box)
        evil-emacs-state-cursor  `(,(face-foreground 'ansi-color-magenta) box)
        evil-visual-state-cursor `(,(face-foreground 'ansi-color-yellow)  box))
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

(map! visual
  "A" #'evil-mc-make-cursor-in-visual-selection-end
  "I" #'evil-mc-make-cursor-in-visual-selection-beg)


;; Languages

(defvar +--nobreadcrumb nil)
(defun +nobreadcrumb (mode)
  "Disable breadcrumb for MODE."
  (add-to-list '+--nobreadcrumb mode))

(defun +ts (language url &rest conf)
  "Add new treesit LANGUAGE source from URL and extra CONF."
  (add-to-list 'treesit-language-source-alist (cons language (cons url conf))))

(defun +lsp (mode command)
  "Set lsp-bridge server COMMAND for MODE."
  (after! lsp-bridge
    (add-to-list 'lsp-bridge-single-lang-server-mode-list (cons mode command))))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode 1))

(use-package fish-mode
  :mode "\\.fish\\'"
  :config
  (+nobreadcrumb 'fish-mode))

(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :custom
  (markdown-display-remote-images t)
  (markdown-max-image-size '(200 . nil))
  :config
  (+nobreadcrumb 'gfm-mode)
  :hook
  (gfm-view-mode . markdown-display-inline-images))

(use-package sh-script
  :ensure nil
  :config
  (+nobreadcrumb 'bash-ts-mode))

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
  :mode "\\.dart\\'"
  :config
  (+ts 'dart "https://github.com/UserNobody14/tree-sitter-dart")
  (+lsp 'dart-ts-mode "dart-analysis-server"))

(use-package svelte-ts-mode
  :ensure (:host github :repo "leafOfTree/svelte-ts-mode")
  :config
  (dolist (e svelte-ts-mode-language-source-alist) (apply '+ts e))
  (+lsp 'svelte-ts-mode "svelteserver"))

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package text-mode
  :ensure nil
  :mode "readme\\'")

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

(map! spc
  "a"   '("act" . embark-act)
  "b c" 'combobulate)

(map! nil
  "M-e" #'embark-act)


;; Completions

(use-package emacs
  :ensure nil
  :custom
  (tab-always-indent 'complete)
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; (enable-recursive-minibuffers t)
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

(map! nil
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

  (define-advice elisp-flymake-byte-compile (:around (orig-fn &rest args) inherit-load-path)
    "Make Flymake inherit `load-path' that might be modified by Elpaca.
This remove warnings on `use-package' body.
https://emacs.stackexchange.com/a/78310"
    (let ((elisp-flymake-byte-compile-load-path (append elisp-flymake-byte-compile-load-path load-path)))
      (apply orig-fn args)))

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

  (define-advice flymake-eldoc-function (:override (&rest _) disable)
    "Disable eldoc function for flymake."
    nil)
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

(map! spc
  "c c" '("compile" . compile)
  "d"   '(:keymap dape-global-map :package dape :which-key "dape")
  "o e" '("error"   . flymake-show-buffer-diagnostics)
  "p c" '("compile" . project-compile))

(map! normal
  "] e" #'flymake-goto-next-error
  "[ e" #'flymake-goto-prev-error)


;; LSP

(use-package nerd-icons-corfu
  :config
  (defun +nic-alias (alias from)
    "Add ALIAS mapping FROM `nerd-icons-corfu-mapping'."
    (add-to-list 'nerd-icons-corfu-mapping (cons alias (cdr (assoc from nerd-icons-corfu-mapping)))))

  (+nic-alias 'feature 'keyword)
  (+nic-alias (intern "special form") 'keyword)
  (+nic-alias 'custom 'variable)
  (+nic-alias 'search 'text)
  (+nic-alias 'face 'color))

(use-package lsp-bridge
  :after (yasnippet markdown-mode orderless nerd-icons-corfu)
  :ensure (:host github :repo "manateelazycat/lsp-bridge"
                 :files (:defaults "*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 :build (:not elpaca--byte-compile))
  :custom
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

  (lsp-bridge-enable-completion-in-minibuffer t)
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
  ;; Sort snippets first, then other candidates
  (delete "template-first-part-candidates" acm-completion-backend-merge-order)
  (setq acm-completion-backend-merge-order (cons "template-first-part-candidates" acm-completion-backend-merge-order))

  ;; Use nerd icons
  (defvar +acm-nerd-icon-mapper
    '(("material" :fn nerd-icons-mdicon :prefix "nf-md-")
      ("octicons" :fn nerd-icons-octicon :prefix "nf-oct-")
      ("codicons" :fn nerd-icons-codicon :prefix "nf-cod-")
      ("cod" :fn nerd-icons-codicon :prefix "nf-cod-")))

  (define-advice acm-icon-build (:around (orig-fn collection name fg-color) use-nerd-icons)
    (if-let* ((_ acm-enable-icon)
              (map (cdr (assoc collection +acm-nerd-icon-mapper)))
              (nf-fn (plist-get map :fn))
              (prefix (plist-get map :prefix))
              (nf-name (concat prefix name))
              (face `(:foreground ,fg-color)))
        (concat " " (funcall nf-fn nf-name :face face) nerd-icons-corfu--space)
      (funcall orig-fn collection name fg-color)))

  (define-advice lsp-bridge-breadcrumb--icon (:around (orig-fn kind active) use-nerd-icons)
    (if-let* ((_ lsp-bridge-breadcrumb-show-icon)
              (icon (cdr (assoc (downcase kind) acm-icon-alist)))
              (icon (or icon (cdr (assoc t acm-icon-alist))))
              (collection (nth 0 icon))
              (_ (assoc collection +acm-nerd-icon-mapper))
              (name (nth 1 icon))
              (fg-color (if active (nth 2 icon) (face-foreground 'mode-line-inactive))))
        (acm-icon-build collection name fg-color)
      (funcall orig-fn kind active)))

  ;; Use the same icons as nerd-icons-corfu
  (dolist (l nerd-icons-corfu-mapping)
    (when-let* ((key (symbol-name (car l)))
                (_ (assoc key acm-icon-alist))
                (map (cdr l))
                (style (plist-get map :style))
                (icon (plist-get map :icon))
                (face (plist-get map :face))
                (color (face-foreground face)))
      (setf (cdr (assoc key acm-icon-alist)) (list style icon color))))

  ;; Replace - with _ as that's what nerd icons uses
  (dolist (map acm-icon-alist)
    (when (assoc (nth 1 map) +acm-nerd-icon-mapper)
      (setf (nth 2 map) (replace-regexp-in-string "-" "_" (nth 2 map)))))

  (evil-set-initial-state 'lsp-bridge-ref-mode 'insert)
  (+pop "*lsp-bridge-doc*")
  (+pop '(major-mode . lsp-bridge-ref-mode) 'bottom)

  (defcustom +lsp-bridge-root-config '()
    "LSP-Bridge root directories."
    :type '(repeat directory)
    :group '+deirn)

  ;; Get root folder from custom alist
  (setq lsp-bridge-get-project-path-by-filepath
        (lambda (file-path)
          (cl-find-if (lambda (e) (string-prefix-p e file-path))
                      +lsp-bridge-root-config)))

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

  (define-advice lsp-bridge--enable (:after () extra)
    (setq-local completion-in-region-function (lambda (&rest _) (call-interactively #'lsp-bridge-popup-complete-menu)))
    (unless (member major-mode +--nobreadcrumb) (lsp-bridge-breadcrumb-mode 1)))

  (define-advice lsp-bridge--disable (:after () extra)
    (lsp-bridge-breadcrumb-mode -1)
    (lsp-bridge-kill-process))

  (defvar +lsp-bridge-doc-mode-map (make-sparse-keymap))
  (define-minor-mode +lsp-bridge-doc-mode
    "Minor mode for *lsp-bridge-doc* buffer.")

  (defun +setup-lsp-bridge-doc-buffer ()
    "Setup *lsp-bridge-doc* buffer."
    (when-let* ((buf (get-buffer "*lsp-bridge-doc*")))
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

(defun +show-symbols ()
  "Show symbols in buffer."
  (interactive)
  (if (+has-lsp)
      (call-interactively #'lsp-bridge-imenu)
    (call-interactively #'consult-imenu)))

(map! spc
  "c a" '("action"        . lsp-bridge-code-action)
  "c r" '("rename symbol" . lsp-bridge-rename)
  "c s" '("symbol list"   . consult-imenu)
  "t l" '("lsp"           . lsp-bridge-mode))

(map! normal
  "K"   #'+show-documentation
  "g d" #'lsp-bridge-xref-find-definition
  "g r" #'lsp-bridge-xref-find-references)


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

(map! spc
  "c f" '("format" . +format-buffer))



;;; init.el ends here.
