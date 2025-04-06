;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; IF you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq doom-font (font-spec :family "JetBrainsMono NF" :size 14))
;; Fallback, for usage in LSP modeline
(add-to-list 'doom-symbol-fallback-font-families "Noto Emoji")

;; Make emacs fullscreen on start
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))
;; Hide title bar
;; (add-to-list 'default-frame-alist '(undecorated . t))

;; Increase window divider size
(setq window-divider-default-bottom-width 5
      window-divider-default-right-width 5)

;; Make dired delete to trash
(setq delete-by-moving-to-trash t)

;; ;; Always open link with sensible-browser
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "sensible-browser")

;; File assoctiations
(defun my/assoc-ext (ext mode)
  (add-to-list 'auto-mode-alist `(,(concat "\\." ext "\\'") . ,mode)))
(my/assoc-ext "bean" 'beancount-mode)

;; Prisma
(my/assoc-ext "prisma" 'prisma-mode)
(after! prisma-mode (add-hook 'prisma-mode-hook #'lsp! 'append))

;; Svelte
;; (use-package! svelte-mode :after '(typescript-mode javascript-mode))
;; (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
;; (after! svelte-mode
;;   (add-hook 'svelte-mode-hook #'lsp! 'append))

;; TODO: TailwindCSS
;; (use-package! lsp-tailwindcss
;;   :after lsp-mode
;;   :init (setq lsp-tailwindcss-add-on-mode t))
;; (after! lsp-tailwindcss
;;   (setq lsp-tailwindcss-skip-config-check t))

;; Discord rich presense
(use-package! elcord
  :config
  (require 'elcord)
  (setq elcord-editor-icon "emacs_icon")
  (setq elcord-use-major-mode-as-main-icon t)
  (elcord-mode))

(after! vterm
  (setq vterm-shell "/usr/bin/env zsh"))

(after! lsp-java
  ;; Configure Java configuration runtimes, generated in ../packages.nix
  (load! "~/.jdk/doom.el" nil t)
  ;; Download newer jdtls that supports newer Java.
  ;; Run `lsp-update-server' after changed.
  ;; https://github.com/emacs-lsp/lsp-java/issues/478
  ;; https://download.eclipse.org/jdtls/milestones/
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.43.0/jdt-language-server-1.43.0-202412191447.tar.gz")
  ;; Give jdtls 2GB max
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx2G" "-Xms100m")))

(after! treemacs
  ;; Collapse empty directories
  (setq treemacs-collapse-dirs 10))

(after! lsp-mode
  ;; https://github.com/emacs-lsp/lsp-mode/blob/master/docs/tutorials/how-to-turn-off.md
  (setq lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        ;; Always ask before executing actions even for single action
        lsp-auto-execute-action nil)
  ;; Add directories to ignore from LSP watch
  (dolist (dir '("[/\\\\]\\.devenv"
                 "[/\\\\]\\.direnv"))
    (push dir lsp-file-watch-ignored-directories)))

;; HACK: Translate C-i to H-i so it can be used in daemon mode
(defun my/C-i ()
  (when (display-graphic-p)
    (key-translate "C-i" "H-i")))
(my/C-i)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (with-selected-frame frame (my/C-i))))

(after! better-jumper
  (keymap-global-set "H-i" #'better-jumper-jump-forward))

;; HACK: On daemon mode, ask confirmation before closing Emacsclient
(defun my/confirm-exit ()
  (interactive)
  (if (y-or-n-p "Really exit Emacsclient? ")
      (save-buffers-kill-terminal)
    (message "Canceled.")))
(when (daemonp)
  (map!
   :g "C-x C-c" #'my/confirm-exit
   :leader "q q" #'my/confirm-exit))
