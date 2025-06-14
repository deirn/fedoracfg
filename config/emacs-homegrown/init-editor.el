;;; init-editor.el -*- lexical-binding: t; -*-

(use-package markdown-mode)
(use-package dart-mode)

(use-package emacs
  :ensure nil
  :config
  (electric-pair-mode))

(use-package evil
  :custom
  (evil-want-keybinding nil)
  (evil-undo-system 'undo-redo)
  :config
  (evil-mode))

(use-package evil-collection
  :init
  (evil-collection-init))

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

(use-package yasnippet
  :init
  (yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

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

;;; init-editor.el ends here
