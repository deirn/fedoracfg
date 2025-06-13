;;; -*- lexical-binding: t; -*-

(add-to-list 'load-path (expand-file-name "share/lsp-bridge" (getenv "FEDORACFG")))

(require 'yasnippet)
(require 'lsp-bridge)
(require 'flymake-bridge)

(yas-global-mode)

(setq
 ;; use uv, so it has consistent package version
 lsp-bridge-python-command "uv"
 lsp-bridge-enable-hover-diagnostic t
 ;; use flymake-bridge, see below
 lsp-bridge-diagnostic-enable-overlays nil

 ;; Use minibuffer instead of popup for code action
 lsp-bridge-code-action-preview-delay nil
 lsp-bridge-code-action-enable-popup-menu nil

 acm-enable-capf t
 acm-enable-icon t
 acm-enable-tabnine nil
 acm-candidate-match-function #'orderless-flex)

(map!
 :n "] e" #'flymake-goto-next-error
 :n "[ e" #'flymake-goto-prev-error

 ;; lsp-bridge map

 (:mode 'lsp-bridge-mode
  :n "g d" #'lsp-bridge-find-def
  :n "g D" #'lsp-bridge-find-references
  :n "K" #'lsp-bridge-show-documentation

  (:leader
   "c a" #'lsp-bridge-code-action
   "c d" #'lsp-bridge-find-def
   "c D" #'lsp-bridge-find-references
   "c f" #'lsp-bridge-code-format)))

(set-popup-rule! "\\*lsp-bridge" :select t :quit t)

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

(dolist (mode '(dart typescript))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            #'-my/enable-lsp-bridge))
