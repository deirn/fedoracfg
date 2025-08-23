;;; formatting.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Formatting
;;; Code:

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

;;; formatting.el ends here.
