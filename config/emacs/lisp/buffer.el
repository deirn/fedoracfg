;;; buffer.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Buffer
;;; Code:

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

;;; buffer.el ends here.
