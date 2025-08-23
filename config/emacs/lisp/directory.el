;;; directory.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;;   Directory
;;; Code:

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

;;; directory.el ends here.
