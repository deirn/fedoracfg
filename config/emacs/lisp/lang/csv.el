;;; csv.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :mode
  ("\\.csv\\'" . csv-mode)
  ("\\.tsv\\'" . tsv-mode))

(use-package rainbow-csv
  :ensure (:host github :repo "emacs-vs/rainbow-csv")
  :hook (csv-mode tsv-mode))

;;; csv.el ends here.
