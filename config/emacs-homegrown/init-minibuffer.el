;;; init-minibuffer.el -*- lexical-binding: t; -*-

(use-package vertico
  :config
  (vertico-mode))

(use-package marginalia
  :after vertico
  :config
  (marginalia-mode))

(use-package consult
  :after vertico)

(use-package embark
  :after (vertico marginalia))

(use-package embark-consult
  :after (embark consult))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package which-key
  :ensure nil
  :init
  (which-key-mode))

;;; init-minibuffer.el ends here
