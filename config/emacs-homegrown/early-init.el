;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;; https://github.com/doomemacs/doomemacs/blob/e6c755305358412a71a990fc2cf592c629edde1e/early-init.el#L36
(if noninteractive
    (setq gc-cons-threshold 134217728  ; 128mb
          gc-cons-percentage 1.0)
  (setq gc-cons-threshold most-positive-fixnum))

;;; early-init.el ends here
