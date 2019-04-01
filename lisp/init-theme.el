;;; init-theme.el --- Setting for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  :disabled t
  ;; :config
  ;; (load-theme 'doom-one t)
  )

(use-package sublime-themes
  :disabled t
  ;; :config
  ;; (load-theme 'doom-one t)
  )

(use-package tango-plus-theme)

(use-package solarized-theme
  :defer t
  :config
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-more-italic t)
  (setq solarized-scale-org-headlines t)
  )

(defun solarized-no-background! ()
    (interactive)
    (set-face-background 'default "unspecified-bg"))


;; ---------------------------------
;; (load-theme 'solarized-dark t)
;; (unless (display-graphic-p)
;;     (solarized-no-background!)
;;   (load-theme 'tango-plus t)
;;   )


(if *win64*
    (load-theme 'tango-plus t)
  (load-theme 'solarized-dark t)
  )

;; (cond ((display-graphic-p)
;;        (load-theme 'solarized-dark t))
;;       ;; (*win64*
;;       ;;  ;; (load-theme 'leuven t)
;;       ;;  ;; (load-theme 'solarized-light t)
;;       ;;  ;; (solarized-no-background!)
;;       ;;  )
;;       (t
;;        (load-theme 'solarized-dark t)
;;        ))

;; If you don't customize it, this is the theme you get.
;; (setq-default custom-enabled-themes '(solarized-dark))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; (add-hook 'after-init-hook 'reapply-themes)

(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(solarized-light))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(solarized-dark))
  (reapply-themes))

(provide 'init-theme)

;;; init-theme.el ends here
