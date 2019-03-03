;;; init-theme.el --- Setting for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package doom-themes
  ;; :config
  ;; (load-theme 'doom-one t)
  )

(use-package sublime-themes
  ;; :config
  ;; (load-theme 'doom-one t)
  )


(use-package solarized-theme
  :defer t
  :config
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)
  (setq solarized-use-variable-pitch t)
  (setq solarized-high-contrast-mode-line t)
  (setq solarized-use-more-italic t)
  (setq solarized-scale-org-headlines t)

  (defun solarized-no-background! ()
    (interactive)
    (set-face-background 'default "unspecified-bg"))
  )

 (cond (*win64*
         (load-theme 'leuven t))
         (t
         (load-theme 'solarized-dark t)
         ;; (solarized-no-background!)
	 ))

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
