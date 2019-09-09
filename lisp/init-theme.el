;;; init-theme.el --- Setting for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (require 'color-theme-modern)


(use-package solarized-theme
  :init
  ;; make the fringe stand out from the background
  (setq solarized-distinct-fringe-background t)

  ;; Don't change the font for some headings and titles
  (setq solarized-use-variable-pitch nil)

  (setq solarized-high-contrast-mode-line t)

  ;; Use less bolding
  (setq solarized-use-less-bold t)

  ;; (setq solarized-use-more-italic t)

  ;; Don't change size of org-mode headlines (but keep other size-changes)
  (setq solarized-scale-org-headlines nil)

  (setq x-underline-at-descent-line t)

  ;; Avoid all font-size changes
  (setq solarized-height-minus-1 1.0)
  (setq solarized-height-plus-1 1.0)
  (setq solarized-height-plus-2 1.0)
  (setq solarized-height-plus-3 1.0)
  (setq solarized-height-plus-4 1.0)

  ;; :config

  )

(setq my-dark-theme       'solarized-dark
      my-light-theme      'solarized-light)

(if *linux*
    (load-theme my-dark-theme t)
  (load-theme my-light-theme t))


(defun toggle-night-color-theme ()
  "Switch to/from night color scheme"
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (progn
        (load-theme my-dark-theme t)
        ;; (if *linux*
        ;;    (do-somthing))
	)
    (load-theme my-light-theme t)
    ;; (if *linux*
    ;;     (do-somthing))
    )
  )

;; Toggle between light and dark themes with F7
(global-set-key (kbd "<f7>") 'toggle-night-color-theme)


(provide 'init-theme)
;;; init-theme.el ends here
