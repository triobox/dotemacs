;;; init-theme.el --- Setting for themes -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

 ;; (require 'color-theme-modern)
(use-package smart-mode-line)
(use-package solarized-theme)

(setq my-dark-theme       'solarized-dark
      my-dark-theme-sml   'respectful
      my-light-theme      'solarized-light
      ;; my-light-theme      'tango-plus
      my-light-theme-sml  'automatic)

;; Workaround for confirmation of loading theme
(setq sml/no-confirm-load-theme t)

(defun toggle-night-color-theme ()
  "Switch to/from night color scheme"
  (interactive)
  (if (eq (frame-parameter (next-frame) 'background-mode) 'light)
      (progn
        (load-theme my-dark-theme nil nil)
        ;; (if *linux*
        ;;    (do-somthing))
        (setq sml/theme my-dark-theme-sml)
	)
    (load-theme my-light-theme nil nil)
    ;; (if *linux*
    ;;     (do-somthing))
    (setq sml/theme my-light-theme-sml)
    )
  (sml/setup)
  )

;; Toggle between light and dark themes with F7
(global-set-key (kbd "<f7>") 'toggle-night-color-theme)


(setq sml/theme           my-light-theme-sml
      sml/shorten-modes   t
      sml/mode-width      'full
      sml/name-width      25
      sml/hidden-modes    '(" Undo-Tree" " pair" " ARev" " GitGutter" " fs" " ElDoc" " WS" " Fly" " Abbrev" " Gtags" " Wrap" " AC" " ivy"))

(add-to-list 'sml/replacer-regexp-list '("^/repos/" ":Repo:"))

(if *linux*
    (setq sml/theme  my-dark-theme-sml)
 )

(sml/setup)

(provide 'init-theme)
;;; init-theme.el ends here
