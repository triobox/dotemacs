;;; init-ace-window.el --- Setting of ace-window  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ace-window
  :diminish (ace-window-mode . " AceW")
  :config
  (global-set-key (kbd "C-x w") 'ace-window)
  (setq aw-dispatch-always t)
  ;; (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (defvar aw-dispatch-alist
  ;;   '((?x aw-delete-window " Ace - Delete Window")
  ;;     (?m aw-swap-window " Ace - Swap Window")
  ;;     (?n aw-flip-window)
  ;;     (?v aw-split-window-vert " Ace - Split Vert Window")
  ;;     (?b aw-split-window-horz " Ace - Split Horz Window")
  ;;     (?i delete-other-windows " Ace - Maximize Window")
  ;;     (?o delete-other-windows))
  ;;   "List of actions for `aw-dispatch-default'.")

  )

(provide 'init-ace-window)
;;; init-ace-window.el ends here
