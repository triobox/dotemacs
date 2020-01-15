;;; init-ace-window.el --- Setting of ace-window  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ace-window
  :diminish (ace-window-mode . " AceW")
  :bind (("C-x w" . ace-window))
  :config
  ;; (global-set-key (kbd "C-x w") 'ace-window)
  (setq aw-minibuffer-flag t)
  (setq aw-dispatch-always t)
  (defvar aw-dispatch-alist
    '((?x aw-delete-window "Delete Window")
      (?m aw-swap-window "Swap Windows")
      (?M aw-move-window "Move Window")
      (?c aw-copy-window "Copy Window")
      (?j aw-switch-buffer-in-window "Select Buffer")
      (?n aw-flip-window)
      (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
      (?F aw-split-window-fair "Split Fair Window")
      (?v aw-split-window-vert "Split Vert Window")
      (?b aw-split-window-horz "Split Horz Window")
      (?o delete-other-windows "Delete Other Windows")
      (?? aw-show-dispatch-help))
    "List of actions for `aw-dispatch-default'.")

  )



(provide 'init-ace-window)
;;; init-ace-window.el ends here
