;;; init-dumb-jump.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dumb-jump
  :bind ( ;; ("M-g o" . dumb-jump-go-other-window)
         ;; ("M-g j" . dumb-jump-go)
         ;; ("M-g i" . dumb-jump-go-prompt)
         ;; ("M-g x" . dumb-jump-go-prefer-external)
         ;; ("M-g z" . dumb-jump-go-prefer-external-other-window)
	 ("M-1" . dumb-jump-hydra/body)
	 )
  :config
  (define-key evil-normal-state-map (kbd "M-.") 'dumb-jump-go)
  (setq dumb-jump-selector 'ivy))

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other window")
    ("e" dumb-jump-go-prefer-external "Go external")
    ("x" dumb-jump-go-prefer-external-other-window "Go external other window")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

(provide 'init-dumb-jump)
;;; init-dumb-jump.el ends here
