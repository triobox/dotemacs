;;; init-misc.el --- Misc packages -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package scratch)

(use-package rainbow-delimiters
  :commands rainbow-delimiters-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package command-log-mode
  :disabled t
  :defer 3
  :diminish (command-log-mode . " Cml")
  :init
  (setq command-log-mode-window-font-size 1)
  (setq command-log-mode-auto-show 1)
  :config
  ;; (command-log-mode 1)
  )

(use-package prog-mode
  :ensure nil
  :config
  ; display-line-numbers-mode only available in *emacs26*
  (if *emacs26*
      (add-hook 'prog-mode-hook 'display-line-numbers-mode))
  ; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (global-prettify-symbols-mode))

;; Highlight strings which represent colours
(use-package rainbow-mode
  :commands (rainbow-mode)
  :config
  (setq rainbow-x-colors nil)
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package restart-emacs
  :defer t
  :commands (restart-emacs))

(use-package smex :defer t)

(use-package visual-regexp-steroids
  :defer 3
  :bind* (("s-%" . vr/replace)
          ("M-s-%" . vr/query-replace))
  :config
  (setq vr/auto-show-help nil))


;; Easier selection
(use-package expand-region
    :defer t
    :bind ("C-=" . er/expand-region))


 ;; highlight changed-and-uncommitted lines when programming
(use-package diff-hl
  :defer t
  :config
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode)
  )

;; Set up ag for displaying search results.
(use-package ag :defer t)

(use-package key-seq
  :after key-chord
  :config
  (key-seq-define-global "qb" #'counsel-bookmark)
  (key-seq-define-global "qc" #'avy-goto-word-1)
  (key-seq-define-global "qd" #'kill-this-buffer)
  (key-seq-define-global "qf" #'ivy-switch-buffer)
  (key-seq-define-global "ql" #'avy-goto-line)
  (key-seq-define-global "qp" #'hydra-projectile/body)
  (key-seq-define-global "qs" #'save-buffer)
  (key-seq-define-global "qg" #'magit-status))


;; (use-package dashboard
;;   :preface
;;   (defun my/dashboard-banner ()
;;     "Set a dashboard banner including information on package initialization
;;      time and garbage collections."
;;     (setq dashboard-banner-logo-title
;;           (format "Emacs ready in %.2f seconds with %d garbage collections."
;;                   (float-time (time-subtract after-init-time before-init-time)) gcs-done)))
;;   :init
;;   (add-hook 'after-init-hook 'dashboard-refresh-buffer)
;;   (add-hook 'dashboard-mode-hook 'my/dashboard-banner)
;;   :custom (dashboard-startup-banner 'logo)
;;   :config (dashboard-setup-startup-hook))

(use-package browse-kill-ring
  :defer t
  :bind ("C-c y" . browse-kill-ring)
  :config
  )

(provide 'init-misc)
;;; init-misc.el ends here
