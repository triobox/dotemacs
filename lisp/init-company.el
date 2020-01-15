;;; init-company.el --- Completion with company -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :defer t
  :diminish company-mode
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))

  (eval-after-load "company"
    '(add-to-list 'company-backends 'company-anaconda))

  (define-key company-mode-map (kbd "M-/") 'company-complete)
  (define-key company-active-map (kbd "M-/") 'company-other-backend)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  (setq-default company-dabbrev-other-buffers 'all
		company-tooltip-align-annotations t)

  ;; (global-set-key (kbd "M-C-/") 'company-complete)
  )

 (use-package company-quickhelp
    :defer t
    :config
    (add-hook 'after-init-hook 'company-quickhelp-mode))

(provide 'init-company)
;;; init-company.el ends here
