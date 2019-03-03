;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package dired
  :defer t
  :ensure nil
  :commands dired
  :config
  (setq dired-dwim-target t)
  (setq dired-auto-revert-buffer t)
  (setq dired-ls-F-marks-symlinks nil)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'top)
  ;; Prefer g-prefixed coreutils version of standard utilities when available
  ;; (let ((gls (executable-find "gls")))
  ;;   (when gls (setq insert-directory-program gls)))
  ;; ask for each top-level dorectory specified by Dired deletion command
  (setq dired-listing-switches
	(if (memq system-type '(windows-nt darwin))
	    "-alh"
	  "-laGh1v --group-directories-first")))


(use-package dired-x
  :ensure nil
  :defer 0.5
  ;; :commands dired-jump
  :preface
  (defun my/dired-revert-after-cmd (command &optional output error)
    (revert-buffer))
  :config (advice-add 'dired-smart-shell-command :after #'my/dired-revert-after-cmd))


(use-package diredfl
  :after (dired)
  :config
  (diredfl-global-mode))


(provide 'init-dired)
;;; init-dired.el ends here
