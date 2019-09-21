;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :defer 0.3
  :diminish (projectile-mode . " Prj")
  ;;Remove the mode name for projectile-mode, but show the project name
  ;; :delight '(:eval (concat " " (projectile-project-name)))
  ;; :bind (("C-c p" . hydra-projectile/body))
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  )

;; (use-package ibuffer-projectile :ensure t)

(use-package counsel-projectile
  :defer t
  :config
  (counsel-projectile-mode +1))

(defhydra hydra-projectile (:color blue :columns 3)
  "  Projectile  "

  ("o" counsel-projectile "counsel-project")
  ("b" counsel-projectile-switch-to-buffer "switch-buffer")
  ("d" counsel-projectile-find-dir "find-dir")
  ("D" projectile-dired "proj-root")
  ("f" counsel-projectile-find-file "find-file")
  ("i" projectile-invalidate-cache "reset-cache" :color red)
  ("K" projectile-kill-buffers "kill-all")
  ("p" counsel-projectile-switch-project "switch-project")
  ("r" projectile-replace "replce")
  ("R" projectile-replace-regexp "regexp-replace")
  ("s" counsel-projectile-git-grep "search-git")
  ("S" projectile-save-project-buffers "save-all")
  ("q" nil "quit" :color red)

  )

(provide 'init-projectile)
;;; init-projectile.el ends here
