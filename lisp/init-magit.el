;;; init-magit.el --- Git SCM support -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defer t
  :commands (magit-blame
             magit-commit
             magit-commit-popup
             magit-diff-popup
             magit-diff-unstaged
             magit-fetch-popup
             magit-init
             magit-log-popup
             magit-pull-popup
             magit-push-popup
             magit-revert
             magit-stage-file
             magit-status
             magit-unstage-file
             magit-blame-mode)
  :bind* (("C-x g" . magit-status))
  :config
  (global-git-commit-mode)

  (use-package magit-popup)
  (use-package git-commit :defer t)

  (use-package magit-gitflow
    :defer t
    :commands turn-on-magit-gitflow
    :bind (:map magit-mode-map
		("%" . magit-gitflow-popup))
    :init
    (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    )

  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-buffer-name-format "%x%M%v: %t%x")
  )

;; (use-package magit-todos
;;   :ensure t
;;   :after magit
;;   :config
;;   (magit-todos-mode))

(use-package git-gutter
  :defer t
  :config
  (global-git-gutter-mode 't)
  :diminish git-gutter-mode)

(use-package git-timemachine :defer t)


(provide 'init-magit)
;;; init-magit.el ends here
