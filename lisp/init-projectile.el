;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :defer t
  :diminish (projectile-mode . " Prj")
  ;;Remove the mode name for projectile-mode, but show the project name
  ;; :delight '(:eval (concat " " (projectile-project-name)))
  :bind (("C-c p" . hydra-projectile/body))
  :config
  (projectile-mode +1)
  ;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)


  ;; (use-package ibuffer-projectile :ensure t)
  (use-package counsel-projectile
    :defer t
    :config
    (counsel-projectile-mode +1))

 )

(defhydra hydra-projectile (:color blue)
  "
  ^
  ^Projectile^        ^Buffers^           ^Find^              ^Search^
  ^──────────^────────^───────^───────────^────^──────────────^──────^────────────
  _q_ quit            _b_ list            _d_ directory       _r_ replace
  _i_ reset cache     _K_ kill all        _D_ root            _R_ regexp replace
  ^^                  _S_ save all        _f_ file            _s_ search
  ^^                  ^^                  _p_ project         ^^
  ^^                  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("b" counsel-projectile-switch-to-buffer)
  ("d" counsel-projectile-find-dir)
  ("D" projectile-dired)
  ("f" counsel-projectile-find-file)
  ("i" projectile-invalidate-cache :color red)
  ("K" projectile-kill-buffers)
  ("p" counsel-projectile-switch-project)
  ("r" projectile-replace)
  ("R" projectile-replace-regexp)
  ("s" counsel-projectile-git-grep)
  ("S" projectile-save-project-buffers))



(provide 'init-projectile)
;;; init-projectile.el ends here
