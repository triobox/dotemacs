;;; init-javascript.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package js2-mode
  :defer t
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
    (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)

    (setq-default js-indent-level 2)
    (setq-default js2-basic-offset 2)

    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode))
    )
  :config
  (progn
    ;; key bindings
    )
  )

(use-package json-mode
  :defer t)

;; (use-package js2-refactor
;;   :defer t
;;   :init
;;   (progn
;;     )
;;   )

(use-package skewer-mode
  :defer t
  :init
  (progn
    (add-hook 'js2-mode-hook 'skewer-mode)
    (add-hook 'css-mode-hook 'skewer-css-mode)
    (add-hook 'html-mode-hook 'skewer-html-mode)
    )
  :config
  (progn
    )
  )

(provide 'init-javascript)
;;; init-javascript.el ends here
