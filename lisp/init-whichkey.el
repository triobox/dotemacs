;;; init-whichkey.el --- Settings for which-key -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :defer 2
  :diminish which-key-mode
  :hook (after-init . which-key-mode)
  :config
  ;; (which-key-mode)
  (which-key-setup-side-window-bottom)
  (which-key-setup-minibuffer)
  (setq which-key-separator " -> ")
  (setq which-key-prefix-prefix "+")
  (setq which-key-sort-order 'which-key-prefix-then-key-order)
  ;; (add-hook 'after-init-hook 'which-key-mode)
  )

(provide 'init-whichkey)

;;; init-whichkey.el ends here
