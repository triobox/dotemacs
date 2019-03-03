;;; init-evil.el --- setting for evil -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; not to expand abbrevs when hitting escape
(setq evil-want-abbrev-expand-on-insert-exit nil)

(use-package evil
  :defer t
  :init
  ;; (setq evil-disable-insert-state-bindings t)
  :config
  (evil-mode 1))

(use-package evil-surround
  :defer t
  :config
  (global-evil-surround-mode 1))

(provide 'init-evil)
;;; init-evil.el ends here
