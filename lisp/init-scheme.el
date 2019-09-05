;;; init-scheme.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package cmuscheme
;;   :defer t
;;   :ensure nil
;;   :bind (:map scheme-mode-map)
;;   :init
;;   (progn
;;     (when *linux*
;;       (setq scheme-program-name "/usr/bin/chezscheme"))
;;     (when *win64*
;;       (setq scheme-program-name "d:/Program/Chez_9.5.2/bin/ta6nt/scheme.exe"))
;;    )
;;   )

(use-package geiser
  :defer t
  :commands run-geiser
  :bind (:map scheme-mode-map
	      ("C-c e" . hydra-scheme/body))
  :init
  (progn
    (setq geiser-active-implementations '(chez))
    (when *linux*
      (setq geiser-chez-binary "/usr/bin/chezscheme"))
    (when *win64*
      (setq geiser-chez-binary "d:/Program/Chez_9.5.2/bin/ta6nt/scheme.exe"))
    )

  :config
  (setq geiser-repl-history-filename "~/.emacs.d/geiser-history")
  (setq geiser-repl-query-on-kill-p nil)
  )

(use-package ac-geiser
  :defer t
  :init
  (add-hook 'geiser-mode-hook 'ac-geiser-setup)
  (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'geister-repl-mode))
  )

(defhydra hydra-scheme (:color blue :columns 3)
  "scheme"
  ("c" geiser-compile-current-buffer "compile buffer")
  ("b" geiser-eval-buffer "eval buffer")
  ("B" geiser-eval-buffer-and-go "eval buffer-go")
  ("e" geiser-eval-last-sexp "eval s-exp")
  ("f" geiser-eval-definition "eval definition")
  ("F" geiser-eval-definition-and-go "eval def-go")
  ("i" geiser-insert-lambda "insert lambda")
  ("l" geiser-load-file "load file")
  ("m" geiser-edit-module "edit module")
  ("r" geiser-eval-region "eval region")
  ("R" geiser-eval-region-and-go "eval reg-go")
  ("z" geiser-mode-switch-to-repl "switch to Repl")
  ("q" nil "quit")
  )

(provide 'init-scheme)
;;; init-scheme.el ends here
