;;; init-flycheck.el --- Configure Flycheck global behaviour -*- lexical-binding: t -*-
;;; commentary:
;;; code:

(use-package flycheck
  :defer 1
  ;; :after (which-key)
  :bind ("C-c f" . hydra-flycheck/body)
  :config
  (global-flycheck-mode)
  (setq flycheck-display-errors-function
	#'flycheck-display-error-messages-unless-error-list)
  (setq flycheck-global-modes '(not org-mode latex-mode))
  ;; (which-key-add-key-based-replacements "C-c !" "flycheck-command-map")
  )

;; (use-package flycheck-color-mode-line
;;   :defer 1
;;   :init
;;   (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line))


(defhydra hydra-flycheck (:color blue)
  "
  ^
  ^Flycheck^          ^Errors^            ^Checker^
  ^────────^──────────^──────^────────────^───────^─────
  _q_ quit            _<_ previous        _?_ describe
  _M_ manual          _>_ next            _d_ disable
  _v_ verify setup    _f_ check           _m_ mode
  ^^                  _l_ list            _s_ select
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("<" flycheck-previous-error :color pink)
  (">" flycheck-next-error :color pink)
  ("?" flycheck-describe-checker)
  ("M" flycheck-manual)
  ("d" flycheck-disable-checker)
  ("f" flycheck-buffer)
  ("l" flycheck-list-errors)
  ("m" flycheck-mode)
  ("s" flycheck-select-checker)
  ("v" flycheck-verify-setup))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
