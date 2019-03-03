;;; init-paredit.el --- Configure paredit structured editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(use-package paredit
  :diminish (paredit-mode . " Par")
  :init
  (add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)
  :config
  ;; Suppress certain paredit keybindings to avoid clashes, including
  ;; my global binding of M-?
  (dolist (binding '("C-<left>" "C-<right>" "C-M-<left>" "C-M-<right>" "M-s" "M-?"))
    (define-key paredit-mode-map (read-kbd-macro binding) nil))
)

;; ----------------------------------------------------------------------------
;; Enable some handy paredit functions in all prog modes
;; ----------------------------------------------------------------------------
(use-package paredit-everywhere
  :hook (prog-mode . paredit-everywhere-mode)
  :config
  (define-key paredit-everywhere-mode-map (kbd "M-s") nil))


(provide 'init-paredit)
;;; init-paredit.el ends here
