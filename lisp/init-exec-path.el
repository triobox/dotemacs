;;; init-exec-path.el --- Set up exec-path to help Emacs find programs  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; By default Emacs doesn't read from the same
;; environment variables set in your terminal.
;; This package fixes that.
(use-package exec-path-from-shell
  :if *unix*
  :defer 0.2
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO" "LANG" "LC_CTYPE"))
    (add-to-list 'exec-path-from-shell-variables var))

  (when (memq window-system '(mac ns x))
    (setq-default exec-path-from-shell-arguments nil)
    (exec-path-from-shell-initialize))
  )


(provide 'init-exec-path)
;;; init-exec-path.el ends here
