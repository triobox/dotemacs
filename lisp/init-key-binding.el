;;; init-key-binding.el --- Personal Key bindings  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; -------------------------------------------------------
;; global Key-binding
;; -------------------------------------------------------
(global-set-key (kbd "C-j") 'delete-indentation)

;; Make C-x C-v undefined
;; (global-unset-key "\C-x\C-v ")

(provide 'init-key-binding)
;;; init-key-binding.el ends here
