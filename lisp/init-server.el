;;; init-server.el --- Allow access from emacsclient -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;
(use-package server
  :config
  (add-hook 'after-init-hook
	    (lambda ()
	      (unless (server-running-p) (server-start)))))


(provide 'init-server)

;;; init-server.el ends here
