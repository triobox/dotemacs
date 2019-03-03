;;; init-epa.el --- EasyPG settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package epa-file
  :ensure nil
  :init (epa-file-enable)
  :bind ("C-c x" . hydra-epa/body)
  :config
  (progn
    ;; use symmetric encryption
    (setq epa-file-select-keys nil)
    ;; disable auto-saving when opening an encrypted file
    (setq epa-file-inhibit-auto-save t)
    ))



(defhydra hydra-epa (:color blue :columns 2)
  "
      Cryptography:
"
  ("d" epa-decrypt-region "decrypt-region")
  ("s" epa-sign-region "sign-region")
  ("e" epa-encrypt-region "encrypt-region")
  ("v" epa-verify-region "verify-region")
  ("D" epa-decrypt-file "decrypt-file")
  ("S" epa-sign-file "sign-file")
  ("E" epa-encrypt-file "encrypt-file")
  ("V" epa-verify-file "verify-file")
  ("i" epa-insert-keys "insert-keys")
  ("I" epa-import-keys "import-keys")
  ("k" epa-list-keys "list-public-keys")
  ("K" epa-list-secret-keys "list-private-keys")
  ("q" nil "quit" :color red)
  )

(provide 'init-epa)
;;; init-epa.el ends here
