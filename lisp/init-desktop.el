;;; init-desktop.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package desktop
  :ensure nil
  :bind (("C-c z" . hydra-desktop/body))
  :config
  (setq-default history-length 1000)
  ;; don't save frame and window config
  (setq desktop-restore-frame nil)
  (add-hook 'after-init-hook 'savehist-mode)

  ;; save a list of open files in ~/.emacs.d/.emacs.desktop
  ;; (setq desktop-path (list user-emacs-directory)
  ;;       desktop-auto-save-timeout 600)

  ;; Do not auto-load desktop file when emacs starts
  (desktop-save-mode nil)

  (setq desktop-buffers-not-to-save
       (concat "\\("
               "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
               "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
               "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

  ;; save a bunch of variables to the desktop file
  ;; for lists specify the len of the maximal saved data also
  (setq desktop-globals-to-save
        '((comint-input-ring        . 50)
          (compile-history          . 30)
          desktop-missing-file-warning
          (dired-regexp-history     . 20)
          (extended-command-history . 30)
          (face-name-history        . 20)
          (file-name-history        . 100)
          (grep-find-history        . 30)
          (grep-history             . 30)
          (ivy-history              . 100)
          (magit-revision-history   . 50)
          (minibuffer-history       . 50)
          (org-clock-history        . 50)
          (org-refile-history       . 50)
          (org-tags-history         . 50)
          (query-replace-history    . 60)
          (read-expression-history  . 60)
          (regexp-history           . 60)
          (regexp-search-ring       . 20)
          register-alist
          (search-ring              . 20)
          (shell-command-history    . 50)
          tags-file-name
          tags-table-list))

  (defun sanityinc/desktop-time-restore (orig &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig args)
        (message "Desktop restored in %.2fms"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)))))

  (advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

  (defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
    (let ((start-time (current-time)))
      (prog1
          (apply orig ver filename args)
        (message "Desktop: %.2fms to restore %s"
                 (sanityinc/time-subtract-millis (current-time)
                                                 start-time)
                 (when filename
                   (abbreviate-file-name filename))))))
  (advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

  )


;; (use-package session
;;   :ensure nil
;;   :config
;;   (setq session-save-file (expand-file-name ".session" user-emacs-directory))
;;   (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
;;   (setq session-save-file-coding-system 'utf-8)
;;   (add-hook 'after-init-hook 'session-initialize)
;;   )


(defhydra hydra-desktop (:color blue :columns 1)
  "
       Desktop:
  "
  ("s" desktop-save "save desktop")
  ("c" desktop-change-dir "change dir")
  ("p" desktop-revert "previous desktop")
  ("d" desktop-clear "empty desktop")
  ("q" nil "quit" :color red)
  )

;; (global-set-key (kbd "C-c z") 'hydra-desktop/body)

(provide 'init-desktop)
;;; init-desktop.el ends here
