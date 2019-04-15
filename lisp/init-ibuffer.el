;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:

(use-package ibuffer
  :diminish ibuffer-mode
  :bind* (("C-x C-b" . ibuffer)
	  ("C-x d" . kill-current-buffer)
	  ;; ("C-x C-b" . hydra-buffer/body)
	  )
  :commands (ibuffer)
  :config

  (setq-default ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-display-summary t)

  (use-package ibuffer-vc)
  ;; (use-package ibuffer-vc
  ;;   :ensure t
  ;;   :config
  ;;   (setq ibuffer-vc-set-filter-groups-by-vc-root t))


  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  ;; Modify the default ibuffer-formats
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size-h 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                filename-and-process)))


  (defun ibuffer-set-up-preferred-filters ()
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process)))

  (setq-default ibuffer-saved-filter-groups
                `(("Default"
                   ("RStat" (or (mode . ess-mode)
                                (mode . ess-help-mode)
                                (mode . inferior-ess-mode)))
                   ("C / C++" (mode . c-mode))
                   ("Org" (mode . org-mode))
                   ("Markdown" (mode . markdown-mode))
                   ("Bash" (or (mode . shell-script-mode)))
                   ("Make" (mode . makefile-mode))
                   ("Dired" (mode . dired-mode))
                   ("PDF" (mode . pdf-view-mode))
                   ("Mail" (or (mode . message-mode)
                               (mode . bbdb-mode)
                               (mode . mail-mode)
                               (mode . mu4e-compose-mode)))
                   ("Elisp" (mode . emacs-lisp-mode))
                   ("Scheme" (mode . scheme-mode))
                   ("shell" (or (mode . eshell-mode)
                                (mode . shell-mode)))
                   ("Magit" (or (mode . magit-mode)))
                   ("Temp" (name . "\*.*\*")))))

  (add-hook 'ibuffer-hook
	    'ibuffer-set-up-preferred-filters
	    ; 'ibuffer-switch-to-saved-filter-groups
	    )
  )


(defhydra hydra-buffer (:color blue :columns 3)
  "
                Buffers :
  "
  ("n" next-buffer "next" :color red)
  ("b" ivy-switch-buffer "switch")
  ("i" ibuffer "ibuffer")
  ("p" previous-buffer "previous" :color red)
  ;; ("C-b" buffer-menu "buffer menu")
  ("N" evil-buffer-new "new")
  ("k" kill-this-buffer "kill" :color red)
  ;; don't come back to previous buffer after delete
  ("d" (progn (kill-this-buffer) (next-buffer)) "kill & next" :color red)
  ("s" save-buffer "save" :color red)
)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
