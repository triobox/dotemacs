;;; init-lisp.el --- Setting for all lisps -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lispy
  :defer t
  :diminish (lispy-mode . " λ")
  ;; :diminish (lispy-mode . "")
  :commands lispy-mode
  ;; :hook ((emacs-lisp-mode-hook lisp-mode-hook) . lispy-mode)
  :init
  (add-hook 'emacs-lisp-mode-hook 'lispy-mode)
  (add-hook 'lisp-mode-hook 'lispy-mode)
  :config
  (setq lispy-avy-keys '(?a ?u ?i ?e ?t ?s ?r ?n ?m)))

;; (use-package lispyville
;;   :after (lispy)
;;   :defer t
;;   :diminish (lispyville-mode . " λe")
;;   :init
;;   (add-hook 'lispy-mode-hook #'lispyville-mode))


(defun lispy-insert-prev-outline-body ()
  (interactive)
  (save-excursion
    (insert
     (save-excursion
       (zo-up 1)
       (mapconcat
        'identity
        (cl-remove-if
         (lambda (s)
           (string-match-p "^;;" s))
         (split-string
          (string-trim
           (lispy--string-dwim
            (worf--bounds-subtree)))
          "\n"
          t))
        "\n")))))

(provide 'init-lisp)
;;; init-lisp.el ends here
