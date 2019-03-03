;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :after key-seq
  :commands (avy-goto-word-1
             avy-goto-word-or-subword-1
             avy-goto-char-in-line
             avy-goto-line
             avy-goto-char)
  :config
  (setq avy-keys '(?a ?t ?u ?s ?i ?r ?e ?n ?p ?d ?l))
  (setq avy-all-windows nil)
  (setq avy-styles-alist
        '((avy-goto-char-in-line . post)
          (avy-goto-word-or-subword-1 . post)
          (avy-goto-word-1 . pre)))
)


(use-package counsel
  :after ivy
  :diminish (counsel-mode)
  :hook (after-init-hook . counsel-mode)
  :bind*
  (("M-x"     . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-r" . counsel-recentf)
   ("C-c C-f" . counsel-git)
   ("C-c C-s" . counsel-git-grep)
   ("C-c /"   . counsel-ag)
   ("C-c l"   . counsel-locate)
   ("C-h a"   . counsel-apropos)
   ("C-h b"   . counsel-descbinds)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-h l"   . counsel-find-library)
   ("C-h i"   . consel-info-lookup-symbol))
  :config
  (setq counsel-find-file-ignore-regexp
        (concat "\\(?:\\`[#.]\\)\\|\\(?:[#~]\\'\\)"
                "\\|\\.x\\'\\|\\.d\\'\\|\\.o\\'"
                "\\|\\.aux\\'"))
  (setq counsel-locate-cmd 'counsel-locate-cmd-mdfind)
  (setq counsel-find-file-at-point t)

)

(use-package counsel-gtags
  :disabled t
  :hook (ggtags-mode . counsel-gtags-mode)
  :bind (:map c-mode-map
         ("C-c g g" . counsel-gtags-dwim)
         ("C-c g s" . counsel-gtags-find-symbol)
         ("C-c g d" . counsel-gtags-find-definition)
         ("C-c g r" . counsel-gtags-find-reference)
         ("C-c g f" . counsel-gtags-find-file)
         ("C-c g c" . counsel-gtags-create-tags)
         ("C-c g u" . counsel-gtags-update-tags))
)


(use-package ivy
  :defer 0.1
  :diminish (ivy-mode)			;dont display ivy in modeline
  ;; :after (counsel swiper)
  :bind
  (:map ivy-mode-map ("C-'" . ivy-avy))
  :config
  (setq-default ivy-display-style 'fancy
                ivy-display-function nil
                ivy-use-virtual-buffers t
                enable-recursive-minibuffers t
                ivy-use-selectable-prompt t
                ;; no regexp by default
                ivy-initial-inputs-alist nil
		;setq ivy-height 10
                ivy-count-format "(%d/%d) ")
  (ivy-mode 1)

  ;; IDO-style directory navigation
  ;; (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  ;; (dolist (k '("C-j" "C-RET"))
  ;;   (define-key ivy-minibuffer-map (kbd k) #'ivy-immediate-done))

  ;; (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)

  ;; (define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)
  )


(use-package swiper
  :after (ivy)
  :bind
  (:map ivy-mode-map ("M-s /" . swiper-at-point))
  :config
  (defun swiper-at-point (sym)
    "use swiper to search for the symbol at point."
    (interactive (list (thing-at-point 'symbol)))
    (swiper sym))
  )

(provide 'init-ivy)
;;; init-ivy.el ends here
