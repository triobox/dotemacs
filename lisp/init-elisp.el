;;; init-elisp.el --- Insert description here -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package emacs-lisp-mode
  :ensure nil
  :mode (("*scratch*" . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook (lambda () (setq mode-name "EL")))
  (add-hook 'emacs-lisp-mode-hook 'set-up-hippie-expand-for-elisp)
  (add-hook 'ielm-mode-hook 'set-up-hippie-expand-for-elisp)
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'hydra-elisp/body)
  :config
  (progn

    (use-package elisp-slime-nav
      :ensure t
      :defer t
      :diminish elisp-slime-nav-mode
      :hook ((emacs-lisp-mode ielm-mode) . elisp-slime-nav-mode))

    (use-package ipretty
      :ensure t
      :hook (after-init . ipretty-mode))
    )
  )


(defun sanityinc/headerise-elisp ()
  "Add minimal header and footer to an elisp buffer in order to placate flycheck."
  (interactive)
  (let ((fname (if (buffer-file-name)
                   (file-name-nondirectory (buffer-file-name))
                 (error "This buffer is not visiting a file"))))
    (save-excursion
      (goto-char (point-min))
      (insert ";;; " fname " --- Insert description here -*- lexical-binding: t -*-\n"
              ";;; Commentary:\n"
              ";;; Code:\n\n")
      (goto-char (point-max))

      (insert ";;; " fname " ends here\n"))))


(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (pp-eval-last-sexp prefix)))

;(global-set-key [remap eval-expression] 'pp-eval-expression)

;; Use C-c C-z to toggle between elisp files and an ielm session
(defvar-local sanityinc/repl-original-buffer nil
  "Buffer from which we jumped to this REPL.")

(defvar sanityinc/repl-switch-function 'switch-to-buffer-other-window)

(defun sanityinc/switch-to-ielm ()
  "Switch to ielm."
  (interactive)
  (let ((orig-buffer (current-buffer)))
    (if (get-buffer "*ielm*")
        (funcall sanityinc/repl-switch-function "*ielm*")
      (ielm))
    (setq sanityinc/repl-original-buffer orig-buffer)))

(defun sanityinc/repl-switch-back ()
  "Switch back to the buffer from which we reached this REPL."
  (interactive)
  (if sanityinc/repl-original-buffer
      (funcall sanityinc/repl-switch-function sanityinc/repl-original-buffer)
    (error "No original buffer")))

;; (after-load 'elisp-mode
;;   (define-key emacs-lisp-mode-map (kbd "C-c C-z") 'sanityinc/switch-to-ielm))

(after-load 'ielm
  (define-key ielm-map (kbd "C-c C-z") 'sanityinc/repl-switch-back))

;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

;; ----------------------------------------------------------------------------
;; Automatic byte compilation
;; ----------------------------------------------------------------------------
(use-package auto-compile
  :ensure t
  :defer t
  :diminish auto-compile-mode
  :hook ((after-init . auto-compile-on-save-mode)
	 (after-init . auto-compile-on-load-mode))
  :init
  (setq auto-compile-display-buffer nil
        ;; lets spaceline manage the mode-line
        auto-compile-use-mode-line nil
        auto-compile-mode-line-counter t)
  :config

  )

;; ----------------------------------------------------------------------------
;; Load .el if newer than corresponding .elc
;; ----------------------------------------------------------------------------
(setq load-prefer-newer t)

(use-package immortal-scratch
  :ensure t
  :hook (after-init . immortal-scratch-mode))

;;; Support byte-compilation in a sub-process, as
;;; required by highlight-cl

(defun sanityinc/byte-compile-file-batch (filename)
  "Byte-compile FILENAME in batch mode, ie. a clean sub-process."
  (interactive "fFile to byte-compile in batch mode: ")
  (let ((emacs (car command-line-args)))
    (compile
     (concat
      emacs " "
      (mapconcat
       'shell-quote-argument
       (list "-Q" "-batch" "-f" "batch-byte-compile" filename)
" ")))))


;; ----------------------------------------------------------------------------
;; Key-binding for emacs-lisp-mode
;; ----------------------------------------------------------------------------
(defhydra hydra-elisp (:color blue :columns 3)
  "elisp"
  ("c" emacs-lisp-byte-compile "byte compile")
  ("b" eval-buffer "eval buffer")
  ("e" pp-eval-expression "eval sexp")
  ("f" eval-defun "eval function")
  ("r" eval-region "eval region")
  ("s" sanityinc/switch-to-ielm "switch to REPL")
  ("g" elisp-slime-nav-find-elisp-thing-at-point "find symbal")
  ("h" elisp-slime-nav-describe-elisp-thing-at-point "find doc")
  ("a" sanityinc/headerise-elisp "add header")
  ("q" nil "quit")

 )

(provide 'init-elisp)

;;; init-elisp.el ends here
