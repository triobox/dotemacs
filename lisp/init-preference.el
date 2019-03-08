;;; init-preference.el --- Preference and configure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; minimal UI
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode   -1)
      (tooltip-mode    -1)
      (menu-bar-mode   -1)
      (global-hl-line-mode 1)
      )
  (global-hl-line-mode -1)
  )

(line-number-mode -1) ; display line number in mode line
(column-number-mode -1) ; display colum number in mode line
;; (setq show-paren-delay 0) ; Show matching parens
(show-paren-mode 1) ; highlight delimiters
;; (save-place-mode) ; save cursor position between sessions

;; setup lines to wrap
(global-visual-line-mode 1)
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)
(setq initial-scratch-message "")

(setq inhibit-compacting-font-caches t)


;; editor behavior
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq indent-tabs-mode nil)
(setq truncate-lines t)
(setq highlight-nonselected-windows nil)
(setq kill-buffer-query-functions nil)
(setq kill-ring-max 200)
(defadvice set-window-dedicated-p (around no-dedicated-windows activate))
(remove-hook 'post-self-insert-hook 'blink-paren-post-self-insert-function)
(setq eval-expression-print-length nil)
(setq eval-expression-print-level nil)
(setq x-selection-timeout 10)
;; improves copying from a ssh -X Emacs.
(setq x-selection-timeout 100)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(add-hook 'server-switch-hook 'raise-frame)

;; find files
(setq find-file-suppress-same-file-warnings t)
(setq read-file-name-completion-ignore-case t)
(setq read-buffer-completion-ignore-case t)
(setq vc-follow-symlinks t)

;; minibuffer interaction
(setq enable-recursive-minibuffers t)
(setq minibuffer-message-timeout 1)
(minibuffer-depth-indicate-mode 1)
(setq read-quoted-char-radix 16)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  create-lockfiles nil
)

;; autosave
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;; make emacs adding customised settings to custom.el
(setq custom-file (expand-file-name "emacs-custom" user-emacs-directory))

;; allow users to provide an optional "local.el"
;; containing personal settings
(setq local-file (expand-file-name "local.el" user-emacs-directory))


(prefer-coding-system 'utf-8)
(setq conding-system-for-read 'utf-8)
(setq conding-system-for-write 'utf-8)

;; remove tailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; use fancy lambdas
(global-prettify-symbols-mode t)

;; Font and Frame Size
(add-to-list 'default-frame-alist '(height . 44))
(add-to-list 'default-frame-alist '(width . 80))

;; Setting English Font
(set-face-attribute
 'default nil :font "Monaco 10")

;; Chinese Font
(dolist (charset '(kana han symbol cjk-misc bopomofo))
  (set-fontset-font (frame-parameter nil 'font)
                    charset
                    (if *win64*
                        (font-spec :family "Microsoft YaHei" :size 13)
                      (font-spec :family "WnQuanYi Zen Hei Mono" :size- 13)
                      )
                    ))

;; (setq custom-file (make-temp-file "emacs-custom"))
;; (setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
;; (load custom-file 'noerror)

(provide 'init-preference)
;;; init-preference.el ends here
