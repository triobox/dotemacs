;;; init-elpa.el --- Settings and helpers for package.el -*- lexical-binding: t -*-
;;; commentary:
;;; code:


;; https://github.com/purcell/emacs.d/blob/master/lisp/init-elpa.el

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))

(setq package-user-dir versioned-package-dir))

;;; Standard package repositories

;; (let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;;                     (not (gnutls-available-p))))
;;        (proto (if no-ssl "http" "https")))
;;   (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;   ;; Official MELPA Mirror, in case necessary.
;;   ;;(add-to-list 'package-archives (cons "melpa-mirror" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/")) t)
;;   (if (< emacs-major-version 24)
;;       ;; For important compatibility libraries like cl-lib
;;       (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))
;;     (unless no-ssl
;;       ;; Force SSL for GNU ELPA
;; (setcdr (assoc "gnu" package-archives) "https://elpa.gnu.org/packages/"))))

;; Initialize package management
(eval-when-compile
  (require 'package)

  (setq package-archives
	'(		      ;("localelpa" . "~/.emacs.d/localelpa/")
          ;; {{ backup repositories
	  ;; ("gnu"   . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
          ;; ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
          ;; ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa-stable/")
          ;; }}

          ;; {{ backup repositories
          ;; ("melpa" . "http://59.111.0.251/elpa/melpa/")
          ("gnu" . "http://mirrors.163.com/elpa/gnu/")
          ("melpa" . "http://mirrors.163.com/elpa/melpa/")
          ("melpa-stable" . "http://mirrors.163.com/elpa/melpa-stable/")
          ;; }}

          ;; uncomment below line if you need use GNU ELPA
          ;; ("gnu" . "https://elpa.gnu.org/packages/")
          ;; ("melpa" . "https://melpa.org/packages/")
          ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
          ))
  (setq load-prefer-newer t)
  (setq package--init-file-ensured t)
  (setq package-enable-at-startup nil)


  (package-initialize)

  (require 'auto-compile)
  (auto-compile-on-load-mode)
  (auto-compile-on-save-mode)

  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

  (require 'use-package)
  (setq use-package-compute-statistics t)
  (setq use-package-always-ensure t)

  )

(use-package diminish)
(use-package bind-key)

(use-package use-package-chords
  :defer t
  :config (key-chord-mode 1))


(provide 'init-elpa)
;;; init-elpa.el ends here
