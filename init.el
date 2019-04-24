;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; commentary:

;; This file bootstraps the configuration, which is divided into
;; a number of other files.

;;; code:

;; Produce backtraces when errors occur
;; (setq debug-on-error t)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "site-lisp" user-emacs-directory))

;; Measure startup time
(require 'init-benchmarking)
;; (setq emacs-load-start-time (current-time))

(setq *win64* (eq system-type 'windows-nt))
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs25* (>= emacs-major-version 25))
(setq *emacs26* (>= emacs-major-version 26))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
     (init-gc-cons-threshold (* 128 1024 1024)))
 (setq gc-cons-threshold init-gc-cons-threshold)
 (add-hook 'emacs-startup-hook
           (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Emacs 25 does gc too frequently
(when *emacs25*
  ;; (setq garbage-collection-messages t) ; for debug
  (setq gc-cons-threshold (* 64 1024 1024) )
  (setq gc-cons-percentage 0.5)
  (run-with-idle-timer 5 t #'garbage-collect))

;; Normally file-name-handler-alist is set to
;; (("\\`/[^/]*\\'" . tramp-completion-file-name-handler)
;; ("\\`/[^/|:][^/|]*:" . tramp-file-name-handler)
;; ("\\`/:" . file-name-non-special))
;; Which means on every .el and .elc file loaded during start up,
;; it has to runs those regexps against the filename.

(let ((file-name-handler-alist nil))

  (require 'init-preference)
  (require 'init-utils)
  ;; (require 'init-site-lisp)
  (require 'init-elpa)

  (require 'init-hydra)
  (require 'init-exec-path)
  (require 'init-frame-hooks)
  (require 'init-recentf)
  ;; (require 'init-autocomplete)
  (require 'init-company)
  ;; (require 'init-dumb-jump)
  (require 'init-dired)

  ;; (require 'init-calendar)
  (require 'init-ibuffer)
  (require 'init-ace-window)
  (require 'init-ivy)
  (require 'init-hippie-expand)
  (require 'init-desktop)

  (require 'init-lisp)
  (require 'init-elisp)
  (require 'init-common-lisp)

  ;; (require 'init-evil)
  (require 'init-python-anaconda)
  (require 'init-flycheck)
  (require 'init-org)
  (require 'init-magit)
  (require 'init-projectile)
  (require 'init-paredit)
  (require 'init-theme)
  (require 'init-whichkey)
  ;; (require 'init-workgroups2)
  (require 'init-undotree)
  ;; (require 'init-spelling)
  ;; (require 'init-epa)

  (require 'init-misc)

  ;; Allow access from emacsclient
  ;; (require 'init-server)

  ;; Variables configured via the interactive 'customize' interface
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; Allow users to provide an optional "init-local"
  ;; containing personal settings
  ;; (require 'init-local nil t)
  ;; (when (file-exists-p local-file)
  ;;   (load local-file))

  )

;; (server-start)

;; (message "Start up time %.2fs" (float-time (time-subtract (current-time) emacs-load-start-time)))

;;; init.el ends here
