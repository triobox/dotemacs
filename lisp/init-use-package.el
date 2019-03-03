;;; init-elpa.el --- settings of use-package -*- lexical-binding: t -*-
;;; commentary:
;;; code:

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; gathering statistics of package loading
;; run the command M-x use-package-report to see the results
(setq use-package-compute-statistics t)
(setq use-package-always-ensure t)

(use-package auto-compile
  :config (auto-compile-on-load-mode))

(setq load-prefer-newer t)

(use-package diminish)
(use-package bind-key)

(use-package use-package-chords
  :defer t
  :config (key-chord-mode 1))

(use-package async
  :init
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1)
  (async-bytecomp-package-mode 1)
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (async-bytecomp-package-mode 1)
  (dired-async-mode 1))


(provide 'init-use-package)
;;; init-use-package.el ends here
