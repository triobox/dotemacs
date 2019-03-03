;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; Aspell Setup (recommended):
;; Skipped because it's easy.
;;
;; Hunspell Setup:
;; 1. Install hunspell from http://hunspell.sourceforge.net/
;; 2. Download openoffice dictionary extension from
;; http://extensions.openoffice.org/en/project/english-dictionaries-apache-openoffice
;; 3. That is download `dict-en.oxt'. Rename that to `dict-en.zip' and unzip
;; the contents to a temporary folder.
;; 4. Copy `en_US.dic' and `en_US.aff' files from there to a folder where you
;; save dictionary files; I saved it to `~/usr_local/share/hunspell/'
;; 5. Add that path to shell env variable `DICPATH':
;; setenv DICPATH $MYLOCAL/share/hunspell
;; 6. Restart emacs so that when hunspell is run by ispell/flyspell, that env
;; variable is effective.
;;
;; hunspell will search for a dictionary called `en_US' in the path specified by
;; `$DICPATH'

(cond
 ;; use aspell
 ((executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; If using flyspell with aspell instead of ispell
  ;; Because the “-l” option mean “--lang” in aspell
  ;; and the “-l” option mean “--list” in ispell.
  ;; flyspell-buffer, and flyspell-region are affected
  (setq ispell-list-command "--list"))
 ;; use hunspell
 ((executable-find "hunpell")
  (setq ispell-program-name "hunspell")
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
        '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8))))
 (t (setq ispell-program-name nil)
    (message "You need install either aspell or hunspell for ispell"))
 )

(use-package flyspell
  :ensure nil
  :if ispell-program-name
  :bind (("C-c s" . hydra-flyspell/body))
  :init
  ;; better performance
  (setq flyspell-issue-message-flag nil)
  )


(defhydra hydra-flyspell (:color blue :columns 1)
  "
          Flyspell:
  "
  ("i" ispell-word "check & correct word at point")
  ("r" ispell-region "check & correct the region")
  ("b" ispell-buffer "check & correct the buffer")
  ("k" ispell-kill-ispell "Kill the spell-checker")
  ("f" flyspell-mode "enable / disable Flyspell mode")
  ("p" flyspell-prog-mode "enable / disable Flyspell mode for comments")
  ("q" nil "quit" :color red))

(provide 'init-spelling)
;;; init-spelling.el ends here
