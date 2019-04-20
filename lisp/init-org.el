;;; init-org.el --- Org-mode setting -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "refile.org"))
(setq org-agenda-files '("~/org/"))

;; (define-key global-map "\C-cl" 'org-store-link)
;; (define-key global-map "\C-ca" 'org-agenda)
;; (define-key global-map "\C-cc" 'org-capture)

(defun org-goto-header ()
  "Goes to the beginning of an element's header, so that you can execute speed commands."
  (interactive)
  (when (equal major-mode 'org-mode)
    (if (org-at-heading-p)
        (beginning-of-line)
      (outline-previous-heading))))

;; ------------------------------------------------------------------------
(use-package org			;org-mode
  ;:ensure nil
  ;; :commands (orgtbl-mode)
  :mode (("\\.txt\\'" . org-mode)
	 (".*/[0-9]*$" . org-mode))
  :bind (("C-c o" . hydra-org/body)
	 ("C-c l" . org-store-link)
	 ;; ("C-c L" . org-insert-link-global)
	 ("C-c a" . org-agenda)
	 ;; ("C-c b" . org-goto-header)
	 ("C-c c" . org-capture))
  :init
  (progn)
  :config
  (progn
    (bind-keys :map org-mode-map
	       ("C-c ." . org-time-stamp)
	       ("C-c !" . org-time-stamp-inactive)
	       ("C-s-f" . forward-sentence)
	       ("C-s-b" . backward-sentence))

    (setq
     org-startup-indented t
     ;; Make editing invisible regions smart
     org-catch-invisible-edits 'smart
     ;; allow lists with letters in them.
     org-list-allow-alphabetical t

     org-use-speed-commands t
     ;; org-use-fast-todo-selection t

     ;; record time I finished a task when I change it to DONE
     org-log-done 'time
     org-src-fontify-natively t
     org-src-tab-acts-natively t
     org-src-window-setup 'current-window

     ;; export
     org-export-with-todo-keywords nil
     org-export-backends '(ascii html icalendar latex md koma-letter)
     org-export-with-tags nil
     org-export-with-toc nil
     org-export-with-section-numbers nil

     )

    ;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
    ;; (add-to-list 'auto-mode-alist '(".*/[0-9]*$" . org-mode)) ;; Journal entries

    ;; speed commands
    (setq org-todo-keywords
	  '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)" "CANCELLED(c@/!)")
	    (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))

    (add-to-list 'org-speed-commands-user (cons "P" 'org-set-property))
    (add-to-list 'org-speed-commands-user (cons "d" 'org-deadline))
    ;; Set a tag
    (add-to-list 'org-speed-commands-user (cons "g" 'org-set-tags))
    ;; Mark a subtree
    (add-to-list 'org-speed-commands-user (cons "m" 'org-mark-subtree))
    ;; Widen
    (add-to-list 'org-speed-commands-user (cons "S" 'widen))
    ;; kill a subtree
    (add-to-list 'org-speed-commands-user (cons "k" (lambda ()
						      (org-mark-subtree)
						      (kill-region
						       (region-beginning)
						       (region-end)))))
    ;; Jump to headline
    (add-to-list 'org-speed-commands-user
		 (cons "q" (lambda ()
			     (avy-with avy-goto-line
			       (avy--generic-jump "^\\*+" nil avy-style)))))

    ;; Teleport heading
    (add-to-list 'org-speed-commands-user (cons "T" 'avy-org-refile-as-child))

    ))

;; ------------------------------------------------------------------------
(use-package org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  ;; (setq org-bullets-bullet-list '("☉" "☆" "○" "□" "◇" "◦"))
  (setq org-bullets-bullet-list '("■" "★" "●"  "◆" "•" "∙"))
  )

;; ----------------------------------------------------------------

;; (use-package ox-clip
;;   ;; :ensure nil
;;   :bind (:map org-mode-map
;; 	      ("C-c k" . ox-clip-formatted-copy))
;;   )

;; ------------------------------------------------------------------------
(use-package org-capture
  :ensure nil
  :defer t
  :init
  (progn
    (setq org-capture-templates
	  (quote (("t" "todo" entry
		   (file "") ;; => `org-default-notes-file'
		   "** TODO %?\n%U\n%a\n" :clock-in t :empty-lines 1)
		  ("r" "respond" entry
		   (file "")
		   "** NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :immediate-finish t :empty-lines 1)
		  ("n" "note" entry
		   (file "")
		   "** NOTE %?\n%U\n%a\n" :clock-in t :empty-lines 1)
		  ;; ("n" "note" entry (file "")
		  ;;  "* %?\n\n  %i\n\n  see: %a" :clock-in t :clock-resume t :empty-lines 1)
		  ("j" "journal" entry
		   (file+olp+datetree "~/org/diary.org")
		   "** %?\n%U\n" :clock-in t :empty-lines 1)
		  ("w" "org-protocol"
		   entry (file "")
		   "** TODO Review %c\n%U\n" :immediate-finish t :empty-lines 1)
		  ("m" "meeting" entry
		   (file "")
		   "** MEETING with %? :MEETING:\n%U" :clock-in t :empty-lines 1)
		  ("p" "phone call" entry
		   (file "")
		   "** PHONE %? :PHONE:\n%U" :clock-in t :empty-lines 1)
		  ("h" "habit" entry
		   (file "")
		   "** NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
    )
  :config
  (progn

    ; Use full outline paths for refile targets - we file directly with IDO
    (setq org-refile-use-outline-path t)
    ; Targets complete directly with IDO
    (setq org-outline-path-complete-in-steps nil)
    ; Allow refile to create parent tasks with confirmation
    (setq org-refile-allow-creating-parent-nodes (quote confirm))
    ; Use the current window for indirect buffer display
    (setq org-indirect-buffer-display 'current-window)


    )
  )

;; ------------------------------------------------------------------------
(use-package org-agenda
  :ensure nil
  :defer t
  :config
  (progn
    ;; I don't want to see things that are done. turn that off here.
    ;; http://orgmode.org/manual/Global-TODO-list.html#Global-TODO-list
    (setq org-agenda-skip-scheduled-if-done t
	  org-agenda-skip-deadline-if-done t
	  org-agenda-skip-timestamp-if-done t
	  org-agenda-todo-ignore-scheduled t
	  org-agenda-todo-ignore-deadlines t
	  org-agenda-todo-ignore-timestamp t
	  org-agenda-todo-ignore-with-date t
	  org-agenda-start-on-weekday nil ;; start on current day
	  org-upcoming-deadline '(:foreground "blue" :weight bold)
	  ;; use timestamps in date-trees. for the journal
	  org-datetree-add-timestamp 'active)

    (add-to-list
     'org-agenda-custom-commands
     '("w" "Weekly Review"
       ( ;; deadlines
	(tags-todo "+DEADLINE<=\"<today>\""
		   ((org-agenda-overriding-header "Late Deadlines")))
	;; scheduled  past due
	(tags-todo "+SCHEDULED<=\"<today>\""
		   ((org-agenda-overriding-header "Late Scheduled")))

	;; now the agenda
	(agenda ""
		((org-agenda-overriding-header "weekly agenda")
		 (org-agenda-ndays 7)
		 (org-agenda-tags-todo-honor-ignore-options t)
		 (org-agenda-todo-ignore-scheduled nil)
		 (org-agenda-todo-ignore-deadlines nil)
		 (org-deadline-warning-days 0)))
	;; and last a global todo list
	(todo "TODO"))))

    ))

;; ------------------------------------------------------------------------
(use-package org-crypt
  :ensure nil
  :defer 1
  :config
  (progn
    ;; encrypt before save file
    (org-crypt-use-before-save-magic)
    ;; set tag of encrypt part
    (setq org-crypt-tag-matcher "crypt")
    ;; avoid recursive encrypt sub-items
    (setq org-tags-exclude-from-inheritance (quote ("crypt")))
    ;; GPG key to use for encryptioV
    ;; Either the Key ID or set to nil to use symmetric encryption.
    (setq org-crypt-key nil)

    ))

;; ------------------------------------------------------------------------
(use-package org-tempo
  :ensure nil
  :defer t)

;; ------------------------------------------------------------------------
(defhydra hydra-org (:color blue :columns 3)
  "
      org-mode:
  "

  ("a" hydra-archive/body "archive")
  ("c" hydra-clock/body "clock")
  ("d" org-decrypt-entry "decrypt-entry")
  ("D" org-decrypt-entries "decrypt-entries")
  ("e" org-encrypt-entry "encrypt-entry")
  ("E" org-encrypt-entries "encrypt-entries")

  ("m" org-emphasize "emphasize")
  ;; ("x" hydra-ox/body "export")
  ("x" org-export-dispatch "export")
  ("k" org-cut-subtree "cut-subtree")
  ("l" org-insert-link-global "insert-link")
  ("L" org-toggle-link-display "display-link")
  ("g" org-set-tags "set-tags")
  ("o" org-open-at-point-global "open-link")
  ("r" org-refile "refile")
  ("s" org-store-link "store-link")
  ("t" org-show-todo-tree "todo-tree")
  ("q" nil "quit" :color red))


(defhydra hydra-clock (:color blue :hint nil)
  "
  ^
  ^Clock^             ^Do^
  ^─────^─────────────^──^─────────
  _q_ quit            _c_ cancel
  ^^                  _d_ display
  ^^                  _e_ effort
  ^^                  _i_ in
  ^^                  _j_ jump
  ^^                  _o_ out
  ^^                  _r_ report
  ^^                  ^^
  "
  ("q" nil)
  ("c" org-clock-cancel)
  ("d" org-clock-display)
  ("e" org-clock-modify-effort-estimate)
  ("i" org-clock-in)
  ("j" org-clock-goto)
  ("o" org-clock-out)
  ("r" org-clock-report))

(defhydra hydra-archive (:color blue :columns 1)
  ("a" org-archive-subtree "archive subtree")
  ("n" org-next-visible-heading "next visible heading")
  ("t" org-next-visible-heading "next visible heading")
  ("p" org-previous-visible-heading "previous visible heading")
  ("s" org-previous-visible-heading "previous visible heading")
  )


;; ------------------------------------------------------------------------
;; Org mode export widget implemented in Hydra
;; kbd "C-c C-," -> hydra-ox/body
(use-package hydra-ox
  :ensure nil
  :defer t)


(provide 'init-org)
;;; init-org.el ends here
