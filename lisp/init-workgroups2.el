;;; init-workgroup2.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; commentary:
;;; code:


;; https://workgroups2.readthedocs.io/en/latest/index.html

(use-package workgroups2
  :defer 2
  :diminish (workgroups-mode . "WG")
  :bind (("C-c z" . hydra-workgroup2/body)
         )
  :init
  ;;(setq wg-session-load-on-start t)    ; default: (not (daemonp))

  ;; <prefix> ? to list all the commands and their bindings
  ;; Change prefix key (before activating WG)
  ;; (setq wg-prefix-key (kbd "C-c z"))
  ;; (which-key-add-key-based-replacements "C-c z" "workgroups-command-map")

  :config
  (setq
   wg-session-file (expand-file-name "workgroups" user-emacs-directory)
   wg-open-this-wg nil
   wg-use-default-session-file nil
   wg-load-last-workgroup nil    ; don't open last workgroup automatically in wg-open-session


   wg-mode-line-decor-left-brace "["
   wg-mode-line-decor-right-brace "]"
   wg-mode-line-only-name t       ; show only current WG name
   wg-display-nowg nil              ; if no workgroups - display nothing
   wg-mode-line-use-faces t       ; colorize mode line
   wg-use-faces t                 ; colorize messages

   wg-flag-modified nil
   wg-emacs-exit-save-behavior           nil ; Options: 'save 'ask nil
   wg-workgroups-mode-exit-save-behavior nil ; Options: 'save 'ask nil
   wg-mess-with-buffer-list nil

   )

  (workgroups-mode 1)


  (defun my-wg-switch-workgroup ()
    (interactive)
    (let* ((group-names (mapcar (lambda (group)
                                  ;; re-shape list for the ivy-read
                                  (cons (wg-workgroup-name group) group))
                                (wg-session-workgroup-list (read (f-read-text (file-truename wg-session-file)))))))
      (ivy-read "work groups"
                group-names
                :action (lambda (e)
                          (wg-find-session-file wg-default-session-file)
                          ;; ivy8 & ivy9
                          (if (stringp (car e)) (setq e (cdr e)))
                          (wg-switch-to-workgroup e)))))

  ;; (eval-after-load 'workgroups2
  ;;   '(progn
  ;;      ;; make sure wg-create-workgroup always success
  ;;      (defadvice wg-create-workgroup (around wg-create-workgroup-hack activate)
  ;;        (unless (file-exists-p (wg-get-session-file))
  ;;          (wg-reset t)
  ;;          (wg-save-session t))

  ;;        (unless wg-current-session
  ;;          ;; code extracted from `wg-open-session'.
  ;;          ;; open session but do NOT load any workgroup.
  ;;          (let ((session (read (f-read-text (file-truename wg-session-file)))))
  ;;            (setf (wg-session-file-name session) wg-session-file)
  ;;            (wg-reset-internal (wg-unpickel-session-parameters session))))
  ;;         ad-do-it
  ;;        ;; save the session file in real time
  ;;        (wg-save-session t))

  ;;    (defadvice wg-reset (after wg-reset-hack activate)
  ;;      (wg-save-session t))

  ;;    ;; I'm fine to to override the original workgroup
  ;;    (defadvice wg-unique-workgroup-name-p (around wg-unique-workgroup-name-p-hack activate)
  ;;      (setq ad-return-value t))))
  )


(defhydra hydra-workgroup2 (:color blue :columns 3)
  "
       Workgroup2-mode:
  "
  ("!" wg-reset "reset workgroups")
  ("?" wg-help "help")
  ("a" wg-associate-buffer-with-workgroup "associate buffer")
  ("A" wg-rename-workgroup "rename workgroup")
  ("c" wg-create-workgroup "create workgroup")
  ("C" wg-clone-workgroup "clone workgroup")
  ("f" wg-find-session-file "find sesion file")
  ("k" wg-kill-workgroup "kill workgroup")
  ("y" wg-yank-workgroup "yank workgroup")
  ("o" wg-open-session "open session")
  ("v" my-wg-switch-workgroup "switch workgroup")
  ("s" wg-save-session "save workgroup")
  ("S" wg-save-session-as "save-as workgroup")

  ("q" nil "quit" :color red)
  )

(provide 'init-workgroups2)
;;; init-workgroup2.el ends here
