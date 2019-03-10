;;; init-python-anaconda.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package python
  :defer t
  :mode
  (("\\.py\\'" . python-mode)
   ("\\.pyx\\'" . python-mode)
   ("\\.wsgi$" . python-mode))
  :init
  (progn

    (setq python-shell-interpreter "ipython"
	  python-shell-interpreter-args "--simple-prompt -i --pprint")

    (defun python-default ()
      (setq mode-name "Python"
            tab-width 4
            fill-column 79)
      (setq-local comment-inline-offset 2)
      (local-set-key (kbd "C-j") 'newline-and-indent))

    (defun inferior-python-setup-hook ()
      (setq indent-tabs-mode t))


    (add-hook 'inferior-python-mode-hook #'inferior-python-setup-hook)
    (add-hook 'python-mode-hook #'python-default)
  )

  :bind (:map python-mode-map
	      ("C-c C-p" . python-start-or-switch-repl)
	      ("C-c C-r" . python-shell-send-region-switch)
	      ("C-c C-d" . python-shell-send-defun-switch)
	      ("C-c C-b" . python-shell-send-buffer-switch)
	      ("C-c C-e" . python-execute-file-focus)
	      ("C-c h" . hydra-python/body))

  :config
  (progn
    (defun python-shell-send-buffer-switch ()
      "Send buffer content to shell and switch to it in insert mode."
      (interactive)
      (python-shell-send-buffer)
      (python-shell-switch-to-shell)
      (evil-insert-state))

    (defun python-shell-send-defun-switch ()
      "Send function content to shell and switch to it in insert mode."
      (interactive)
      (python-shell-send-defun nil)
      (python-shell-switch-to-shell)
      (evil-insert-state))

    (defun python-shell-send-region-switch (start end)
      "Send region content to shell and switch to it in insert mode."
      (interactive "r")
      (python-shell-send-region start end)
      (python-shell-switch-to-shell)
      (evil-insert-state))

    (defun python-start-or-switch-repl ()
      "Start and/or switch to the REPL."
      (interactive)
      (let ((shell-process
	     (or (python-shell-get-process)
		 ;; `run-python' has different return values and different
		 ;; errors in different emacs versions. In 24.4, it throws an
		 ;; error when the process didn't start, but in 25.1 it
		 ;; doesn't throw an error, so we demote errors here and
		 ;; check the process later
		 (with-demoted-errors "Error: %S"
		   ;; in Emacs 24.5 and 24.4, `run-python' doesn't return the
		   ;; shell process
		   (call-interactively #'run-python)
		   (python-shell-get-process)))))
	(unless shell-process
	  (error "Failed to start python shell properly"))
	(pop-to-buffer (process-buffer shell-process))
	(evil-insert-state)))

    (defun python-execute-file (arg)
      "Execute a python script in a shell."
      (interactive "P")
      ;; set compile command to buffer-file-name
      ;; universal argument put compile buffer in comint mode
      (let ((universal-argument t)
	    (compile-command (format "python %s" (file-name-nondirectory
						  buffer-file-name))))
	(if arg
	    (call-interactively 'compile)
	  (compile compile-command t)
	  (with-current-buffer (get-buffer "*compilation*")
	    (inferior-python-mode)))))

    (defun python-execute-file-focus (arg)
      "Execute a python script in a shell and switch to the shell buffer in `insert state'."
      (interactive "P")
      (spacemacs/python-execute-file arg)
      (switch-to-buffer-other-window "*compilation*")
      (end-of-buffer)
      (evil-insert-state))
   )
 )


(use-package pip-requirements :defer t)

(use-package anaconda-mode
  :defer t
  :after (python)
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  ;; :config
  )

(use-package company-anaconda
    :defer t
    :after (company python)
    ;; :init
    ;; (push 'company-anaconda company-backends)
    )

(use-package conda
    :defer t
    :init
    (custom-set-variables '(conda-anaconda-home "~/miniconda3"))
    :config
    (conda-env-initialize-interactive-shells)
    ;; (conda-env-initialize-eshell)
    (conda-env-autoactivate-mode t)
    (setq-default mode-line-format
                  (cons '(:exec conda-env-current-name) mode-line-format)))

(defhydra hydra-python (:hint nil :color teal)
 "
^Send^             ^find^            ^Actions^
^----^             ^----^            ^-------^
_ff_: function     _dd_: defin       _s_: run-python
_fs_: function->   _d4_: defin->     _*_: jump-back
_rr_: region       _aa_: assign      _y_: yapf
_rs_: region->     _a4_: assign->
_bb_: buffer       _ee_: refer
_bs_: buffer->     _e4_: refer->
"
  ;; shell send
  ("rr" python-shell-send-region)
  ("rs" python-shell-send-region-switch)
  ("ff" python-shell-send-defun)
  ("fs" python-shell-send-defun-switch)
  ("bb" python-shell-send-buffer)
  ("bs" python-shell-send-buffer-switch)
  ;; code nav
  ("dd" anaconda-mode-find-definitions)
  ("d4" anaconda-mode-find-definitions-other-window)
  ("aa" anaconda-mode-find-assignments)
  ("a4" anaconda-mode-find-assignments-other-window)
  ("ee" anaconda-mode-find-references)
  ("e4" anaconda-mode-find-references-other-window)
  ("*" anaconda-mode-go-back)
  ("?" anaconda-mode-show-doc)
  ("s" python-start-or-switch-repl :color red)
  ;; code editing
  ;; ("<" python-indent-shift-left)
  ;; (">" python-indent-shift-right)
  ;; test
  ;; TODO python test via nose
  ;; actions
  ("y" py-yapf-buffer)
  ("q" nil "quit" :color blue)
)

(provide 'init-python-anaconda)

;;; init-python-anaconda.el ends here
