;;; init-undotree.el --- Settings for undo-tree -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package undo-tree
  :defer t
  :diminish (undo-tree-mode . "")
  :bind (("C-x u" . undo-tree-visualize)
	 ("C-x r u" . undo-tree-save-state-to-register)
	 ("C-x r U" . undo-tree-restore-state-from-register)
         ;; ("h" undo-tree-save-history)
         ;; ("l" undo-tree-load-history)
	 :map undo-tree-visualizer-mode-map
	 ("q" . undo-tree-visualizer-quit)
	 ("d" . undo-tree-visualizer-toggle-diff)
	 ("t" . undo-tree-visualizer-toggle-timestamps)
	 ("r" . undo-tree-visualize-redo)
	 ("u" . undo-tree-visualize-undo)
	 )
  :config
  (global-undo-tree-mode)

  ;; compressing undo history
  (defadvice undo-tree-make-history-save-file-name
      (after undo-tree activate)
    (setq ad-return-value (concat ad-return-value ".gz")))

  ;; (defhydra hydra-undotree (:color blue :columns 2)
  ;;   "
  ;; 		    undo-tree :
  ;;   "
  ;;   ("q" undo-tree-visualizer-quit "quit" :color red)
  ;;   ("d" undo-tree-visualizer-toggle-diff "toggle diff")
  ;;   ("t" undo-tree-visualizer-toggle-timestamps "toggle timestamps")
  ;;   ("r" undo-tree-visualize-redo "redo")
  ;;   ("u" undo-tree-visualize-undo "undo")
  ;;   ("h" undo-tree-save-history "save history")
  ;;   ("l" undo-tree-load-history "load history")
  ;;   )
  )

(provide 'init-undotree)

;;; init-undotree.el ends here
