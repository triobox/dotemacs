
(use-package auto-complete
  :commands (auto-complete-mode)
  ;:init
  ;(auto-complete-mode 1)
  :config
  (setq ac-delay 0.4)
  (ac-config-default)
  (define-key ac-complete-mode-map "\C-j" 'newline-and-indent)
  (define-key ac-complete-mode-map [return] nil))

(provide 'init-autocomplete)
