;;; init-elpa.el --- settings of hydra -*- lexical-binding: t -*-
;;; commentary:
;;; code:

(use-package hydra
  :defer 0.5
  :init
  (setq hydra-is-helpful t)
  :bind (
	 ;; ("C-c L" . hydra-ledger/body)
         ;; ("C-c c" . hydra-clock/body)
         ;; ("C-c g" . hydra-go-to-file/body)
         ;; ("C-c m" . hydra-magit/body)
         ("C-c w" . hydra-windows/body))


  )



(use-package hydra-example
  :ensure nil
  :defer t
  :init
  ;; (setq hydra-examples-verbatim t)
  :config
  (progn
    ;; the whole menu for `Buffer-menu-mode'
    (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)

    (define-key org-agenda-mode-map "v" 'hydra-org-agenda-view/body)
    )
  )



(defhydra hydra-toggle (:color pink :hint nil)
  "
_a_ abbrev-mode:       %`abbrev-mode
_d_ debug-on-error:    %`debug-on-error
_f_ auto-fill-mode:    %`auto-fill-function
_h_ highlight          %`highlight-nonselected-windows
_t_ truncate-lines:    %`truncate-lines
_w_ whitespace-mode:   %`whitespace-mode
_l_ org link display:  %`org-descriptive-links
"
  ("a" abbrev-mode)
  ("d" toggle-debug-on-error)
  ("f" auto-fill-mode)
  ("h" (setq highlight-nonselected-windows (not highlight-nonselected-windows)))
  ("t" toggle-truncate-lines)
  ("w" whitespace-mode)
  ("l" org-toggle-link-display)
  ("q" nil "quit"))

;(global-set-key (kbd "C-c C-v") 'hydra-toggle/body)

(defhydra hydra-apropos (:color blue
                         :hint nil)
  "
_m_an              _c_ommand
_a_propos          _l_ibrary
_d_ocumentation    _u_ser-option
_v_ariable     valu_e_"
  ("m" man)
  ("a" apropos)
  ("d" apropos-documentation)
  ("v" apropos-variable)
  ("c" apropos-command)
  ("l" apropos-library)
  ("u" apropos-user-option)
  ("e" apropos-value))


(defhydra hydra-error ()
  "goto-error"
  ("h" first-error "first")
  ("j" next-error "next")
  ("k" previous-error "prev")
  ("v" recenter-top-bottom "recenter")
  ("q" nil "quit"))

(defhydra hydra-zoom ()
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("r" (text-scale-set 0) "reset")
  ("0" (text-scale-set 0) :bind nil :exit t)
  ("1" (text-scale-set 0) nil :bind nil :exit t))


(defhydra hydra-windows (:color pink)
  "
  ^
  ^Windows^           ^Window^            ^Zoom^
  ^───────^───────────^──────^────────────^────^──────
  _q_ quit            _b_ balance         _-_ out
  ^^                  _i_ heighten        _+_ in
  ^^                  _j_ narrow          _=_ reset
  ^^                  _k_ lower           ^^
  ^^                  _l_ widen           ^^
  ^^                  _s_ swap            ^^
  ^^                  ^^                  ^^
  "
  ("q" nil)
  ("b" balance-windows)
  ("i" enlarge-window)
  ("j" shrink-window-horizontally)
  ("k" shrink-window)
  ("l" enlarge-window-horizontally)
  ("s" switch-window-then-swap-buffer :color blue)
  ("-" text-scale-decrease)
  ("+" text-scale-increase)
  ("=" (text-scale-increase 0)))

(defun hydra-set-mark ()
  (interactive)
  (if (region-active-p)
      (progn
        (deactivate-mark)
        (hydra-keyboard-quit))
    (call-interactively 'set-mark-command)
    (hydra-region/body)))


(defhydra hydra-region ()
  ("E" forward-sentence)
  ("f" forward-word)
  ("b" backward-word)
  ("w" kill-region :exit t))

;; Group jump-to-files commands.
(defhydra hydra-go-to-file (:color blue)
  "
  ^
  ^Go To^           ^Config^            ^Agenda             ^Other^
  ^─────^───────────^──────^────────────^──────^────────────^─────^────────
  _q_ quit          _ca_ alacritty      _ac_ contacts       _ob_ book
  ^^                _cd_ dunst          _af_ findmycat      _ol_ learning
  ^^                _ce_ emacs          _ao_ organizer      _om_ movies
  ^^                _ci_ i3             _ap_ people         _op_ purchases
  ^^                _cn_ neofetch       _ar_ routine        _ou_ usb
  ^^                _cp_ polybar        _as_ school         ^^
  ^^                _cq_ qutebrowser    ^^                  ^^
  ^^                _cR_ rofi           ^^                  ^^
  ^^                _cr_ ranger         ^^                  ^^
  ^^                _cs_ sway           ^^                  ^^
  ^^                _ct_ tmux           ^^                  ^^
  ^^                ^^                  ^^                  ^^
  "
  ("q" nil)
  ("ac" (find-file "~/.personal/agenda/contacts.org"))
  ("af" (find-file "~/.personal/agenda/findmycat.org"))
  ("ao" (find-file "~/.personal/agenda/organizer.org"))
  ("ap" (find-file "~/.personal/agenda/people.org"))
  ("ar" (find-file "~/.personal/agenda/routine.org"))
  ("as" (find-file "~/.personal/agenda/school.org"))
  ("ca" (find-file (format "%s/alacritty/alacritty.yml" xdg-config)))
  ("cd" (find-file (format "%s/dunst/dunstrc" xdg-config)))
  ("ce" (find-file "~/.emacs.d/config.org"))
  ("ci" (find-file (format "%s/i3/config" xdg-config)))
  ("cn" (find-file (format "%s/neofetch/config.conf" xdg-config)))
  ("cp" (find-file (format "%s/polybar/config" xdg-config)))
  ("cq" (find-file (format "%s/qutebrowser/config.py" xdg-config)))
  ("cR" (find-file (format "%s/rofi/config.rasi" xdg-config)))
  ("cr" (find-file (format "%s/ranger/rc.conf" xdg-config)))
  ("cs" (find-file (format "%s/sway/config" xdg-config)))
  ("ct" (find-file (format "%s/tmux/tmux.conf" xdg-config)))
  ("ob" (find-file "~/.personal/other/books.org"))
  ("ol" (find-file "~/.personal/other/learning.org"))
  ("om" (find-file "~/.personal/other/movies.org"))
  ("op" (find-file "~/.personal/other/purchases.org"))
  ("ou" (find-file "~/.personal/other/usb.org")))


;; -------------------------------------------------------
;; global Key-binding
;; -------------------------------------------------------
(global-set-key (kbd "C-M-o") 'hydra-window/body)
(global-set-key (kbd "C-M-k") 'hydra-pause-resume)
;(global-set-key (kbd "C-M-k") 'ora-kill-current-buffer)
(global-set-key (kbd "C-M-y") 'counsel-hydra-heads)
(global-set-key (kbd "C-M-j") 'counsel-semantic)
(global-set-key (kbd "C-x SPC") 'hydra-rectangle/body)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
;(global-set-key (kbd "C-t") 'counsel-M-x)
;(global-set-key (kbd "C-SPC") 'hydra-set-mark)
;(global-set-key (kbd "C-SPC") 'set-mark-command)
(global-set-key (kbd "C-M-a") 'hydra-toggle/body)
(global-set-key (kbd "<f2>") 'hydra-zoom/body)
(global-set-key (kbd "M-g") 'hydra-error/body)




(provide 'init-hydra)

;;; init-hydra.el ends here
