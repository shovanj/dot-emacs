(when window-system

  )

;; etags-select
(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-?") 'etags-select-find-tag)

;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; [Ctrl]-[L]
(global-set-key "\C-l" 'goto-line)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

(global-set-key (kbd "C-c d") 'desktop-change-dir)

;; disable C-z
(global-unset-key "\C-z")

;; (global-set-key (kbd "TAB") 'smart-tab)


;; (define-key esc-map "[" 'vc-git-grep)
;; (define-key esc-map "[" 'ack-and-a-half)
(define-key esc-map "]" 'magit-status)

(when (require 'deft nil 'noerror)
  (global-set-key [f9] 'deft))

(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y")

(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "C-x j") 'ace-window)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defvar aw-dispatch-alist
'((?x aw-delete-window " Ace - Delete Window")
    (?m aw-swap-window " Ace - Swap Window")
    (?n aw-flip-window)
    (?v aw-split-window-vert " Ace - Split Vert Window")
    (?b aw-split-window-horz " Ace - Split Horz Window")
    (?i delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows))
"List of actions for `aw-dispatch-default'.")


(global-set-key [f9] 'deft)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)


(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)




(global-set-key (kbd "S-s-<right>") 'tabbar-forward-tab)
(global-set-key (kbd "S-s-<left>") 'tabbar-backward-tab)

(global-set-key (kbd "s-<right>") 'tabbar-forward-group)
(global-set-key (kbd "s-<left>") 'tabbar-backward-group)


(global-set-key (kbd "s-o") 'ido-find-file)

;; InjtelliJ
(global-set-key (kbd "s-1") 'neotree-toggle)
(global-set-key (kbd "s-e") 'ibuffer)
