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


(define-key esc-map "[" 'vc-git-grep)
(define-key esc-map "]" 'magit-status)
