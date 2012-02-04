(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; ido-mode
(require 'ido)
(ido-mode t)
(setq ido-enable-flex-matching t) ; enable fuzzy matching

;; turn on paren match highlighting
(show-paren-mode 1)

;; deleting files goes to OS's trash folder
(setq delete-by-moving-to-trash t)

;; y or n shortcut for confirmations
(fset 'yes-or-no-p 'y-or-n-p)

;; automatically reload files was modified by external program
(global-auto-revert-mode 1)

;; highlight current line
(global-hl-line-mode -1)

;; hide scroll bar mode
(set-scroll-bar-mode nil)
;; hide menu bar mode
(menu-bar-mode -1)

;; background color for modeline
(set-face-background 'modeline "yellow")
;; foreground color for modeline
(set-face-foreground 'modeline "black")
;; background color for highlighted region
(set-face-background 'region "cyan")


;; unique names for buffer with same filename
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; display trailing whitespaces in a line
(setq-default show-trailing-whitespace t)

;; disable C-z
(global-unset-key "\C-z")

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; enable save desktop
;; call desktop-save to save desktop for each project/location
(desktop-save-mode 1)

;; custom shortcuts
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M--") 'split-window-vertically)
(global-set-key (kbd "M-\\") 'split-window-horizontally)
(global-set-key (kbd "M-0") 'delete-window)

(global-set-key (kbd "C-c <up>") 'windmove-up)  ; Ctl + up arrow
(global-set-key (kbd "C-c <down>") 'windmove-down)  ; Ctl + down arrow
(global-set-key (kbd "C-c <right>") 'windmove-right)  ; Ctl + right arrow
(global-set-key (kbd "C-c <left>") 'windmove-left)  ; Ctl + left arrow

