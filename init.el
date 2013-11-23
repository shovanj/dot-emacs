(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; ido-mode settings
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 10
      kept-old-versions 10
      version-control t)

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

;; hide menu bar mode
(menu-bar-mode -1)

;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

; will reduce the number of messages that appear in the “*Messages*” window to 512.
(setq message-log-max 512)

; add /usr/local/bin to path
(add-to-list 'exec-path "/usr/local/bin")

(setq-default show-trailing-whitespace t)


(setq inhibit-startup-message   t)   ; Don't want any startup message
(setq make-backup-files         nil) ; Don't want any backup files
(setq auto-save-list-file-name  nil) ; Don't want any .saves files
(setq auto-save-default         nil) ; Don't want any auto saving

(setq search-highlight           t) ; Highlight search object
(setq query-replace-highlight    t) ; Highlight query object
(setq mouse-sel-retain-highlight t) ; Keep mouse high-lightening

(delete-selection-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(deft-auto-save-interval 5.0)
 '(deft-directory "/Users/sjoshi/Documents/deft/")
 '(deft-extension "org")
 '(deft-text-mode (quote org-mode))
 '(load-dir-recursive t)
 '(load-dirs (quote ("~/.emacs.d/personal" "~/.emacs.d/personal/hooks"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit nil)))))

 (desktop-save-mode 1)

(require 'package)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d")    ; This may not be appeared if you have already added.
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
(ac-config-default)
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)
