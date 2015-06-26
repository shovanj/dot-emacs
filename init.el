(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

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
(menu-bar-mode 1)

;; more readable :)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))



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
 '(deft-directory (getenv "DEFT_DIR"))
 '(deft-extension "org")
 '(deft-text-mode (quote org-mode))
 '(gnus-directory (getenv "GNUS_NEWS_DIR"))
 '(gnus-home-directory (getenv "GNUS_DIR"))
 '(load-dir-recursive t)
 '(org-agenda-files (quote (concat (getenv "DEFT_DIR") "todos.org")))
 '(org-export-latex-listings t)
 '(tabbar-separator (quote (1.0)))
 '(tabbar-use-images nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-item-highlight ((t (:inherit nil))))
 '(tabbar-button ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "highlightColor" :foreground "secondaryLabelColor" :height 1.2))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "Purple" :box (:line-width 1 :color "white" :style pressed-button)))))
 '(tabbar-unselected ((t (:inherit tabbar-default)))))

;;  (desktop-save-mode 1)

;; (require 'package)
;; (add-to-list 'package-archives
;;              '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; (package-initialize)

(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/dict")

;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-1.4/dict")
;; (ac-config-default)
;; ; Use dictionaries by default
;; (setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
;; (global-auto-complete-mode t)
;; ; Start auto-completion after 2 characters of a word
;; (setq ac-auto-start 2)
;; ; case sensitivity is important when finding matches
;; (setq ac-ignore-case nil)

(add-to-list 'load-path "~/.emacs.d/packages/deft")
(require 'deft)


(add-to-list 'load-path "~/.emacs.d/packages/git-modes")
(add-to-list 'load-path "~/.emacs.d/packages/magit")
(require 'magit)

(add-to-list 'load-path "~/.emacs.d/packages/htmlize")
(require 'htmlize)


(add-to-list 'load-path "~/.emacs.d/packages/swift-mode")
(require 'swift-mode)

(add-to-list 'load-path "~/.emacs.d/packages/git-timemachine")
(require 'git-timemachine)

(add-to-list 'load-path "~/.emacs.d/packages/popwin-el")
(require 'popwin)
(popwin-mode 1)

(add-to-list 'load-path "~/.emacs.d/packages/emacs-elixir/")
(require 'elixir-mode)
;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
(add-to-list 'load-path "~/.emacs.d/packages/markdown-mode")
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(add-to-list 'load-path "~/.emacs.d/packages/web-mode/")
(require 'web-mode)

(add-to-list 'load-path "~/.emacs.d/packages/yaml-mode/")
(require 'yaml-mode)

(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)

(add-to-list 'load-path "~/.emacs.d/packages/avy")
(require 'avy)

(add-to-list 'load-path "~/.emacs.d/packages/ace-window")
(require 'ace-window)

(add-to-list 'load-path "~/.emacs.d/packages/inf-ruby")
(autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
(add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)

(load-file "~/.emacs.d/personal/key_bindings.el")
(load-file "~/.emacs.d/personal/detect_mode.el")
(load-file "~/.emacs.d/personal/custom_functions.el")

(load-file "~/.emacs.d/personal/hooks/ruby.el")
(load-file "~/.emacs.d/personal/hooks/javascript.el")
(load-file "~/.emacs.d/personal/hooks/yaml.el")
(load-file "~/.emacs.d/personal/hooks/yas.el")



(autoload 'zap-up-to-char "misc"
    "Kill up to, but not including ARGth occurrence of CHAR." t)


(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
;; (global-set-key (kbd "M-/") 'hippie-expand)

(setq require-final-newline t
      visible-bell t
      ediff-window-setup-function 'ediff-setup-windows-plain)


(add-to-list 'load-path "~/.emacs.d/packages/neotree")
(require 'neotree)


(add-to-list 'load-path "~/.emacs.d/packages/tabbar-mode")
(require 'tabbar)
(tabbar-mode)


;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))

(defun ido-disable-line-truncation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-truncation)
(defun ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
(add-hook 'ido-setup-hook 'ido-define-keys)



(defun tabbar-buffer-groups-by-dir ()
        "Put all files in the same directory into the same tab bar"
        (with-current-buffer (current-buffer)
          (let ((dir (expand-file-name default-directory)))
            (cond ;; assign group name until one clause succeeds, so the order is important
             ((eq major-mode 'dired-mode)
              (list "Dired"))
             ((memq major-mode
                    '(help-mode apropos-mode Info-mode Man-mode))
              (list "Help"))
             ((string-match-p "\*.*\*" (buffer-name))
              (list "Misc"))
             (t (list dir))))))

(defun tabbar-switch-grouping-method (&optional arg)
  "Changes grouping method of tabbar to grouping by dir.
With a prefix arg, changes to grouping by major mode."
  (interactive "P")
  (ignore-errors
    (if arg
      (setq tabbar-buffer-groups-function 'tabbar-buffer-groups) ;; the default setting
        (setq tabbar-buffer-groups-function 'tabbar-buffer-groups-by-dir))))
