(setq backup-directory-alist `(("." . "~/.emacs-backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 20
      kept-old-versions 20
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

;; hide menu bar mode
(menu-bar-mode -1)

;; background color for highlighted region
(set-face-background 'region "cyan")


;; unique names for buffer with same filename
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers

;; display trailing whitespaces in a line
;; (setq-default show-trailing-whitespace t)
(setq-default show-trailing-whitespace nil)


;; use spaces instead of tabs
(setq-default indent-tabs-mode nil)

;; enable save desktop
;; call desktop-save to save desktop for each project/location
(desktop-save-mode 1)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "green3"))) t)
 '(diff-removed ((t (:foreground "red"))) t)
 '(erb-exec-face ((t nil)))
 '(erb-face ((t nil)))
 '(erb-out-delim-face ((t (:background "LightPink1" :foreground "black"))))
 '(erb-out-face ((t nil)))
 '(highlight-80+ ((t (:background "gray84"))) t)
 '(magit-diff-add ((t (:foreground "green"))) t)
 '(magit-diff-del ((t (:foreground "violet red"))) t)
 '(magit-item-highlight ((t nil)) t)
 '(mode-line ((t (:background "grey75" :foreground "black" :box nil :family "Inconsolata")))))
 ;; '(mode-line ((t (:background "#FCF6E3" :foreground "white" :box (:line-width -1 :color "#FCF6E3") :height 132 :family "Inconsolata")))))

(set-face-attribute 'default nil
                    :family "Inconsolata" :height 135 :weight 'normal)

;; custom methods
;; insert date
(defun sj-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d")))

;; elpa repositories
;; Sun 2013-04-28 removing elpa references
;; (setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
;;                          ("gnu" . "http://elpa.gnu.org/packages/")))

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-prompt-for-directory t)
 '(auto-save-default t)
 '(auto-save-interval 20)
 '(auto-save-timeout 10)
 '(custom-safe-themes (quote ("fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(highlight-80+-columns 100)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ruby . t))))
 '(org-src-fontify-natively t)
 '(projectile-show-paths-function (quote projectile-hashify-with-relative-paths))
 '(show-paren-mode t)
 '(speedbar-default-position (quote left))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t))

;; ruby mode
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))

;; rhtml stuff
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)




;; outline minor mode
;; You may also want to bind hide-body, hide-subtree, show-substree,
;; show-all, show-children, ... to some keys easy folding and unfolding
(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|class\\|module\\)")))




(add-hook 'after-init-hook
          '(lambda ()
             (yas/load-directory "~/.emacs.d/snippets")))


(when window-system
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1))



; Moving cursor down at bottom scrolls only a single line, not half page
(setq scroll-step 1)
(setq scroll-conservatively 5)

; Don't want any auto saving
(setq auto-save-default         nil)

; Highlight search object
(setq query-replace-highlight    t)
; Highlight query object
(setq search-highlight           t)
; Keep mouse high-lightening
(setq mouse-sel-retain-highlight t)


; will reduce the number of messages that appear in the “*Messages*” window to 512.
(setq message-log-max 512)

; remove all source control hooks:
; (setq vc-handled-backends ())

; switch to current buffer for magit status
(setq magit-status-buffer-switch-function 'switch-to-buffer)

; add /usr/local/bin to path
(add-to-list 'exec-path "/usr/local/bin")

;; Setting rbenv path
(setenv "PATH" (concat (getenv "HOME") "/.rbenv/shims:" (getenv "HOME") "/.rbenv/bin:" (expand-file-name "/usr/local/bin:") (getenv "PATH")))
(setq exec-path (cons (concat (getenv "HOME") "/.rbenv/shims") (cons (concat (getenv "HOME") "/.rbenv/bin") exec-path)))

(add-to-list 'load-path "~/.emacs.d/vendor/expand-region")
(require 'expand-region)



(add-hook 'org-load-hook
          (lambda ()
            (local-set-key (kbd "C-c a") 'org-agenda)
            )
          )

(setq js-indent-level 4)
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))



(setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

(defun coffee-custom ()
  "coffee-mode-hook"

  ;; CoffeeScript uses two spaces.
  (make-local-variable 'tab-width)
  (set 'tab-width 2)

  ;; If you don't want your compiled files to be wrapped
  ;; (setq coffee-args-compile '("-c" "--bare"))

  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

  ;; Riding edge.
  ;; (setq coffee-command "~/dev/coffee")

  ;; Compile '.coffee' files on every save
  ;; (and (file-exists-p (buffer-file-name))
  ;;      (file-exists-p (coffee-compiled-file-name))
  ;;      (coffee-cos-mode t))
  )

(add-hook 'coffee-mode-hook 'coffee-custom)



(add-to-list 'magic-mode-alist '("<!DOCTYPE html .+DTD XHTML .+>" . nxml-mode))




(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete//ac-dict")
(ac-config-default)


(setq speedbar-use-images nil)

(make-face 'speedbar-face)

;; (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))




;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(require 'autopair)

(require 'yaml-mode)


(when system-type 'darwin
      (load-theme 'tsdh-dark t)
      ;; (set-face-font 'speedbar-face "Monaco-12")
      ;; background color for modeline
      ;; (set-face-background 'mode-line "gray27")
      ;; foreground color for modeline
      ;; (set-face-foreground 'mode-line "white")
 
)
;; (set-face-font 'mode-line "Monaco-12")



(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)



(autopair-global-mode t)

(add-to-list 'load-path "~/.emacs.d/vendor/emacs-nav-49")
(require 'nav)
(nav-disable-overeager-window-splitting)




(add-hook 'coffee-mode-hook 'auto-complete-mode)



;; (add-to-list 'load-path "~/.emacs.d/vendor/ruby-mode")

 (require 'ob-tangle) 




(setq ring-bell-function 'ignore)


;; (setq projectile-enable-caching nil)
;; (setq projectile-require-project-root nil)

(add-to-list 'load-path "~/.emacs.d/vendor/")
(require 'projectile)

(add-hook 'ruby-mode-hook 'projectile-mode)
(add-hook 'rhtml-mode-hook 'projectile-mode)
(add-hook 'yaml-mode-hook 'projectile-mode)
(add-hook 'coffee-mode-hook 'projectile-mode)
(add-hook 'js-mode-hook 'projectile-mode)

(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))


;; to change fonts 
;; M-x list-faces-display

(load "~/.emacs.d/ibuffer")
(load "~/.emacs.d/detect_mode")
(load "~/.emacs.d/key_bindings")

(global-linum-mode t)
(require 'git-gutter-fringe)
(global-git-gutter-mode t)
