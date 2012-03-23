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
(global-set-key (kbd "M-o")  'other-window)
(global-set-key (kbd "M--")  'split-window-vertically)
(global-set-key (kbd "M-\\") 'split-window-horizontally)
(global-set-key (kbd "M-0")  'delete-window)

(global-set-key (kbd "C-c <up>")    'windmove-up)    ; Ctl + up arrow
(global-set-key (kbd "C-c <down>")  'windmove-down)  ; Ctl + down arrow
(global-set-key (kbd "C-c <right>") 'windmove-right) ; Ctl + right arrow
(global-set-key (kbd "C-c <left>")  'windmove-left)  ; Ctl + left arrow

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "green"))))
 '(diff-removed ((t (:foreground "red"))))
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-item-highlight ((t nil))))

;; custom methods
;; insert date
(defun sj-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d")))

;; elpa repositories
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; ruby mode
(add-hook 'ruby-mode-hook (lambda () (local-set-key "\r" 'newline-and-indent)))
(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
;; rhtml stuff
(add-to-list 'load-path "~/.emacs.d/vendor/rhtml")
(require 'rhtml-mode)
(add-hook 'rhtml-mode-hook
          (lambda () (rinari-launch)))
(add-to-list 'auto-mode-alist '("\\.rhtml\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.rfpdf\\'" . rhtml-mode))

;; outline minor mode
;; You may also want to bind hide-body, hide-subtree, show-substree,
;; show-all, show-children, ... to some keys easy folding and unfolding
(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|class\\|module\\)")))

;; custom shortcuts for outline minor mode
(global-set-key (kbd "C-c j i") 'hide-body)
(global-set-key (kbd "C-c j m") 'show-all)
(global-set-key (kbd "C-c j k") 'show-entry)
(global-set-key (kbd "C-c j j") 'hide-entry)

;; =====================================================================;;
;; ====================== ibuffer setup ================================;;
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ;; Programming Folders
               ("Controllers"   (filename . "\\/app\\/controllers"))
               ("Models"   (filename . "\\/app\\/models"))
               ("Views"   (filename . "\\/app\\/views"))
               ("Rspec" (filename . "\\/spec\\/"))
               ("Helpers"   (filename . "\\/app\\/helpers"))
               ("Migrations"   (filename . "\\/db\\/"))
               ("Configurations"   (filename . "\\/config\\/"))
               ("Lib"   (filename . "\\/lib\\/"))
               ("View"   (name . "\\.rhtml"))
               ("View"   (name . "\\.erb"))
               ("Yaml"   (mode . yaml-mode))
               ("Haml"   (mode . haml-mode))
               ("Javascript"   (mode . javascript-mode))
               ("Development"
                (filename . "\\*Development\\*$"))
               ("Ruby"   (mode . ruby-mode))
               ;; Org Mode, Calendar
               ("Org" (or
                       (name . "^\\*Calendar\\*$")
                       (name . "^diary$")
                       (mode . org-mode)))
               ("HTML"   (name . "\\.html"))
               ("Emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*GNU Emacs\\*$")
                         (name . "^\\.")
                         (name . "\\.el")
                         (name . "^\\*Messages\\*$")))
               ("Dired" (mode . dired-mode))))))
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-expert t)
;; ====================== ibuffer setup ================================;;
;; =====================================================================;;

(add-hook 'after-init-hook
          '(lambda ()
             (yas/load-directory "~/.emacs.d/snippets")))


(when window-system
  (menu-bar-mode 1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (set-frame-height (selected-frame) 74))

;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
