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
;; (set-face-background 'modeline "yellow")
;; foreground color for modeline
;; (set-face-foreground 'modeline "black")
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

;; disable C-z
(global-unset-key "\C-z")

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
 '(diff-added ((t (:foreground "green3"))))
 '(diff-removed ((t (:foreground "red"))))
 '(erb-exec-face ((t nil)))
 '(erb-face ((t nil)))
 '(erb-out-delim-face ((t (:background "LightPink1" :foreground "black"))))
 '(erb-out-face ((t nil)))
 '(highlight-80+ ((t (:background "gray84"))) t)
 '(magit-diff-add ((t (:foreground "green"))))
 '(magit-diff-del ((t (:foreground "violet red"))))
 '(magit-item-highlight ((t nil))))
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
(setq package-archives '(("ELPA" . "http://tromey.com/elpa/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ack-and-a-half-prompt-for-directory t)
 '(minimap-width-fraction 0.1)
 '(minimap-window-location (quote right))
 '(org-agenda-files (quote ("~/Dropbox/org/projects/home.org" "~/Dropbox/org/todos.org")))
 '(org-babel-load-languages (quote ((emacs-lisp . t) (ruby . t))))
 '(org-src-fontify-natively t)
 '(show-paren-mode t)
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil t)
 '(sr-speedbar-right-side nil)
 '(sr-speedbar-skip-other-window-p t))

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
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . rhtml-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile" . ruby-mode))

;; outline minor mode
;; You may also want to bind hide-body, hide-subtree, show-substree,
;; show-all, show-children, ... to some keys easy folding and unfolding
(add-hook 'ruby-mode-hook
          '(lambda ()
             (outline-minor-mode)
             (setq outline-regexp " *\\(def \\|class\\|module\\)")))


;; =====================================================================;;
;; ====================== ibuffer setup ================================;;

(autoload 'ibuffer "ibuffer" "List buffers." t)

(setq ibuffer-saved-filter-groups
      (quote (("default"
               ;; Programming Folders
               ("Terminals" (mode . term-mode))
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
               ("Cucumber"   (mode . feature-mode))
               ("Coffee"   (mode . coffee-mode))
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

;; =============   global keys ==================

;; etags-select
(global-set-key (kbd "M-.") 'etags-select-find-tag-at-point)
(global-set-key (kbd "M-?") 'etags-select-find-tag)

;; window keys
(global-set-key (kbd "M-o")  'other-window)
(global-set-key (kbd "M--")  'split-window-vertically)
(global-set-key (kbd "M-\\") 'split-window-horizontally)
(global-set-key (kbd "M-0")  'delete-window)

;; window movement keys
(global-set-key (kbd "C-c <up>")    'windmove-up)    ; Ctl + up arrow
(global-set-key (kbd "C-c <down>")  'windmove-down)  ; Ctl + down arrow
(global-set-key (kbd "C-c <right>") 'windmove-right) ; Ctl + right arrow
(global-set-key (kbd "C-c <left>")  'windmove-left)  ; Ctl + left arrow


;; outline minor mode keys
(global-set-key (kbd "C-c j i") 'hide-body)
(global-set-key (kbd "C-c j m") 'show-all)
(global-set-key (kbd "C-c j k") 'show-entry)
(global-set-key (kbd "C-c j j") 'hide-entry)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Invoke M-x without the Alt key
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

; [Ctrl]-[L]
(global-set-key "\C-l" 'goto-line)

;; expand-region
(global-set-key (kbd "C-=") 'er/expand-region)

;; magit-status




(setq js-indent-level 4)
(add-hook 'js-mode-hook
          (lambda ()
            ;; Scan the file for nested code blocks
            (imenu-add-menubar-index)
            ;; Activate the folding mode
            (hs-minor-mode t)))
;; Show-hide
(global-set-key (kbd "C-c <C-right>") 'hs-show-block)
(global-set-key (kbd "C-c <C-down>") 'hs-show-all)
(global-set-key (kbd "C-c <C-left>") 'hs-hide-block)
(global-set-key (kbd "C-c <C-up>") 'hs-hide-all)

;; (defun wicked/php-mode-init ()
;;   "Set some buffer-local variables."
;;   (setq case-fold-search t)
;;   (setq indent-tabs-mode nil)
;;   (setq-default indent-tabs-mode nil)
;;   (setq php-mode-force-pear nil)
;;   (setq fill-column 78)
;;   (setq c-basic-offset 4)
;;   (setq mumamo-background-colors nil))
;; (add-hook 'php-mode-hook 'wicked/php-mode-init)


(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

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


(global-set-key (kbd "C-c d") 'desktop-change-dir)
(global-set-key (kbd "C-c t") 'ansi-term)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c m") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)

(add-to-list 'magic-mode-alist '("<!DOCTYPE html .+DTD XHTML .+>" . nxml-mode))




(add-to-list 'load-path "~/.emacs.d/vendor/auto-complete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/vendor/auto-complete//ac-dict")
(ac-config-default)


(setq speedbar-use-images nil)

(make-face 'speedbar-face)
;; (set-face-font 'speedbar-face "Monaco-12")
;; (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))




;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(require 'autopair)

(require 'yaml-mode)

(when window-system
  (global-set-key (kbd "s-f") 'ns-toggle-fullscreen)
  (global-set-key (kbd "s-m") 'magit-status)
  (global-set-key (kbd "s-b") 'ibuffer)
  (global-set-key (kbd "<s-left>") 'previous-buffer)
  (global-set-key (kbd "<s-right>") 'next-buffer)

  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
  (load-theme 'solarized-light t)
  ;; (load-theme 'tango-dark t)
  )



(when (and (eq system-type 'darwin) window-system)
  ;; preview for Marked.app
  ;; https://github.com/JEG2/dotfiles/blob/master/emacs.d/packages.el#L121
  (defun osx-markdown-preview ()
    (interactive)
    (save-buffer)
    (call-process "open" nil nil nil "-a" "Marked.app" (buffer-file-name)))
  (add-hook 'markdown-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c o p") 'osx-markdown-preview))))

(defun senny-ruby-interpolate ()
  "In a double quoted string, interpolate."
  (interactive)
  (insert "#")
  (when (and
         (looking-back "\".*")
         (looking-at ".*\""))
    (insert "{}")
    (backward-char 1)))

(eval-after-load 'ruby-mode
  '(progn
     (define-key ruby-mode-map (kbd "#") 'senny-ruby-interpolate)))

(require 'flymake-ruby)
(add-hook 'ruby-mode-hook 'flymake-ruby-load)
(add-hook 'ruby-mode-hook 'ruby-end-mode)

(autopair-global-mode t)
