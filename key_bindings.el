(when window-system
  (global-set-key (kbd "s-g") 'magit-status)
  (global-set-key (kbd "s-b") 'ibuffer)
  (global-set-key (kbd "<s-left>") 'previous-buffer)
  (global-set-key (kbd "<s-right>") 'next-buffer)
  (global-set-key (kbd "s-t") 'projectile-find-file)
  (global-set-key (kbd "s-/") 'comment-or-uncomment-region-or-line)


  ;;(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
  ;;(load-theme 'solarized-light t)
  )


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

;; Show-hide
(global-set-key (kbd "C-c <C-right>") 'hs-show-block)
(global-set-key (kbd "C-c <C-down>") 'hs-show-all)
(global-set-key (kbd "C-c <C-left>") 'hs-hide-block)
(global-set-key (kbd "C-c <C-up>") 'hs-hide-all)

(global-set-key (kbd "C-c d") 'desktop-change-dir)
(global-set-key (kbd "C-c t") 'ansi-term)
(global-set-key (kbd "C-c e") 'eval-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c l") 'magit-log)


;; Optional: set up a quick key to toggle nav
(global-set-key [f8] 'nav-toggle)

;; disable C-z
(global-unset-key "\C-z")



(when (and (eq system-type 'darwin) window-system)
  ;; preview for Marked.app
  ;; https://github.com/JEG2/dotfiles/blob/master/emacs.d/packages.el#L121
  (defun sj-osx-markdown-preview ()
    (interactive)
    (save-buffer)
    (call-process "open" nil nil nil "-a" "Marked.app" (buffer-file-name)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (local-set-key (kbd "C-c o p") 'osx-markdown-preview)))


  (defun sj-osx-finder-open ()
    (interactive)
    (save-buffer)
    (call-process "open" nil nil nil "-a" "Finder.app" "."))
  ;; (global-set-key (kbd "s-o") 'osx-finder-open)
  (global-set-key (kbd "C-c o o")    'osx-finder-open))


(defun sj-delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

