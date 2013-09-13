;; (defun smart-tab ()
;;   "This smart tab is minibuffer compliant: it acts as usual in
;;     the minibuffer. Else, if mark is active, indents region. Else if
;;     point is at the end of a symbol, expands it. Else indents the
;;     current line."
;;   (interactive)
;;   (if (minibufferp)
;;       (unless (minibuffer-complete)
;;         (dabbrev-expand nil))
;;     (if mark-active
;;         (indent-region (region-beginning)
;;                        (region-end))
;;       (if (looking-at "\\_>")
;;           (dabbrev-expand nil)
;;         (indent-for-tab-command)))))


;; custom methods
;; insert date
(defun sj-date ()
  "Insert date at point."
  (interactive)
  (insert (format-time-string "%a %Y-%m-%d")))


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
