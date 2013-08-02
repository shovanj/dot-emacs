(require 'yaml-mode)

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-hook 'yaml-mode-hook
            '(lambda ()
               (add-to-list 'write-file-functions 'delete-trailing-whitespace )
               (linum-mode 1)
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
