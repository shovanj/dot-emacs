(add-hook 'js-mode-hook (lambda ()
                          (add-to-list 'write-file-functions 'delete-trailing-whitespace )
                          (linum-mode 1)
                          (setq js-indent-level 2)))

(add-hook 'js2-mode-hook (lambda ()
                          (add-to-list 'write-file-functions 'delete-trailing-whitespace )
                          (linum-mode 1)
                          (setq js-indent-level 2)))
