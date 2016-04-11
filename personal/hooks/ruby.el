(add-hook 'ruby-mode-hook (lambda ()
			    (add-to-list 'write-file-functions 'delete-trailing-whitespace )
                            (linum-mode 1)
                            (local-set-key "\r" 'newline-and-indent)))

(add-hook 'ruby-mode-hook 'projectile-mode)
