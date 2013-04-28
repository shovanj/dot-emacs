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
