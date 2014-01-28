(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-package)
(package-declare 'evil
                 'solarized-theme
                 'ujelly-theme
                 'zenburn-theme
                 'color-theme-wombat+
                 'clues-theme
                 'grandshell-theme
                 'cherry-blossom-theme
                 'phoenix-dark-pink-theme
                 'color-theme-sanityinc-tomorrow
                 'cyberpunk-theme
                 'flycheck
                 'company
                 'coffee-mode
                 'web-mode
                 'yaml-mode
                 'misc-cmds
                 'ido-ubiquitous
                 'powerline
                 )

;; base
(require 'init-look)
(require 'init-ido)
(require 'init-edit)

;; minor modes
(require 'init-evil)
(require 'init-flycheck)
(require 'init-company)

;; major modes
(require 'init-c)
(require 'init-web)

(provide 'init)
