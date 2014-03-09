(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'init-package)
(package-declare 'evil
                 'flycheck
                 'company
                 'magit
                 'flx
                 'flx-ido
                 'smex
                 'coffee-mode
                 'web-mode
                 'yaml-mode
                 'js2-mode
                 'misc-cmds
                 'ido-ubiquitous
                 'powerline
                 'frame-fns
                 'frame-cmds
                 'ujelly-theme
                 'color-theme-wombat+
                 'clues-theme
                 'grandshell-theme
                 'cherry-blossom-theme
                 'phoenix-dark-pink-theme
                 'color-theme-sanityinc-tomorrow
                 'cyberpunk-theme
                 'base16-theme
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
(require 'init-js)

(provide 'init)
