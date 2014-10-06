(add-to-list 'load-path (expand-file-name "init" user-emacs-directory))

(require 'iso-transl)
(require 'init-package)
(package-declare 'evil                                    ;VIM
                 'evil-leader                             ;leader key
                 'flycheck                                ;on-the-fly syntax
                 'company                                 ;completion
                 'magit                                   ;GIT integration
                 'flx                                     ;fuzzy search
                 'flx-ido                                 ;flx for ido
                 'smex                                    ;M-x enhancement
                 'coffee-mode                             ;coffeescript
                 'web-mode                                ;web templates
                 'yaml-mode                               ;YAML
                 'js2-mode                                ;javascript
                 'misc-cmds                               ;some commands
                 'ido-ubiquitous                          ;ido trully everywhere
                 'powerline                               ;powerline bottom bar
                 'frame-fns                               ;frame functions
                 'frame-cmds                              ;frame commands (interactive)
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
