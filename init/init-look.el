(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)

(setq ring-bell-function 'ignore)

(load-theme 'cyberpunk t)
(set-face-attribute 'default nil :height 110)

(powerline-default-theme)

(provide 'init-look)
