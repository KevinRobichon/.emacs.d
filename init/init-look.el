(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(show-paren-mode 1)

(setq ring-bell-function 'ignore)

(load-theme 'cyberpunk t)

(powerline-default-theme)

(set-face-attribute 'default nil :height 110)

(provide 'init-look)
