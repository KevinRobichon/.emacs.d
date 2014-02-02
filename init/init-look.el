(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(load-theme 'grandshell t)

(powerline-default-theme)

(set-face-attribute 'default nil :height 110)

(maximize-frame)
(toggle-frame-fullscreen)

(provide 'init-look)
