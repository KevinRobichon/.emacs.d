(menu-bar-mode -1)
(if (display-graphic-p)
 (progn
   (tool-bar-mode -1)
   (scroll-bar-mode -1)))

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

(load-theme 'grandshell t)

(powerline-default-theme)

(if (display-graphic-p)
  (set-face-attribute 'default nil :height 110))

(if (display-graphic-p)
  (maximize-frame))

(provide 'init-look)
