(global-evil-leader-mode)
(setq evil-leader/in-all-states t)
(evil-leader/set-leader ",")
(evil-leader/set-key
 "b" 'ido-switch-buffer
 "f" 'ido-find-file
 "x" 'smex
 )

(evil-mode t)
(setq evil-default-cursor t)

(provide 'init-evil)
