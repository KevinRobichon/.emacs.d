(global-auto-revert-mode t)
(show-paren-mode 1)

(setq-default ring-bell-function 'ignore
              tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace t
              make-backup-files nil
              auto-save-list-file-name nil
              auto-save-default nil
              )

(dolist (hook '(shell-mode-hook
                compilation-mode-hook))
  (add-hook hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

(provide 'init-edit)
