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
                 'misc-cmds
                 'ido-ubiquitous
                 'powerline
                 )

(require 'init-ido)
(require 'init-evil)
(require 'init-look)

(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(defun web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(global-auto-revert-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-clang-include-path '("."))
(add-hook 'after-init-hook #'global-company-mode)

(provide 'init)
