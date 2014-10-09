(require 'cl)
(require 'iso-transl)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun package-declare (&rest packages)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package packages)
    (unless (package-installed-p package) (package-install package))))

(package-declare 'use-package                             ;package loading
                 'evil                                    ;VIM
                 'evil-leader                             ;leader key
                 'evil-nerd-commenter                     ;comments
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
(require 'use-package)

;; look
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

;; ido-mode
(ido-mode t)
(ido-everywhere t)
(ido-ubiquitous-mode t)
(flx-ido-mode t)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-use-virtual-buffers t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; edit
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
                compilation-mode-hook
                erc-mode-hook
                ))
  (add-hook hook
            (lambda ()
              (setq show-trailing-whitespace nil))))

(use-package evil
  :init
  (progn
    (use-package evil-leader
      :init
      (global-evil-leader-mode)
      :config
      (progn
        (setq evil-leader/in-all-states t)
        (evil-leader/set-leader ",")
        (evil-leader/set-key
          "b" 'ido-switch-buffer
          "f" 'ido-find-file
          "x" 'smex
          "ci" 'evilnc-comment-or-uncomment-lines
          )))
    (setq evil-default-cursor t)
    (evil-mode t)))

(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'after-init-hook #'global-company-mode)

;; C
(setq-default c-default-style "linux"
              c-basic-offset 4)

;; web
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 4)
            (setq web-mode-css-indent-offset 4)
            (setq web-mode-code-indent-offset 4)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init)
