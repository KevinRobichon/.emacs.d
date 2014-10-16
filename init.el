(require 'cl)
(require 'iso-transl)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(defun package-declare (&rest packages)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package packages)
    (unless (package-installed-p package) (package-install package))))

(package-declare 'use-package                             ;package loading
                 'evil                                    ;VIM
                 'evil-leader                             ;leader key
                 'ace-jump-mode                           ;fast motion
                 'flycheck                                ;on-the-fly syntax
                 'company                                 ;completion
                 'magit                                   ;GIT integration
                 'flx                                     ;fuzzy search
                 'flx-ido                                 ;flx for ido
                 'ido-ubiquitous                          ;ido trully everywhere
                 'smex                                    ;M-x enhancement
                 'coffee-mode                             ;coffeescript
                 'web-mode                                ;web templates
                 'yaml-mode                               ;YAML
                 'js2-mode                                ;javascript
                 'misc-cmds                               ;some commands
                 'frame-fns                               ;frame functions
                 'frame-cmds                              ;frame commands (interactive)
                 'nyan-mode                               ;nyan cat
                 'smart-mode-line                         ;better modeline
                 'ujelly-theme
                 'color-theme-wombat+
                 'clues-theme
                 'grandshell-theme
                 'cherry-blossom-theme
                 'phoenix-dark-pink-theme
                 'color-theme-sanityinc-tomorrow
                 'cyberpunk-theme
                 'base16-theme
                 'sublime-themes
                 )
(require 'use-package)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-auto-revert-mode t)
(show-paren-mode 1)

(line-number-mode t)
(column-number-mode t)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

(setq-default ring-bell-function 'ignore
              tab-width 4
              indent-tabs-mode nil
              show-trailing-whitespace t
              make-backup-files nil
              auto-save-list-file-name nil
              auto-save-default nil)

(unless (display-graphic-p)
  (require 'mouse)
  (xterm-mouse-mode t)
  (global-set-key [mouse-4] (lambda ()
                               (interactive)
                               (scroll-down 1)))
  (global-set-key [mouse-5] (lambda ()
                               (interactive)
                               (scroll-up 1)))
  (setq mouse-sel-mode t))

(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
(when (member "Dina" (font-family-list))
  (set-face-attribute 'default nil :font "Dina-8"))

(load-theme 'grandshell t)

(use-package smart-mode-line
  :init
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'dark)
  (set-face-attribute 'mode-line nil
                      :box '(:line-width 1 :color "gray50"))
)
;; (use-package smart-mode-line-powerline-theme)
(use-package nyan-mode
  :init (nyan-mode))

(use-package magit)

(use-package ido
  :init
  (ido-mode t)
  :config
  (setq ido-everywhere t
        ido-enable-flex-matching t
        ido-use-filename-at-point nil
        ido-use-virtual-buffers t)
  (use-package ido-ubiquitous
    :init (ido-ubiquitous-mode t))
  (use-package flx
    :config
    (use-package flx-ido
      :init (flx-ido-mode t)))
  (use-package smex
    :init (smex-initialize)
    :bind ("M-x" . smex)))

(use-package company
  :commands global-company-mode
  :idle (global-company-mode t)
  :config
  (define-key company-active-map (kbd "\C-n") 'company-select-next)
  (define-key company-active-map (kbd "\C-p") 'company-select-previous)
  (define-key company-active-map (kbd "\C-d") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "<tab>") 'company-complete))

(use-package flycheck
  :commands global-flycheck-mode
  :idle (global-flycheck-mode t))

(use-package ace-jump-mode)

(use-package evil
  :pre-load
  (setq evil-default-cursor t
        evil-want-C-u-scroll t)
  :init
  (evil-mode t)
  :config
  (define-key evil-normal-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
  (define-key evil-visual-state-map (kbd "SPC") 'evil-ace-jump-char-mode)
  (use-package evil-leader
    :init
    (global-evil-leader-mode)
    :config
    (setq evil-leader/in-all-states t)
    (evil-leader/set-leader ",")
    (evil-leader/set-key
      "b" 'ido-switch-buffer
      "f" 'ido-find-file
      "x" 'smex
      )))

(setq-default c-default-style "linux"
              c-basic-offset 4)

(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-markup-indent-offset 4)
            (setq web-mode-css-indent-offset 4)
            (setq web-mode-code-indent-offset 4)))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.hbs\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(provide 'init)
