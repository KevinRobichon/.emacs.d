;;; package --- Summary
;;; Commentary:
(require 'cl)

;;; Code:
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun package-version-joined (package)
  (package-version-join (package-desc-vers (cdr (assoc package package-alist)))))

(defun package-dependencies (package)
  (let ((info (assoc package package-alist)))
    (and info (mapcar 'car (package-desc-reqs (cdr info))))))

(defun package-maybe-install (package)
  (unless (package-installed-p package) (package-install package)))

(defun package-list-dependencies (packages)
  (delete-dups (apply 'append (cons packages (mapcar 'package-dependencies packages)))))

(defun package-list-dependencies-recursive (packages)
  (let ((prev (copy-seq packages))
        (deps (package-list-dependencies packages)))
    (while (set-difference deps prev)
      (setq prev (copy-seq deps))
      (setq deps (package-list-dependencies prev)))
    deps))

(defun package-installed ()
  (mapcar 'car package-alist))

(defun package-declare (&rest packages)
  (package-initialize)
  (let (deps)
    (unless package-archive-contents (package-refresh-contents))
    (dolist (package packages)
      (package-maybe-install package))
    (setq deps (package-list-dependencies-recursive packages))
    (dolist (package (set-difference (package-installed) deps))
      (package-delete (symbol-name package) (package-version-joined package)))))

(package-declare 'evil
                 'solarized-theme
                 'flycheck
                 'company
                 'coffee-mode
                 'web-mode
                 'misc-cmds
                 )

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode 1)

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(defun web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq ring-bell-function 'ignore)

(ido-mode t)

(global-auto-revert-mode t)

(evil-mode t)
(setq evil-default-cursor t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-clang-include-path '("."))
(add-hook 'after-init-hook #'global-company-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 4)
 '(custom-safe-themes (quote ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load-theme 'solarized-dark)
(set-face-attribute 'default nil :height 110)

(provide 'init)
;;; init.el ends here
