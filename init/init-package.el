(require 'cl)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun package-maybe-install (package)
  (unless (package-installed-p package) (package-install package)))

(defun package-declare (&rest packages)
  (package-initialize)
  (unless package-archive-contents (package-refresh-contents))
  (dolist (package packages)
    (package-maybe-install package))
  )

(provide 'init-package)
