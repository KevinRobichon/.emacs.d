(require 'cl)

(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defun package-version-joined (package)
  (package-version-join (package-desc-vers (cdr (assoc package package-alist)))))

(defun package-dependencies (package)
  (let ((info (assoc package package-alist)))
    (and info (mapcar 'car (package-desc-reqs (cadr info))))))

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
    ;(setq deps (package-list-dependencies-recursive packages))
    ;(dolist (package (set-difference (package-installed) deps))
      ;(package-delete (symbol-name package) (package-version-joined package)))
    ))

(provide 'init-package)
