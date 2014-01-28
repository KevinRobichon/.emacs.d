(defun web-mode-hook ()
  "Hook for web-mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4))

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init-web)