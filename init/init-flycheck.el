(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-clang-include-path '("."))

(provide 'init-flycheck)
