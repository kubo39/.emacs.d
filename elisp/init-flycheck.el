;; flycheck

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-flycheck)
