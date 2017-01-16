;; flycheck

(eval-after-load 'flycheck
  '(custom-set-variables
    (if window-system
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
(if window-system
    (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'init-flycheck)
