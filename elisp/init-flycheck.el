;;; flycheck -- Settings for flycheck

;;; Commentary:
;;

;;; Code:

(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))

(add-hook 'after-init-hook #'global-flycheck-mode)

(provide 'init-flycheck)
;;; init-flycheck.el ends here
