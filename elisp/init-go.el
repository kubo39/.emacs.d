;;; init-go

;;; Commentary:
;;

(require 'go-mode)
(require 'company-go)

(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook 'flycheck-mode)
(add-hook 'go-mode-hook (lambda()
           (local-set-key (kbd "M-.") 'godef-jump)
           (set (make-local-variable 'company-backends) '(company-go))
           (company-mode)))

(provide 'init-go)
;;; init-go.el ends here

