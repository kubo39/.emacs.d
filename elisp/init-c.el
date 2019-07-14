(require 'cc-mode)

(require 'company)
(add-to-list 'company-backend 'company-c-headers)

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq flycheck-clang-language-standard "c++17")))


(provide 'init-c)
;;; init-c ends here
