;; rust-mode

(require 'rust-mode)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))
;; (setq rust-format-on-save t)

(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode)))

;
(add-hook 'racer-mode #'eldoc-mode)
(add-hook 'racer-mode-hook #'company-mode)
(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(define-key rust-mode-map (kbd "TAB") #'company-indent-or-complete-common)
(setq company-tooltip-align-annotations t)
(provide 'init-rust)
