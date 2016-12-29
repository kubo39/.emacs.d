;; rust-mode

(require 'rust-mode)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook
          '(lambda ()
             (cargo-minor-mode)
             (racer-mode)
             (ac-etags-ac-setup)
             (local-set-key (kbd "C-c <tab>") 'rust-format-buffer)))

(add-hook 'racer-mode
          '(lambda ()
             (eldoc-mode)
             (company-mode)))

;; (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)

(provide 'init-rust)
