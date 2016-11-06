;; rust-mode

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; racer -- auto-compelte for rust
(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/rust/src/")

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
