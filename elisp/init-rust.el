;; rust-mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; racer -- auto-compelte for rust
(setq racer-rust-src-path "~/rust/src/")
(setq racer-cmd "~/racer/target/release/racer")
(add-to-list 'load-path "/~/racer/editors")

(provide 'init-rust)
