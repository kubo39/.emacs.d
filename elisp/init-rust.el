;; rust-mode
(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; racer -- auto-compelte for rust
(setq racer-rust-src-path "/home/kubo39/rust/src/")
(setq racer-cmd "/home/kubo39/racer/target/release/racer")
(add-to-list 'load-path "/home/kubo39/racer/editors")

(provide 'init-rust)
