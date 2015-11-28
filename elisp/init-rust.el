;; rust-mode

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs$" . rust-mode))

;; racer -- auto-compelte for rust
(setq racer-rust-src-path "~/rust/src/")
(setq racer-cmd "~/racer/target/release/racer")
(add-to-list 'load-path "/~/racer/editors")

(add-hook 'rust-mode-hook
  '(lambda ()
     (racer-activate)
     (local-set-key (kbd "M-.") #'racer-find-definition)
     (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

(provide 'init-rust)
