;; rust-mode

(require 'rust-mode)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook
          '(lambda ()
             (racer-mode)))
;
(add-hook 'racer-mode
          '(lambda ()
             (eldoc-mode)))

(provide 'init-rust)
