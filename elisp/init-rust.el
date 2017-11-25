;;; init-rust --- Settings for editing Rust source code

;;; Commentary:
;;

;; C-c C-f  -- rust-format-buffer
;; M-.      -- racer-find-definition
;; M-,      -- pop-tag-mark

;;; Code:

(require 'rust-mode)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook
          (lambda ()
            (racer-mode)
            (cargo-minor-mode)))

(add-hook 'racer-mode-hook #'eldoc-mode)

(add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

(provide 'init-rust)
;;; init-rust ends here
