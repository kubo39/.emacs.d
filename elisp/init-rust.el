;;; init-rust --- Settings for editing Rust source code

;;; Commentary:
;;

;; C-c C-f  -- rust-format-buffer
;; M-.      -- racer-find-definition
;; M-,      -- pop-tag-mark

;;; Code:

(require 'rust-mode)

(add-to-list 'exec-path (expand-file-name "~/.cargo/bin/"))

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(provide 'init-rust)
;;; init-rust ends here
