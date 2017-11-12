;; nim-mode

(setq nimsuggest-path "~/.nimble/bin/nimsuggest")

(require 'nim-mode)

(add-hook 'nim-mode-hook 'nimsuggest-mode)
(add-hook 'nimsuggest-mode-hook 'company-mode)

(provide 'init-nim)
