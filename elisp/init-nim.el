;; nim-mode
(eval-after-load 'nim-mode
  '(add-hook 'nim-mode-hook 'ac-nim-enable))

(provide 'init-nim)
