;;------------------------
;; ido-mode
;;------------------------

(ido-mode t)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

(provide 'init-ido)
