;; go-mode

(require 'go-mode)
(setq auto-mode-alist (cons
  '("\\.go$" . go-mode) auto-mode-alist))
(require 'go-autocomplete)

(provide 'init-go)
