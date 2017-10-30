;; idris-mode

(require 'idris-mode)
(setq auto-mode-alist (cons '("\\.idr$" . idris-mode) auto-mode-alist))

(provide 'init-idris)
