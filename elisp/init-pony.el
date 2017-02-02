;;; ponylang

(require 'ponylang-mode)
(autoload 'ponylang-mode "ponylang-mode" "Major mode for editing Pony code." t)
(setq auto-mode-alist (cons '("\\.pony$" . ponylang-mode) auto-mode-alist))

(provide 'init-pony)

