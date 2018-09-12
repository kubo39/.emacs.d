;;; init-pony --- Settings for editing Pony source code

;;; Commentary:
;;



;;; Code:

(require 'ponylang-mode)
(autoload 'ponylang-mode "ponylang-mode" "Major mode for editing Pony code." t)
(setq auto-mode-alist (cons '("\\.pony$" . ponylang-mode) auto-mode-alist))

(provide 'init-pony)
;;; init-pony.el ends here
