;;; init-coq --- settings for editing Coq tactic.

;;; Commentary:

;;; Code:
(require 'proof-general)

(add-hook 'coq-mode-hook #'company-coq-mode)

(provide 'init-coq)
;;; init-coq.el ends here
