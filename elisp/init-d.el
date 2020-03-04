;;; init-d --- Settings for editing D source code

;;; Commentary:
;;

;;; Code:

(require 'd-mode)

(add-to-list 'exec-path "~/dlang/dmd-2.090.1/linux/bin64/")
(add-to-list 'exec-path "~/.dub/bin/")

(add-hook 'd-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (setq tab-width 4)
            #'lsp
            ))

(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("$HOME/.dub/bin"))
  :major-modes '(d-mode)
  :server-id 'dls))

(provide 'init-d)
;;; init-d.el ends here
