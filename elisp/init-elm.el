;; elm-mode

(require 'elm-mode)

(setq auto-mode-alist (cons '("\\.elm$" . elm-mode) auto-mode-alist))

(with-eval-after-load 'company
  (add-to-list 'company-backends 'company-elm))
(add-hook 'elm-mode-hook #'elm-oracle-setup-completion)

(add-hook 'elm-mode-hook
          (lambda ()
            (setq company-backends '(company-elm))))

(provide 'init-elm)
