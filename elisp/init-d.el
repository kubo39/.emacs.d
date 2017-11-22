;;;-------------------------------------
;;; D-Language
;;;-------------------------------------

(require 'd-mode)

(setq auto-mode-alist (cons '("\\.d$" . d-mode) auto-mode-alist))

(add-to-list 'exec-path "~/dlang/dmd-2.077.0/linux/bin64/")
(add-to-list 'exec-path "~/.dub/packages/dfmt-master/dfmt/")
(add-to-list 'exec-path "~/DCD/bin/")

(require 'company-dcd)

(add-hook 'd-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (setq tab-width 4)
            (company-dcd-mode)
            (dfmt-setup-keys)
            (define-key company-dcd-mode-map (kbd "M-.") 'company-dcd-goto-definition)
            (define-key company-dcd-mode-map (kbd "M-,") 'company-dcd-goto-def-pop-marker)
            ))

(provide 'init-d)
