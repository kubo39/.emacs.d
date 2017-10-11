;;;-------------------------------------
;;; D-Language
;;;-------------------------------------

(require 'd-mode)

(setq auto-mode-alist (cons '("\\.d$" . d-mode) auto-mode-alist))

(add-to-list 'exec-path "~/dlang/dmd-2.076.1/linux/bin64/")
(add-to-list 'exec-path "~/DCD/bin/")

(require 'company-dcd)
(add-hook 'd-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

(provide 'init-d)
