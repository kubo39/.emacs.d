;;;-------------------------------------
;;; D-Language
;;;-------------------------------------

(require 'd-mode)

(setq auto-mode-alist (cons '("\\.d$" . d-mode) auto-mode-alist))
(setq load-path (cons "~/DCD/bin" load-path)) ;;;   DCDに load-path を通す

(require 'company-dcd)
(add-hook 'd-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

(provide 'init-d)
