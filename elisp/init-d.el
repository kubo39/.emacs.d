;;;-------------------------------------
;;; D-Language
;;;-------------------------------------


(require 'd-mode)
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)

(setq auto-mode-alist (cons '("\\.d$" . d-mode) auto-mode-alist))
(setup-flycheck-d-unittest)
(setq load-path (cons "~/DCD/bin" load-path)) ;;;   DCDに load-path を通す

(require 'ac-dcd)                             ;;; ac-dcd
(add-hook 'd-mode-hook
          '(lambda ()
             (c-set-style "bsd")
             (setq c-basic-offset 4)
             (setq indent-tabs-mode nil)
             (setq tab-width 4)
             (local-set-key  (kbd "C-c C-p") 'flycheck-previous-error)
             (local-set-key  (kbd "C-c C-n") 'flycheck-next-error)
             (ac-etags-ac-setup)
             (ac-dcd-setup)))

(provide 'init-d)
