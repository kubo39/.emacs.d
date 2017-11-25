;;; init-d --- Settings for editing D source code

;;; Commentary:
;;

;; C-c ?   -- company-dcd-show-ddoc-with-buffer
;; C-c .   -- company-dcd-goto-definition
;; C-c ,   -- company-dcd-def-pop-marker
;; C-c s   -- company-dcd-ivy-search-symbol
;; C-c F b -- dfmt-buffer
;; C-c F f -- dfmt-file

;;; Code:

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
            (dfmt-setup-keys)))

(provide 'init-d)
;;; init-d.el ends here
