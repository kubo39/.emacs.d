;;; init-d --- Settings for editing D source code

;;; Commentary:
;;

;; C-c ?   -- company-dcd-show-ddoc-with-buffer
;; M-.     -- company-dcd-goto-definition
;; C-c .   -- company-dcd-goto-definition (original)
;; M-,     -- company-dcd-def-pop-marker
;; C-c ,   -- company-dcd-def-pop-marker (original)
;; C-c s   -- company-dcd-ivy-search-symbol
;; C-c C-f -- dfmt-buffer
;; C-c F b -- dfmt-buffer (original)
;; C-c F f -- dfmt-file

;;; Code:

(require 'd-mode)

(add-to-list 'exec-path "~/dlang/dmd-2.089.0/linux/bin64/")
(add-to-list 'exec-path "~/.dub/bin/")

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
            (local-set-key (kbd "C-c C-f") 'dfmt-buffer)
            ))

(provide 'init-d)
;;; init-d.el ends here
