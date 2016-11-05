;;----------------------
;; auto-complete
;;----------------------

(require 'auto-complete)
(global-auto-complete-mode t)

(require 'ac-etags)

(custom-set-variables
  '(ac-etags-requires 1))

(eval-after-load "etags"
  '(progn
      (ac-etags-setup)))

(provide 'init-autocomplete)
