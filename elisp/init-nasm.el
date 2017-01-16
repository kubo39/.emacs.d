;;;----------------------
;;; nasm
;;;----------------------

(require 'nasm-mode)
(setq auto-mode-alist (cons '("\\.asm$" . nasm-mode) auto-mode-alist))

(provide 'init-nasm)
