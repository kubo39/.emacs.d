;;--------------------------------
;; init-elpa
;;--------------------------------

(require 'package)

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t))

(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t) ;; meplaを追加
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t) ;; elpaを追加

(provide 'init-elpa)
