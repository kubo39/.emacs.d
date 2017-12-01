;;--------------------------------
;; init-elpa
;;--------------------------------

(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.milkbox.net/packages/") t) ;; meplaを追加
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t) ;; elpaを追加

(provide 'init-elpa)
