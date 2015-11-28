;;; yasnippet

(add-to-list 'load-path "~/.emacs.d/yasnippet/")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode t)

(provide 'init-yasnippet)
