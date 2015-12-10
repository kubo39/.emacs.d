;;; yasnippet

(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet")
(require 'yasnippet)
(yas/load-directory "~/.emacs.d/snippets")
(yas/global-mode t)

(custom-set-variables '(yas-trigger-key "TAB"))

(provide 'init-yasnippet)
