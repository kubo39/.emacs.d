;; crystal

(el-get-bundle jpellerin/emacs-crystal-mode)
(add-to-list 'auto-mode-alist '("\\.cr$" . crystal-mode))

(provide 'init-crystal)
