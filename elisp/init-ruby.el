;;--------------------
;; ruby
;;--------------------

;; (add-to-list 'exec-path "~/.anyenv/envs/rbenv/shims/")

;(add-to-list 'exec-path "/usr/local/bin")
(require 'ggtags)

;;; *.ru *.gemspec Rakefile
(setq auto-mode-alist (cons
 '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons
 '("\\.gemspec$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons
 '("Rakefile$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append
 '(("ruby" . ruby-mode)) interpreter-mode-alist))

;; (add-hook 'ruby-mode-hook
;;          (lambda()
;;            (ggtags-mode)))

;; (require 'rubocopfmt)
;; (add-hook 'ruby-mode-hook #'rubocopfmt-mode)

(provide 'init-ruby)
