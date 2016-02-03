;; cperl-mode

(add-to-list 'auto-mode-alist '("\\.pl$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.pm$" . cperl-mode))
(add-to-list 'auto-mode-alist '("\\.t$" . cperl-mode))

(add-hook
 'cperl-mode-hook
 '(lambda ()
    (setq indent-tabs-mode nil)
    ; BestPractices からぱくったがなんかうごいてない
    (setq fill-column 78)
    (setq auto-fill-mode t)
    ; face設定。これはどっかちがうとこにうつす
    (set-face-background 'cperl-hash-face (face-background 'default))
    (setq cperl-hash-face 'cperl-hash-face)
    ;(make-face 'cperl-array-face)
    ;(set-face-foreground 'cperl-array-face "color-69")
    (set-face-background 'cperl-array-face (face-background 'default))
    (setq cperl-array-face 'cperl-array-face)
    (set-face-underline-p 'underline nil)
    (setq cperl-indent-level 4)
    (setq cperl-continued-statement-offset 4)
    (setq cperl-brace-offset -4)
    (setq cperl-label-offset -4)
    (setq cperl-indent-parens-as-block t)
    (setq cperl-close-paren-offset -4)
    (setq cperl-tab-always-indent t)
    (setq cperl-highlight-variables-indiscriminately t)
    (global-set-key (kbd "C-c C-v") 'uncomment-region)))

(provide 'init-perl)
