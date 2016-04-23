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
;; perlのソースコード内パスからモジュールソース開く
(add-hook
 'cperl-mode-hook
 '(lambda ()
    (defun pm-find (module)
      (interactive (list (let* ((default-entry (cperl-word-at-point))
                                (input (read-string
                                        (format "perldoc entry%s: "
                                                (if (string= default-entry "")
                                                    ""
                                                  (format " (default %s)" default-entry))))))
                           (if (string= input "")
                               (if (string= default-entry "")
                                   (error "No perldoc args given")
                                 default-entry)
                             input))))
      (if (string= module "")
          (message "No module name found at this point.")
        (let (perldoc-output exit-status)
          (with-temp-buffer
            (setq exit-status (call-process "perldoc" nil t nil "-lm" module))
            (goto-char (point-min))
            (setq perldoc-output (buffer-substring (point-at-bol)
                                                   (point-at-eol))))
          (if (not (zerop exit-status))
              (message "No module found for \"%s\"." module)
            (find-file perldoc-output)))))
    (defun pm ()
      (interactive)
      (pm-find  (cperl-word-at-point))
      )
    )
 )

(provide 'init-perl)
