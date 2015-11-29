;;; eshell関連

;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
(setq eshell-cmpl-cycle-completions t)
;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-cmpl-cycle-cutoff-length 5)


;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)

(provide 'init-eshell)
