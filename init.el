;; =====================================
;;
;;  基本設定
;;
;; =====================================

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))  ;; el-get

(require 'init-elpa)
(package-initialize)


; 文字大きく
(set-face-attribute 'default nil
                    :family "Menlo" ;; font
                    :height 150) ;; font size

;; EmacsのWindowを一番上に表示
(if (eq window-system 'ns)
    (x-focus-frame nil))

;; Language.
(set-language-environment 'Japanese)

;;; 極力 utf-8 とする
(prefer-coding-system 'utf-8)

;;; 常時デバッグ状態
(setq debug-on-error t)

;;; tabサイズを4スペースに
(setq-default tab-width 4 indent-tabs-mode nil)


(require 'init-elpa)
(require 'init-ido)
(require 'init-tabbar)
(require 'init-autocomplete)
(require 'init-flycheck)
(require 'init-tramp)
(require 'init-kill-ring)


;;; eshell関連
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
(setq eshell-cmpl-cycle-completions t)
;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-cmpl-cycle-cutoff-length 5)


;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)

;;; スクリーン最大化
;(set-frame-parameter nil 'fullscreen 'maximized)

; 自動保存
(require 'auto-save-buffers-enhanced)
(setq auto-save-buffers-enhanced-interval 0.5)
(auto-save-buffers-enhanced t)

;;; region の色
(set-face-background 'region "SkyBlue")
(set-face-foreground 'region "black")

;;; 初期フレームの設定
(setq initial-frame-alist
      (append
       '((width    . 120)  ; フレーム幅(文字数)
	 (height   . 40)   ; フレーム高(文字数)
	 (top      . 60)   ; 表示位置
	 (left     . 80)   ; 表示位置
	 (foreground-color . "azure3") ; 文字が白
	 (background-color . "black")  ; 背景は黒
	 (border-color     . "black")
	 (mouse-color      . "white")
	 (cursor-color     . "green"))
       initial-frame-alist))


;;; undo-tree: Undo の履歴を視覚化
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

; 画面外の文字は折り返しして表示
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;; スクロール
(setq scroll-step 1)

;;; 対応する括弧を強調
(show-paren-mode t)

;;; C-c C-c でregionをコメントアウト
(global-set-key (kbd "C-c C-c") 'comment-region)

;;; C-c C-v でregionをコメントアウト解除
(global-set-key (kbd "C-c C-v") 'uncomment-region)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;;; スクロールバーを消す
(toggle-scroll-bar 0)

;;; メニューバーを消す
(menu-bar-mode 0)

;;; アイコンバーを消す
(tool-bar-mode 0)

;;; OccurをC-o にバインド
(global-set-key (kbd "C-o") 'occur)

;;; goto-line C-l に
(global-set-key (kbd "C-l") 'goto-line)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;; BackspaceをC-hに割り当て
(keyboard-translate ?\C-h ?\C-?)

;;; スタートアップメッセージを非表示
(setq inhibit-startup-message t)

;;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)

;;; バッテリー残量
;(display-battery-mode t)

;;; 文字列置換
(global-set-key (kbd "C-r") 'replace-regexp)

;;; yes と入力するのは面倒なので y でokにする
(fset 'yes-or-no-p 'y-or-n-p)

;; ヘルプバッファや補完バッファをポップアップで表示
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)


;; 行末の空白を表示
(setq-default show-trailing-whitespace t)

;-------------------------------------------------------



;; 言語系
(require 'init-d)
(require 'init-markdown)
(require 'init-ruby)
(require 'init-rust)
(require 'init-go)
(require 'init-nim)
(require 'init-crystal)
(require 'init-scala)
(require 'init-pony)
(require 'init-haml)


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(package-selected-packages
   (quote
    (powerline racer flycheck-rust dts-mode d-mode undo-tree tabbar smex scala-mode rust-mode quickrun popwin ponylang-mode markdown-mode julia-mode haml-mode go-mode go-autocomplete git-rebase-mode flycheck-pos-tip flycheck-d-unittest el-get company browse-kill-ring auto-save-buffers-enhanced anzu ac-nim ac-dcd)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
