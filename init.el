

;; =====================================
;;
;;  基本設定
;;
;; =====================================


;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
;; (add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))  ;; el-get

(require 'init-elpa)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; 自動インストール設定
(require 'cl)

(defvar installing-package-list
  '(
    smex
    undo-tree
    auto-save-buffers-enhanced
    company
    tabbar
    flycheck
    powerline
    moe-theme
    popwin
    el-get
    browse-kill-ring
    multi-term
    ;; mozc
    ac-etags

    ;; nasm
    nasm-mode
    ;; c
    auto-complete-c-headers
    auto-complete-clang
    google-c-style
    ;; crystal
    crystal-mode
    ;; d
    d-mode
    company-dcd
    dfmt
    ;; elm
    elm-mode
    ;; rust
    rust-mode
    flycheck-rust
    racer
    ;; ocaml
    tuareg
    ;; haskell
    haskell-mode
    ;; go
    go-mode
    company-go
    ;; nim
    nim-mode
    ;; ponylang
    ponylang-mode
    ;; idris
    idris-mode
    ;; typescript
    typescript-mode
    tss
    ;; toml
    toml-mode
    ;; yaml
    yaml-mode
    ;; markdown
    markdown-mode
    ;; json
    json-mode
    ;; review
    review-mode

    ;; GNU gobal
    ggtags
    ;; utils
    exec-path-from-shell
    xclip
    ))
(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
        (package-install pkg))))

;; (require 'mozc)
;; (set-language-environment "Japanese")
;; (setq default-input-method "japanese-mozc")

;; ruby macicomment off
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ac-etags-requires 1)
 '(package-selected-packages
   (quote
    (xclip exec-path-from-shell ggtags swift-mode rubocopfmt review-mode crystal-mode ghc jedi dfmt idris-mode flymake-hlint company-dcd flycheck-elm elm-mode haskell-mode bison-mode editorconfig dockerfile-mode erlang ponylang-mode toml-mode typescript-mode tss moe-theme powerline tabbar smex popwin el-get company browse-kill-ring auto-save-buffers-enhanced)))
 '(ruby-insert-encoding-magic-comment nil))

; 文字大きく
(set-face-attribute 'default nil
                    :family "Ubuntu Mono" ;; font
                    :height 150) ;; font size

;; EmacsのWindowを一番上に表示
(if (eq window-system 'ns)
    (x-focus-frame nil))

;; 半角/全角でも変換できるように
(define-key global-map [zenkaku-hankaku] 'toggle-input-method)

;; Language.
(set-language-environment 'Japanese)

;;; 極力 utf-8 とする
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; 警告音もフラッシュも全て無効
(setq ring-bell-function 'ignore)

;;; 常時デバッグ状態
(setq debug-on-error t)

;;; tabサイズを4スペースに
(setq-default tab-width 4 indent-tabs-mode nil)

;; suspendを無効に
(define-key global-map (kbd "C-x C-z") nil)

;; xclip
(require 'xclip)
(xclip-mode t)

;; スクロール1行ごとに
(setq scroll-step 1)

(require 'company)
(global-company-mode)

(require 'init-ido)
(require 'init-tabbar)
(require 'init-autocomplete)
(require 'init-flycheck)
(require 'init-tramp)
(require 'init-kill-ring)
(require 'init-powerline)
(require 'init-eshell)
;(require 'init-auto-save-buffers)
;(require 'init-yasnippet)

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

(when window-system
  (require 'moe-theme)
  (moe-dark))

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
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; メニューバーを消す
(if (fboundp 'scroll-bar-mode) (menu-bar-mode -1))

;;; アイコンバーを消す
(if (fboundp 'scroll-bar-mode) (tool-bar-mode -1))

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
(require 'init-editorconfig)

(require 'init-nasm)
(require 'init-c)
(require 'init-crystal)
(require 'init-d)
(require 'init-elm)
(require 'init-go)
(require 'init-haskell)
(require 'init-idris)
(require 'init-nim)
;; (require 'init-ocaml)
(require 'init-pony)
(require 'init-python)
(require 'init-markdown)
(require 'init-perl)
(require 'init-ruby)
(require 'init-rust)
(require 'init-ts)
(require 'init-review)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
