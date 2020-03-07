

;; =====================================
;;
;;  基本設定
;;
;; =====================================


;;; Code:

(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

(require 'init-elpa)
(package-initialize)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; 自動インストール設定
(require 'cl)

(defvar installing-package-list
  '(
    undo-tree
    company
    tabbar
    flycheck
    powerline
    moe-theme
    popwin
    browse-kill-ring
    multi-term
    dimmer
    use-package
    ;; mozc

    ;; coq
    proof-general
    company-coq
    ;; typescript
    tss
    ))
(let ((not-installed (loop for x in installing-package-list
                            when (not (package-installed-p x))
                            collect x)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))


(use-package exec-path-from-shell
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)


;; (require 'mozc)
;; (set-language-environment "Japanese")
;; (setq default-input-method "japanese-mozc")

;; ruby macicomment off
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (nim-mode haskell-mode d-mode python-mode gnu-elpa-keyring-update company-coq fsharp-mode exec-path-from-shell review-mode ghc jedi flymake-hlint flycheck-elm elm-mode bison-mode editorconfig dockerfile-mode toml-mode tss moe-theme powerline tabbar smex popwin company browse-kill-ring)))
 '(ruby-insert-encoding-magic-comment nil)
 '(safe-local-variable-values (quote ((whitespace-line-column . 80)))))

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

;; xsel --  M-x x-clipboard-copy
;; (defun x-clipboard-copy ()
;;   (interactive)
;;   (when (region-active-p)
;;     (shell-command-on-region (region-beginning) (region-end) "xsel -ib" nil nil)))

;; スクロール1行ごとに
(setq scroll-step 1)

(require 'company)
(global-company-mode)


;;; smex
(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  )


;;; ido-mode
(use-package ido
  :init
  (ido-mode t)
  :hook
  (ido-setup . (lambda ()
                 (define-key ido-completion-map [up] 'previous-history-element)))
  )


;;; tabber
(require 'tabbar)
(tabbar-mode)
(global-set-key "\M-]" 'tabbar-forward)  ; 次のタブ
(global-set-key "\M-[" 'tabbar-backward) ; 前のタブ
;; タブ上でマウスホイールを使わない
(tabbar-mwheel-mode nil)
;; グループを使わない
(setq tabbar-buffer-groups-function nil)
;; 左側のボタンを消す
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))
;; tabber色設定
(set-face-attribute ; バー自体の色
  'tabbar-default nil
   :background "black"
   :family "Inconsolata"
   :height 1.0)
(set-face-attribute ; アクティブなタブ
  'tabbar-selected nil
   :background "black"
   :foreground "green"
   :weight 'bold
   :box nil)
(set-face-attribute ; 非アクティブなタブ
  'tabbar-unselected nil
   :background "black"
   :foreground "red"
   :box nil)


;;; flycheck
(with-eval-after-load 'flycheck
  (flycheck-pos-tip-mode))
(add-hook 'after-init-hook #'global-flycheck-mode)


;;; tramp
(require 'tramp)
(setq tramp-default-method "ssh")


;;; kill-ring
(require 'browse-kill-ring)
(global-set-key "\M-y" 'browse-kill-ring)
;; C-g で終了
(add-hook 'browse-kill-ring-hook
          (lambda ()
            (define-key browse-kill-ring-mode-map (kbd "\C-g") 'browse-kill-ring-quit)))
;; Emacsのkill-ringsをクリップボードに対応
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-w" 'clipboard-kill-region)


;;; powerline
(require 'powerline)
(powerline-default-theme)


;;; eshell関連
;;
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
(setq eshell-cmpl-cycle-completions t)
;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-cmpl-cycle-cutoff-length 5)
;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)


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
;(setq system-uses-terminfo nil)

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

;; 非アクティブなウィンドウを暗くする
(require 'dimmer)
(dimmer-mode)

;-------------------------------------------------------

;; 言語系

(use-package lsp-mode
  :ensure t
  )


(use-package ccls
  :ensure t
  :custom
  (ccls-executable "~/ccls/Release/ccls")
  :init
  (add-hook 'c++-mode-hook
            '(lambda ()
               (setq c-basic-offset 4)
               (setq indent-tabs-mode nil)
               (setq tab-width 4)
               (require 'ccls)
               (lsp)
               ))
  (add-hook 'c-mode-hook
            '(lambda ()
               (setq c-basic-offset 4)
               (setq indent-tabs-mode nil)
               (setq tab-width 4)
               (require 'ccls)
               (lsp)
               ))
  )


(use-package d-mode
  :ensure t
  :init
  (add-to-list 'lsp-language-id-configuration '(d-mode . "d"))
  :hook
  (d-mode . (lambda ()
              (c-set-style "bsd")
              (setq c-basic-offset 4)
              (setq tab-width 4)
              (lsp)))
  :commands d-mode
  )
(lsp-register-client
 (make-lsp-client
  :new-connection (lsp-stdio-connection '("~/.dub/packages/.bin/dls-latest/dls"))
  :major-modes '(d-mode)
  :server-id 'dls))


(use-package dockerfile-mode
  :ensure t
  )


(use-package editorconfig
  :ensure t
  :init
  (setq editorconfig-exec-path "/usr/bin/editorconfig")
  :config
  (editorconfig-mode 1)
  )


(use-package elm-mode
  :ensure t
  :hook
  (elm-mode . lsp)
  )


(use-package go-mode
  :ensure t
  :init
  (add-to-list 'exec-path "~/go/bin/")
  :hook
  (go-mode . lsp)
  :commands go-mode
  )


(use-package haskell-mode
  :ensure t
  )


(use-package json-mode
  :ensure t
  )


(use-package markdown-mode
  :ensure t
  )


(use-package nasm-mode
  :ensure t
  :mode (("\\.asm$" . nasm-mode))
  )


(use-package nim-mode
  :ensure t
  :hook
  (nim-mode . lsp)
  :commands nim-mode
  :ensure-system-package
  (nimlsp . "nimble install nimlsp")
  )


(use-package perl-mode
  :ensure t
  :commands perl-mode
  )


(use-package python-mode
  :ensure t
  ;; :hook
  ;; (python-mode . lsp)
  :commands python-mode
  :interpreter (("python" . python-mode))
  )


(use-package review-mode
  :ensure t
  )


(use-package ruby-mode
  :ensure t
  :commands ruby-mode
  :mode (("\\.ru$" . ruby-mode)
         ("\\.gemspec$" . ruby-mode)
         ("Rakefile$" . ruby-mode))
  :interpreter (("ruby" . ruby-mode))
  :hook
  (ruby-mode . lsp)
  :ensure-system-package
  (solargraph . "gem install solargraph")
  )


(use-package rustic
  :ensure t
  :commands rustic
  )


(use-package toml-mode
  :ensure t
  )


(use-package tuareg
  :ensure t
  :hook
  (tuareg-mode . lsp)
  )


(use-package typescript-mode
  :ensure t
  :commands typescript-mode
  :hook
  (typescript-mode . (lambda ()
                       (require 'tss)
                       (tss-config-default)
                       (lsp)))
  )


(use-package yaml-mode
  :ensure t
  )


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))



(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(provide 'init)
;;; init.el ends here
