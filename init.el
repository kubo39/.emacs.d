;; =====================================
;;
;;  基本設定
;;
;; =====================================

(setq default-directory (expand-file-name "~/")) ; osx cocoa emacs workaround.


;; EmacsのWindowを一番上に表示
(if (eq window-system 'ns)
    (x-focus-frame nil))

;; Language.
(set-language-environment 'Japanese)

;;; 極力 utf-8 とする
(prefer-coding-system 'utf-8)

;;; 常時デバッグ状態
(setq debug-on-error t)

;;; load-path を通す
(setq load-path (cons "~/.emacs.d/elisp" load-path))

;;; tabサイズを4スペースに
(setq-default tab-width 4 indent-tabs-mode nil)

; server start for emacs-client
(require 'server)
(unless (server-running-p)
  (server-start))


;package.el
(require 'package)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t) ;; meplaを追加
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t) ;;
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t) ;; elpaを追加
; 初期化
(package-initialize)

(defvar installing-package-list
  '(
    markdown-mode
    scss-mode
    js2-mode
    ruby-block
    google-c-style
    yaml-mode
    flycheck
    yasnippet
    dropdown-list
    migemo
    open-junk-file
    recentf-ext
    popwin
    google-translate
    auto-complete
    foreign-regexp
    auto-highlight-symbol
    highlight-symbol
    slim-mode
    apache-mode
    zenburn-theme
    ssh-config-mode
    smarty-mode
    go-mode
    go-autocomplete
    jade-mode
    coffee-mode
    haml-mode
    ))


;; melpa.el
;; (require 'melpa)

;-------------------------------------------------------

;;; tabbar
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

;;; auto-install.el
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
 ;(auto-install-update-emacswiki-package-name t)
  (auto-install-compatibility-setup))

;;; ターミナルエミュレータのシェルを bash に設定
;; (when (require 'multi-term nil t)
;;   (setq multi-term-program "/bin/bash"))

;;; 起動時にバッファ２分割、左側に bash 表示
;; (defun split-window-and-run-term()
;;   (setq w (selected-window))
;;   (setq w2 (split-window w nil t))
;;   (select-window w)
;;  (multi-term)
;;   (select-window w2))
;; (add-hook 'after-init-hook (lambda()(split-window-and-run-term)))

;; (eshell)

;;; eshell関連
;; 確認なしでヒストリ保存
(setq eshell-ask-to-save-history (quote always))
;; 補完時にサイクルする
(setq eshell-cmpl-cycle-completions t)
;;補完候補がこの数値以下だとサイクルせずに候補表示
(setq eshell-cmpl-cycle-cutoff-length 5)


; 自動略語補完
(require 'auto-complete)
(require 'go-autocomplete)
(global-auto-complete-mode t)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode c++-mode java-mode go-mode d-mode
                    perl-mode cperl-mode python-mode ruby-mode
                    makefile-mode sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Majo modes `auto-complete-mode' can run on."
  :type '(list symbol)
  :group 'auto-complete)


;; 履歴で重複を無視する
(setq eshell-hist-ignoredups t)

;;; スクリーン最大化
;(set-frame-parameter nil 'fullscreen 'maximized)

; 自動保存
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

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

;; フォントの設定
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("Takaoゴシック" . "unicode-bmp"))

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

;;; 透明度の設定
;(set-frame-parameter nil 'alpha '80)

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
(toggle-scroll-bar nil)

;;; メニューバーを消す
(menu-bar-mode nil)

;;; アイコンバーを消す
(tool-bar-mode nil)

;;; OccurをC-, にバインド
(global-set-key (kbd "C-,") 'occur)

;;; goto-line C-: に
(global-set-key (kbd "C-:") 'goto-line)

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

;;; shift+カーソルキーで分割ウィンドウの切り替え
;(windmove-default-keybindings)

;;; yes と入力するのは面倒なので y でokにする
(fset 'yes-or-no-p 'y-or-n-p)

;;; anything
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(define-key global-map "\C-x\;" 'anything)
;; (add-to-list 'anything-sources 'anything-c-source-emacs-commands)

;; バッファ切り替えをanythingに
;;(define-key global-map (kbd "\C-x b") 'anything)


;;; anything-project
;; (require 'anything-project)
;; (global-set-key (kbd "M-t") 'anything-project)

;; (ap:add-project
;;  :name 'python
;;  :look-for '(".git")
;;  :include-regexp '("\\.c$" "\\.h$" "\\.js$" "\\.rb$" "\\.py$" "\\.html$")
;;  )

;; ヘルプバッファや補完バッファをポップアップで表示
(require 'popwin)
(setq display-buffer-function 'popwin:display-buffer)

;;; yasnippet
;(add-to-list 'load-path "~/.emacs.d/elisp/yasnippet/")
;(require 'yasnippet)
;(yas/initialize)
;(yas/load-directory "~/.emacs.d/elisp/snippets")
;(yas/global-mode t)

;;; kill-summary
; yankをべんりに
; C-n とC-pが大事。「.」でyankポインタを変更。
(require 'kill-summary) ;"kill-summary" nil t)
(define-key global-map "\ey" 'kill-summary)

;; Emacsのkill-ringsをクリップボードに対応
(global-set-key "\M-w" 'clipboard-kill-ring-save)
(global-set-key "\C-w" 'clipboard-kill-region)

; wdired
;(require 'wdired)
;(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


;; =====================================================
;;
;; IRC-mode: ERC
;;
;; =====================================================

;; (require 'erc)
;; (require 'flyspell)
;; (require 'ispell)
;; (eval-when-compile (require 'cl))
;; (defun start-irc ()
;;   "Connect to IRC."
;;   (interactive)
;;   (lexical-let* ((nickname "kubo39"))
;;     (setq erc-autojoin-channels-alist '(("mixiwatcher.lo.mixi.jp"
;;                                          "#graduates2014" "#hegemony-dev"))
;;           erc-nick (append `(,nickname)
;;                            (loop for i from ?a upto ?z
;;                                  for string = (char-to-string i)
;;                                  if (string-match "[a-z]" string)
;;                                  collect (concat nickname (capitalize string)))))
;;     ;; (erc-tls :server "irc.oftc.net" :port 6697
;;     ;;          :full-name "you")
;;     (erc :server "mixiwatcher.lo.mixi.jp" :port 6667
;;          :full-name "kubo39"))
;;   )
;; ;; auto spell checker

;; (add-hook 'erc-mode-hook
;;           'flyspell-mode-on)
;; (global-set-key (kbd "C-c i") 'start-irc)



;; =====================================================
;;
;; org-mode
;;
;; =====================================================

(require 'org-install)
;; キーバインドの設定
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cr" 'org-remember)
;; 拡張子がorgのファイルを開いた時，自動的にorg-modeにする
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; org-modeでの強調表示を可能にする
(add-hook 'org-mode-hook 'turn-on-font-lock)
;; 見出しの余分な*を消す
(setq org-hide-leading-stars t)
;; org-default-notes-fileのディレクトリ
(setq org-directory "~/org/")
;; org-default-notes-fileのファイル名
(setq org-default-notes-file "notes.org")


;; =====================================================
;;
;; root権限でファイルを開く設定
;;
;; =====================================================

;;; sudo とか ssh とか ubuntu用
(require 'tramp)

(defun th-rename-tramp-buffer ()
  (when (file-remote-p (buffer-file-name))
    (rename-buffer
     (format "%s:%s"
             (file-remote-p (buffer-file-name) 'method)
             (buffer-name)))))

(add-hook 'find-file-hook
          'th-rename-tramp-buffer)

(defadvice find-file (around th-find-file activate)
  "Open FILENAME using tramp's sudo method if it's read-only."
  (if (and (not (file-writable-p (ad-get-arg 0)))
           (y-or-n-p (concat "File "
                             (ad-get-arg 0)
                             " is read-only.  Open it as root? ")))
      (th-find-file-sudo (ad-get-arg 0))
    ad-do-it))

(defun th-find-file-sudo (file)
  "Opens FILE with root privileges."
  (interactive "F")
  (set-buffer (find-file (concat "/sudo::" file))))


;; =====================================================
;;
;; Languages mode(各言語モード)
;;
;; =====================================================

;;; Scala
(add-to-list 'load-path "~/.emacs.d/scala-mode")
(require 'scala-mode-auto)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;;; turn on syntax highlighting
(global-font-lock-mode 1)

;;; Groovy
(add-to-list 'load-path "~/.emacs.d/groovy-mode")
;;; use groovy-mode when file ends in .groovy or has #!/bin/groovy at start
(autoload 'groovy-mode "groovy-mode" "Major mode for editing Groovy code." t)
(add-to-list 'auto-mode-alist '("\.groovy$" . groovy-mode))
(add-to-list 'interpreter-mode-alist '("groovy" . groovy-mode))

;;; make Groovy mode electric by default.
(add-hook 'groovy-mode-hook
          '(lambda ()
             (require 'groovy-electric)
             (groovy-electric-mode)))


;;; D-Language

(add-to-list 'load-path "~/.emacs.d/d-mode")
(autoload 'd-mode "d-mode" "Major mode for editing D code." t)
(setq auto-mode-alist (cons
		       '("\\.d$" . d-mode) auto-mode-alist))


;;; *.ru *.gemspec Rakefile
(setq auto-mode-alist (cons
 '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons
 '("\\.gemspec$" . ruby-mode) auto-mode-alist))
(setq auto-mode-alist (cons
 '("Rakefile$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append
 '(("ruby" . ruby-mode)) interpreter-mode-alist)x)

;;; *.tac, *.pyx
(setq auto-mode-alist (cons
  '("\\.tac$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons
  '("\\.pyx$" . python-mode) auto-mode-alist))


;;haml-mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))


;; clojure-mode
(require 'clojure-mode)
(add-to-list 'auto-mode-alist '("\\.clj$" . clojure-mode))


;; rust-mode
(require 'rust-mode)


;; julia-mode
(require 'julia-mode)


;; markdown-mode
(require 'markdown-mode)

(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))


;; =====================================================
;;
;; flymake mode
;;
;; =====================================================

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)

(load "flymake")

;;;;  flymake for ruby
;; I don't like the default colors :)
(set-face-background 'flymake-errline "red4")
(set-face-background 'flymake-warnline "dark slate blue")
;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "ruby" (list "-c" local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.rb\\'" flymake-ruby-init))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.ru\\'" flymake-ruby-init))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.gemspec\\'" flymake-ruby-init))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("Rakefile$" flymake-ruby-init))
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))))


;;; python: flymake + pyflakes + pep8
(add-hook 'find-file-hook 'flymake-find-file-hook)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "~/.emacs.d/pycheckers"  (list local-file))
      ))
(add-to-list 'flymake-allowed-file-name-masks
	     '("\\.py\\'" flymake-pyflakes-init))

;;; jedi - python autocompletion
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)


;; (load-library "flymake-cursor")
