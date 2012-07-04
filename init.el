;; =====================================
;;
;;  基本設定
;;
;; =====================================

;; Language.
(set-language-environment 'Japanese)

;;; 極力 utf-8 とする
(prefer-coding-system 'utf-8)

;;; 常時デバッグ状態
(setq debug-on-error t)

;;; load-path を通す
(setq load-path (cons "~/.emacs.d/elisp" load-path))


;-------------------------------------------
(setq user-full-name "Hiroki Noda")
(setq user-main-address "kubo39@gmail.com")

;-------------------------------------------

;;; auto-install.el
;(when (require 'auto-install nil t)
; (setq auto-install-directory "~/.emacs.d/elisp/")
; (auto-install-update-emacswiki-package-name t)
; (auto-install-compatibility-setup))

;;; ターミナルエミュレータのシェルを bash に設定
(when (require 'multi-term nil t)
  (setq multi-term-program "/bin/bash"))

;;; 起動時にバッファ２分割、左側に bash 表示
(defun split-window-and-run-term()
  (setq w (selected-window))
  (setq w2 (split-window w nil t))
  (select-window w)
  (multi-term)
  (select-window w2))
(add-hook 'after-init-hook (lambda()(split-window-and-run-term)))

;;; スクリーン最大化
;(set-frame-parameter nil 'fullscreen 'maximized)

; 自動保存
(require 'auto-save-buffers)
(run-with-idle-timer 0.5 t 'auto-save-buffers)

; 自動略語補完
(require 'auto-complete)
(global-auto-complete-mode t)

(defcustom ac-modes
  '(emacs-lisp-mode lisp-interaction-mode
                    c-mode c++-mode java-mode
                    perl-mode cperl-mode python-mode ruby-mode
                    makefile-mode sh-mode fortran-mode f90-mode ada-mode
                    xml-mode sgml-mode)
  "Majo modes `auto-complete-mode' can run on."
  :type '(list symbol)
  :group 'auto-complete)

;;; region の色
(set-face-background 'region "SkyBlue")
(set-face-foreground 'region "black")

;;; 初期フレームの設定
(setq initial-frame-alist
      (append
       '((width    . 160)  ; フレーム幅(文字数)
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
;(set-fontset-font (frame-parameter nil 'font)
;		  'japanese-jisx0208
;		  '("Takaoゴシック" . "unicode-bmp"))

;;; undo-tree: Undo の履歴を視覚化
(when (require 'undo-tree nil t)
  (global-undo-tree-mode))

; 画面外の文字は折り返しして表示
(setq truncate-lines t)
(setq truncate-partial-width-windows nil)

;;; 対応する括弧を強調
(show-paren-mode t)

;;; 現在行を目立たせる
;(global-hl-line-mode)

;;; カーソルの位置が何行目かを表示する
;(line-number-mode t)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;; 行番号表示
(global-linum-mode)
(setq linum-format "%4d")

;; モードラインに時刻を表示させる
;(display-time)

;;; スクロールバーを消す
(toggle-scroll-bar nil)

;;; メニューバーを消す
(menu-bar-mode nil)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; C-k で行全体を削除
(setq kill-whole-line t)

;; BackspaceをC-hに割り当て
(keyboard-translate ?\C-h ?\C-?)

;;; スタートアップメッセージを非表示
(setq inhibit-startup-message t)

;;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)

;;; バッテリー残量
(display-battery-mode t)

;;; 文字列置換
(global-set-key (kbd "C-r") 'replace-string)

;;; shift+カーソルキーで分割ウィンドウの切り替え
;(windmove-default-keybindings)

;;; yes と入力するのは面倒なので y でokにする
(fset 'yes-or-no-p 'y-or-n-p)


;;; anything
(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)
(define-key global-map "\C-x\;" 'anything)

;;; anything-project
(require 'anything-project)
;(global-set-key (kbd "C-c C-f") 'anything-project)
(global-set-key (kbd "M-t") 'anything-project)

(ap:add-project
 :name 'python
 :look-for '(".git")
 :include-regexp '("\\.py$" "\\.html$")
  )



;;; kill-summry
; yankをべんりに
; C-n とC-pが大事。「.」でyankポインタを変更。
;(require 'kill-summary) ;"kill-summary" nil t)
;(define-key global-map "\ey" 'kill-summary)

; wdired
;(require 'wdired)
;(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)


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
;; .d を java-mode と関連付け
(setq auto-mode-alist (cons
 '("\\.d$" . java-mode) auto-mode-alist))
(setq interpreter-mode-alist (append
 '(("java" . java-mode)) interpreter-mode-alist))
(setq java-deep-indent-paren-style nil)
;(add-hook 'java-mode-hook '(lambda () (inf-java-keys)))


;;; *.ru
(setq auto-mode-alist (cons
 '("\\.ru$" . ruby-mode) auto-mode-alist))
(setq interpreter-mode-alist (append
 '(("ruby" . ruby-mode)) interpreter-mode-alist)x)


;;; *.tac, *.pyx
(setq auto-mode-alist (cons
  '("\\.tac$" . python-mode) auto-mode-alist))
(setq auto-mode-alist (cons
  '("\\.pyx$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (append
 '(("python" . python-mode)) interpreter-mode-alist))


;;haml-mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))

;; =====================================================
;;
;; flymake mode
;;
;; =====================================================

;; GUIの警告は表示しない
(setq flymake-gui-warnings-enabled nil)


;;;;  flymake for ruby
(when (load "flymake" t)
; (require 'flymake)
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
	       '("Rakefile$" flymake-ruby-init)))
(add-hook
 'ruby-mode-hook
 '(lambda ()
    ;; Don't want flymake mode for ruby regions in rhtml files
    (if (not (null buffer-file-name)) (flymake-mode))))


;;; python: flymake + pyflakes + pep8
(add-hook 'find-file-hook 'flymake-find-file-hook)
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
	   (local-file (file-relative-name
			temp-file
			(file-name-directory buffer-file-name))))
      (list "~/bin/pychecker"  (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	       '("\\.py\\'" flymake-pyflakes-init)))
(load-library "flymake-cursor")
