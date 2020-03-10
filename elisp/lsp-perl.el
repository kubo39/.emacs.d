;;; lsp-perl.el --- lsp-perl config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Installl: `cpanm Perl::LanguageServer`

;;; Code:

(require 'lsp-mode)

(defgroup lsp-perl nil
  "LSP support for Perl"
  :group 'lsp-mode
  :link '(url-link "https://github.com/richterger/Perl-LangServer")
  )

(defcustom lsp-perl-path "perl"
  "Path to perl interpreter."
  :type 'string
  :group 'lsp-perl)

(defcustom lsp-perl-lang-server-args nil
  "Add Perl::LanguageServer initialization options."
  :type '(repeat string)
  :group 'lsp-perl)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (cons lsp-perl-path
                                           '("-MPerl::LanguageServer -e Perl::LanguageServer::run -- --port 13603 --version 2.1.0"))))
                  :major-modes '(perl-mode)
                  :priority -1
                  :server-id 'perl-lang-server))

(provide 'lsp-perl)
;;; lsp-perl.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
