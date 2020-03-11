;;; lsp-perl.el --- lsp-perl config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Installl: `cpanm Perl::LanguageServer`

;;; Code:

(require 'lsp-mode)

(defgroup lsp-perl nil
  "LSP support for Perl"
  :group 'lsp-mode
  :link '(url-link "https://github.com/richterger/Perl-LanguageServer"))

(defcustom lsp-perl-path "perl"
  "Path to perl interpreter."
  :type 'string
  :group 'lsp-perl)

(defcustom lsp-perl-server-port 13603
  ""
  :type 'integer
  :group 'lsp-perl)

(defcustom lsp-perl-client-version "2.1.0"
  ""
  :type 'string
  :group 'lsp-perl)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (list lsp-perl-path
                                           (format "-MPerl::LanguageServer -e Perl::LanguageServer::run -- --port %o --version %s"
                                                   lsp-perl-server-port lsp-perl-client-version))))
                  :major-modes '(perl-mode)
                  :priority -1
                  :server-id 'perl-lang-server))

(provide 'lsp-perl)
;;; lsp-perl.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
