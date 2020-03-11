;;; lsp-perl.el --- lsp-perl config  -*- lexical-binding: t; -*-

;;; Commentary:

;; Install: `cpanm Perl::LanguageServer`

;;; Code:

(require 'lsp-mode)

(defgroup lsp-perl nil
  "LSP support for Perl"
  :group 'lsp-mode
  :link '(url-link "https://github.com/richterger/Perl-LanguageServer"))

(defcustom lsp-perl-language-server-path "perl"
  "Path to perl interpreter."
  :type 'string
  :group 'lsp-perl)

(defcustom lsp-perl-language-server-port 13603
  "Listen port."
  :type 'integer
  :group 'lsp-perl)

(defcustom lsp-perl-language-server-client-version "2.1.0"
  "client version."
  :type 'string
  :group 'lsp-perl)

(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection
                                   (lambda ()
                                     (list lsp-perl-language-server-path
                                           (format "-MPerl::LanguageServer -e Perl::LanguageServer::run -- --port %o --version %s"
                                                   lsp-perl-language-server-port lsp-perl-language-server-client-version))))
                  :major-modes '(perl-mode)
                  :priority -1
                  :server-id 'perl-language-server))

(provide 'lsp-perl)
;;; lsp-perl.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
