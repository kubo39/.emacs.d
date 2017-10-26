
(require 'auto-complete-clang)
(setq ac-clang-flags
      (append '("-std=c++11")
              (mapcar (lambda (item) (concat "-I" item))
                      (split-string
                       "
 /usr/include/c++/4.8
 /usr/include/x86_64-linux-gnu/c++/4.8
 /usr/include/c++/4.8/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
                       ))))


(require 'auto-complete-c-headers)

(add-hook 'c-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (add-to-list 'ac-sources
                         'ac-source-c-headers t)))
(add-hook 'c++-mode-hook
          (lambda ()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (add-to-list 'ac-sources
                         'ac-source-c-headers t)))


(provide 'init-c)
