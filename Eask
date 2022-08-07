(package "lsp-java"
         "3.0"
         "Java support for lsp-mode")

;; XXX: Linter will report errors because of the package main file; let's
;; disable this for now!

(files "*.el"
       "install/**")

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")
(depends-on 'lsp-mode)
(depends-on 'markdown-mode)
(depends-on 'treemacs)
(depends-on 'dap-mode)
(depends-on 'dash)
(depends-on 'ht)
(depends-on 'f)
(depends-on 'request)


(development
 (depends-on "company")
 (depends-on "flycheck")
 (depends-on "buttercup"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
