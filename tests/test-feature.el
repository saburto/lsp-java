(require 'lsp-java)
(require 'lsp-mode)
(require 'lsp-protocol)
(require 'edmacro)
(require 'f)
(require 'ht)
(require 'dash)
(require 'flycheck)

(setq buttercup-stack-frame-style 'pretty)

(setq lsp-enable-snippet nil)

(global-auto-revert-mode t)

(defun lsp--java-wait-for (what pred)
  (while (not (funcall pred))
    (print (concat "waiting for ..." what))
    (sleep-for 1)))

(describe "lsp-java with a simple java project"

  (before-all
    (setq simple-java-folder (concat default-directory "tests/resources/simple-java"))
    (cd simple-java-folder)
    (find-file "src/main/java/simple/java/App.java")
    (lsp-workspace-folders-add simple-java-folder)
    (if (f-exists? (expand-file-name
                    (locate-user-emacs-file (f-join ".cache" "lsp" "eclipse.jdt.ls"))))
        (print "Already installed")
      (progn
        (lsp-install-server nil 'jdtls)
        (lsp--java-wait-for "download server"
                            (lambda () (not (-some--> #'lsp--client-download-in-progress? (lsp--filter-clients it)))))))
    (lsp-toggle-trace-io))

  (before-each
   ;; (delete-file "src/test/java/simple/java/AppTest.java")
   ;; (find-file "src/main/java/simple/java/App.java")
   ;; (revert-buffer nil t)
   (lsp)
   (lsp--java-wait-for "server ready"
                       (lambda () (--some? (eq (lsp--workspace-status it) 'initialized)
                                           (lsp--session-workspaces (lsp-session)))))
   )

  (xit "add a invalid statement"
      (insert "new ArrayList()" )
      (sleep-for 4)
      (expect (length (lsp-cur-line-diagnostics)) :to-equal 1))

  (xit "add a invalid import"
      (forward-line)
      (insert "import ok.ok.not.found;")
      (sleep-for 4)

      (cl-loop for diag across (lsp-cur-line-diagnostics)
               do (progn
                    (print (ht-get diag "message"))
                    (print
                     (ht-get* diag "range" "start" "line"))))

      (expect (length (lsp-cur-line-diagnostics)) :to-equal 1))

  (describe "using tests"
            (it "create a test from current file"

                (spy-on 'yes-or-no-p :and-return-value t)
                (let ((new-file (lsp-java-generate-test)))
                  (find-file new-file)
                  (expect (buffer-file-name) :to-match ".*/src/test/java/simple/java/AppTest.java$")
                  (print (buffer-string))))))





