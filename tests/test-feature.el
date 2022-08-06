(require 'lsp-java)
(require 'lsp-mode)
(require 'lsp-protocol)
(require 'edmacro)
(require 'f)
(require 'ht)
(require 'dash)
(require 'flycheck)

;; enable for req/response communication with the server
(setq lsp-log-io t)

(setq buttercup-stack-frame-style 'pretty)

(setq lsp-enable-snippet nil)

(global-auto-revert-mode t)

(defun lsp--java-wait-for (what pred)
  (while (not (funcall pred))
    (print (concat "waiting for ..." what))
    (sleep-for 1)))

(defun print-body (method id body type &optional process-time)
  (let ((entry (make-lsp--log-entry
                :timestamp (format-time-string "%I:%M:%S %p")
                :process-time process-time
                :method method
                :id id
                :type type
                :body body)))
    (setq json-encoding-pretty-print t)
    (print (concat (format "Type %s - %s [%s] \n" type method id)
                   (json-encode body)
                   "\n"))
    entry))

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
                            (lambda () (not (-some--> #'lsp--client-download-in-progress? (lsp--filter-clients it))))))))

  (before-each
   (lsp)
   (lsp--java-wait-for "server ready"
                       (lambda () (--some? (eq (lsp--workspace-status it) 'initialized)
                                           (lsp--session-workspaces (lsp-session)))))

   (spy-on 'lsp--make-log-entry :and-call-fake 'print-body)
   (spy-on 'lsp-request :and-call-through))

  (it "add a invalid statement"
      (insert "new ArrayList()" )
      (sleep-for 4)
      (expect (length (lsp-cur-line-diagnostics)) :to-equal 1))

  (it "add a invalid import"
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
    (it "fetch source paths only once"
        (let ((sources (lsp-java-test-get-source-path)))
          (expect (length sources) :to-equal 2)
          (let ((sources-second-time (lsp-java-test-get-source-path)))
            (expect sources :to-equal sources-second-time)))
        (expect (spy-calls-count 'lsp-request) :to-equal 1))

    (it "check if a filename is not a test"
        (expect (lsp-java--test-is-a-test ) :not :to-be-truthy))

    (it "go to test even and create when does not exist"
        (spy-on 'yes-or-no-p :and-return-value t)


        (delete-file "src/test/java/simple/java/AppTest.java")

        (lsp-java-go-to-test)
        (expect (lsp-java--test-is-a-test ) :to-be-truthy))

    (xit "generate a new test "
         (spy-on 'yes-or-no-p :and-return-value t)

         (find-file "src/main/java/simple/java/App.java")
         (delete-file "src/test/java/simple/java/AppTest.java")

         (let ((new-file (lsp-java-generate-test)))
           (find-file new-file)
           (expect (buffer-file-name) :to-match ".*/src/test/java/simple/java/AppTest.java$")
           (expect (lsp-java--test-is-a-test ) :to-be-truthy)
           (print (buffer-string))))

    (it "go to implementation and go back to test"
         (lsp-java-go-to-test)
         (expect (lsp-java--test-is-a-test ) :not :to-be-truthy)

         (lsp-java-go-to-test)
         (expect (lsp-java--test-is-a-test ) :to-be-truthy))))







