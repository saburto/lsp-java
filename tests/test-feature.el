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

(defun lsp--java-wait-for (what pred &optional max-attemps)
  (setq try-count 1)
  (while (not (funcall pred))
    (when (> try-count (or max-attemps 60))
      (user-error (format "waiting for %s more than %s seconds!" what max-attemps)))
    (setq try-count (+ 1 try-count))
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

(defun expect-error-on-current-line (error-msg)
  (lsp--java-wait-for "errors"
                      (lambda ()
                        (> (length (lsp-cur-line-diagnostics)) 0)))
  (expect (length (lsp-cur-line-diagnostics)) :to-equal 1)
  (let ((error-msg (-> (lsp-cur-line-diagnostics)
                       (aref 0)
                       (ht-get "message"))))
    (expect error-msg :to-match error-msg)))

(describe "lsp-java with a simple java project"

  (before-all

   (setq simple-java-folder (f-join default-directory "tests/resources/simple-java"))
   (setq target-java-folder (f-join temporary-file-directory "lsp-java-test"))
   (when (f-exists? target-java-folder)
     (f-delete target-java-folder t))
   (f-copy simple-java-folder target-java-folder)

   (cd target-java-folder)
   (find-file "src/main/java/simple/java/App.java")
   (setq test-app-class (buffer-file-name))

   (lsp-workspace-folders-add target-java-folder)

   (if (f-exists? (expand-file-name
                   (locate-user-emacs-file (f-join ".cache" "lsp" "eclipse.jdt.ls"))))
       (print "Already installed")
     (progn
       (lsp-install-server nil 'jdtls)
       (lsp--java-wait-for "download jdt.ls server"
                           (lambda () (not (-some--> #'lsp--client-download-in-progress? (lsp--filter-clients it))))))))

  (before-each
   (lsp)
   (lsp--java-wait-for "server ready"
                       (lambda () (--some? (eq (lsp--workspace-status it) 'initialized)
                                           (lsp--session-workspaces (lsp-session)))))

   (spy-on 'lsp--make-log-entry :and-call-fake 'print-body)
   (spy-on 'lsp-request :and-call-through))

  (xit "add a invalid statement"
      (insert "new ArrayList()" )
      (expect-error-on-current-line "Syntax error"))

  (it "ok"
      (spy-on 'yes-or-no-p :and-return-value t)
      (lsp-java-go-to-test)
      (forward-line)
      (insert "import org.junit.jupiter.api.Test;")
      (end-of-buffer)
      (forward-line -1)
      (insert "@Test void this_is_a_test_number_2() { //nothing }")
      (save-buffer)
      (lsp)
      (sleep-for 10)
      (print (lsp-java-find-java-projects)))

  (xit "add a invalid import"
      (forward-line)
      (insert "import ok.ok.not.found;")
      (expect-error-on-current-line "import.*cannot to be resolved"))

  (describe "using tests"
    (xit "fetch source paths only once"
        (let ((sources (lsp-java-test-get-source-path)))
          (expect (length sources) :to-equal 2)
          (let ((sources-second-time (lsp-java-test-get-source-path)))
            (expect sources :to-equal sources-second-time)))
        (expect (spy-calls-count 'lsp-request) :to-equal 1))

    (xit "check if a filename is not a test"
        (expect (lsp-java--test-is-a-test ) :not :to-be-truthy))

    (xit "go to test even and create when does not exist"
        (spy-on 'yes-or-no-p :and-return-value t)

        (lsp-java-go-to-test)
        (expect (lsp-java--test-is-a-test ) :to-be-truthy)

        (expect (buffer-string) :to-match "class AppTest")

        (save-buffer)
        (save-current-buffer)
        )

    (xit "go to implementation and go back to test"
        (spy-on 'yes-or-no-p :and-return-value t)

        (find-file test-app-class)

        (lsp-java-go-to-test)
        (expect (lsp-java--test-is-a-test ) :to-be-truthy)

        (lsp-java-go-to-test)
        (expect (lsp-java--test-is-a-test ) :not :to-be-truthy))))







