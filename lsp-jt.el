;;; lsp-jt.el --- Java test support -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Version: 2.0
;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Package-Requires: ((emacs "25.1") (lsp-mode "6.0"))
;; Keywords: language, tools
;; URL: https://github.com/emacs-lsp/lsp-java

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Emacs frontend of https://github.com/Microsoft/vscode-java-test

;;; Code:

(require 'cl-lib)
(require 'lsp-mode)
(require 'lsp-java)
(require 'treemacs)

(eval-when-compile
  (require 'lsp-treemacs))

(declare-function dap-debug "ext:dap-mode")

(defvar lsp-jt--refresh-timer nil)

(defconst lsp-jt-kind-root 0)
(defconst lsp-jt-kind-workspace 1)
(defconst lsp-jt-kind-workspace-folder 2)
(defconst lsp-jt-kind-project 3)
(defconst lsp-jt-kind-package 4)
(defconst lsp-jt-kind-class 5)
(defconst lsp-jt-kind-method 6)

(defvar lsp-jt-results (ht))

(defun lsp-jt-browser--scedule-refresh ()
  (when lsp-jt--refresh-timer
    (cancel-timer lsp-jt--refresh-timer))
  (setq lsp-jt--refresh-timer (run-with-idle-timer 0.5 nil 'lsp-jt-browser-refresh)))

(defcustom lsp-jt-status-updated-hook nil
  "List of functions to be called after test status has changed."
  :type 'hook
  :group 'lsp-java)

(defcustom lsp-jt-test-run-finished-hook nil
  "List of functions to be called after all tests have finished."
  :type 'hook
  :group 'lsp-java)

(defcustom lsp-jt-theme "Default"
  "The `lsp-jt' theme."
  :type 'string
  :group 'lsp-java)

(defcustom lsp-jt-root (expand-file-name (locate-user-emacs-file "eclipse.jdt.ls/server/java-test/server"))
  "The `lsp-jt' root."
  :type 'string
  :group 'lsp-java)

(eval-and-compile
  (lsp-interface
   (jt:TestItem (:id :label :fullName :children :testLevel :testKind :projectName :uri :range :jdtHandler))
   (jt:Argument (:projectName :testNames :testLevel :testKind :uniqueId))
   (jt:JUnitLaunchArguments (:workingDirectory :mainClass :projectName :classpath :modulepath :vmArguments :programArguments))))

(defconst lsp-jt-test-kind-none -1)
(defconst lsp-jt-test-kind-junit 0)
(defconst lsp-jt-test-kind-junit5 1)
(defconst lsp-jt-test-kind-testng 2)

(defvar lsp-jt-browser-position-params
  `((side . ,treemacs-position)
    (slot . 4)
    (window-width . ,treemacs-width)))

(defun lsp-jt--process-test-lens (lens)
  (-let [(test-data &as &jt:TestItem :range ) lens]
    (-doto test-data
      (lsp-put :range range))))

(defface lsp-jt-error-face
  '((t :height 1.0 :inherit error))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defface lsp-jt-success-face
  '((t :height 1.0 :inherit success))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defface lsp-jt-in-progress-face
  '((t :height 1.0 :inherit warn))
  "The face used for code lens overlays."
  :group 'lsp-faces)

(defvar-local lsp-jt--last-callback nil)

(lsp-defun lsp-jt--junit-arguments ((node &as &jt:TestItem :id :project-name :test-level :test-kind
                                          :jdt-handler))
  (lsp--send-execute-command
   "vscode.java.test.junit.argument"
   (vector
    (lsp--json-serialize
     (lsp-make-jt-argument
      :uniqueId ""
      :testLevel test-level
      :testNames (vector (if (eq test-level lsp-jt-kind-method) jdt-handler id))
      :test-kind (or test-kind lsp-jt-test-kind-junit)
      :projectName project-name)))))

(lsp-defun lsp-jt--find-test-method ((&jt:TestItem :test-level))
  (= test-level lsp-jt-kind-method))


(defun lsp-jt-run-method-at-point ()
  (interactive)
  (let* ((methods (lsp--send-execute-command
                  "vscode.java.test.findTestTypesAndMethods"
                  (vector (lsp--path-to-uri (buffer-file-name)))))
        (symbols-at-the-point (lsp--document-symbols->document-symbols-hierarchy methods (lsp--cur-position)))
        (test-method (or
                      (-first #'lsp-jt--find-test-method symbols-at-the-point)
                      (user-error "No test method found at the point"))))
    (lsp-jt--start-test test-method t)))


(defun lsp-jt-run-class ()
  (interactive)
  (lsp-jt--start-test
   (lsp-seq-first
    (lsp--send-execute-command
     "vscode.java.test.findTestTypesAndMethods"
     (vector (lsp--path-to-uri (buffer-file-name))))) t))


(lsp-defun lsp-jt--start-test ((node &as &jt:TestItem :id :test-kind :test-level :project-name) no-debug?)

  (when (= test-kind lsp-jt-test-kind-testng) (user-error "TestNG is not implemented!"))

  (lsp-java-with-jdtls
    (let* (
           (arguments (lsp-jt--junit-arguments node))
           (analyzer (lsp-jt--create-analyzer project-name lsp-jt-results))
           (launch-config (lsp-jt--create-launch-config arguments no-debug? analyzer
                           (lambda ()
                             ;; mark all pending/running in undefined status
                             (let (needs-update)
                               (mapc (-lambda ((test-result &as &plist :status))
                                       (when (or (eq status :running)
                                                 (eq status :pending))
                                         (plist-put test-result :status nil)
                                         (setq needs-update t)))
                                     (ht-values lsp-jt-results))
                               (when needs-update (run-hooks 'lsp-jt-status-updated-hook)))
                             (run-hooks 'lsp-jt-test-run-finished-hook)))))

      (when (eq test-level lsp-jt-kind-method)
        (puthash id (list :status :pending) lsp-jt-results))

      (dap-debug launch-config)
      (run-hooks 'lsp-jt-status-updated-hook))))



(defun lsp-jt-lens-backend (_modified? callback)
  (setq-local lsp-jt--last-callback callback)
  (lsp-request-async
   "workspace/executeCommand"
   (list :command "vscode.java.test.search.codelens"
         :arguments (vector (lsp--buffer-uri)))
   (lambda (result)
     (let* ((lenses (-map #'lsp-jt--process-test-lens result))
            (all-lenses
             (append
              (mapc
               (lambda (lens)
                 (lsp-put
                  lens
                  :command (lsp-make-command
                            :title "Debug"
                            :command (lambda ()
                                       (interactive)
                                       (lsp-jt--start-test lens nil)))))
               lenses)
              (mapc
               (lambda (lens)
                 (lsp-put
                  lens
                  :command (lsp-make-command
                            :title "Run"
                            :command (lambda ()
                                       (interactive)
                                       (lsp-jt--start-test lens t)))))
               (-map #'lsp-copy lenses))
              (-keep
               (lambda (lens)
                 (-when-let ((title . face) (lsp-jt--status lens))
                   (lsp-put lens
                            :command (lsp-put
                                      (lsp-make-command
                                       :title title
                                       :command #'lsp-jt-report-open)
                                      :_face face))))
               (-map #'lsp-copy lenses)))))
       (funcall callback all-lenses lsp--cur-version)))
   :mode 'detached))

(defvar lsp-lens-backends)
(declare-function lsp-lens-refresh "lsp-lens" (buffer-modified? &optional buffer))
;;;###autoload
(define-minor-mode lsp-jt-lens-mode
  "Toggle code-lens overlays."
  :group 'lsp-jt
  :global nil
  :init-value nil
  :lighter nil
  (let ((buffer (current-buffer)))
    (cond
     (lsp-jt-lens-mode
      (require 'lsp-lens)
      (setq-local lsp-lens-backends (cl-pushnew 'lsp-jt-lens-backend lsp-lens-backends))
      (lsp-lens-refresh t)
      (add-hook 'lsp-jt-status-updated-hook
                (lambda ()
                  (with-current-buffer buffer
                    (lsp-lens-refresh nil)))))
     (t
      (setq-local lsp-lens-backends (delete 'lsp-jt-lens-backend lsp-lens-backends))
      (remove-hook 'lsp-jt-status-updated-hook
                   (lambda ()
                     (with-current-buffer buffer
                       (lsp-lens-refresh nil))))))))

(defun lsp-jt-search (root callback)
  (lsp-java-with-jdtls
    (let* ((response (lsp-request "workspace/executeCommand"
                                  (list :command "vscode.java.test.findJavaProjects"
                                        :arguments (vector root))))
           (jdt-handler-id (-> response
                               (-first-item)
                               (lsp-get :jdtHandler))))
      (lsp-request-async
       "workspace/executeCommand"
       (list :command "vscode.java.test.findTestPackagesAndTypes"
             :arguments (vector jdt-handler-id))
       callback))))

(defun lsp-jt-search-methods (jdt-handler-id callback)
  (lsp-java-with-jdtls
    (lsp-request-async
     "workspace/executeCommand"
     (list :command "vscode.java.test.findDirectTestChildrenForClass"
           :arguments (vector jdt-handler-id))
     callback)))

(defun lsp-jt-goto (&rest _)
  "Goto the symbol at point."
  (interactive)
  (-if-let ((&jt:TestItem :uri :range (&Range? :start)) (-some-> (treemacs-node-at-point)
                                                                                (button-get :data)))
      (progn
        (select-window (get-mru-window (selected-frame) nil :not-selected))
        (find-file (lsp--uri-to-path uri))
        (when start
          (goto-char (lsp--position-to-point start))))
    (user-error "No test under point.")))

(defun lsp-jt--roots ()
  (-uniq (gethash 'jdtls (lsp-session-server-id->folders (lsp-session)))))

(treemacs-modify-theme "Default"
  :icon-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode/")
  :config
  (progn
    (treemacs-create-icon :file "class.png" :extensions (java-test-class) :fallback "-")
    (treemacs-create-icon :file "debug.png" :extensions (java-test-debug) :fallback "-")
    (treemacs-create-icon :file "method.png" :extensions (java-test-method) :fallback "-")
    (treemacs-create-icon :file "package.png" :extensions (java-test-package) :fallback "-")
    (treemacs-create-icon :file "placeholder.png" :extensions (java-test-placeholder) :fallback "-")
    (treemacs-create-icon :file "refresh.png" :extensions (java-test-refresh) :fallback "-")
    (treemacs-create-icon :file "run.png" :extensions (java-test-run) :fallback "-")
    (treemacs-create-icon :file "running.png" :extensions (java-test-running) :fallback "-")
    (treemacs-create-icon :file "pass.png" :extensions (java-test-pass) :fallback "-")
    (treemacs-create-icon :file "test-error.png" :extensions (java-test-error) :fallback "-")
    (treemacs-create-icon :file "history.png" :extensions (java-test-pending) :fallback "-")))

(lsp-treemacs-define-action lsp-jt-run (:data)
  "Run test from browser."
  (lsp-jt--start-test data t))

(defvar lsp-jt-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "x") #'lsp-jt-run)
    (define-key (kbd "d") #'lsp-jt-debug)
    (define-key (kbd "R") #'lsp-jt-browser-refresh))
  "Keymap for `lsp-jt-mode'.")

(define-minor-mode lsp-jt-mode "Java Test Mode"
  :keymap lsp-jt-mode-map)

(lsp-treemacs-define-action lsp-jt-debug (:data)
  "Debug from browser."
  (lsp-jt--start-test data nil))

(defvar lsp-jt--refresh-lens-timer nil)

(defun lsp-jt--do-refresh-lenses ()
  (->>
   (lsp-find-workspace 'jdtls nil)
   (lsp--workspace-buffers)
   (-map (lambda (buffer)
           (with-current-buffer buffer
             (when (and lsp-jt-lens-mode lsp-jt--last-callback)
               (lsp-jt-lens-backend nil lsp-jt--last-callback)))))))

(defun lsp-jt--schedule-refresh-lens ()
  (when lsp-jt--refresh-lens-timer
    (cancel-timer lsp-jt--refresh-lens-timer))
  (setq lsp-jt--refresh-lens-timer
        (run-at-time 0.2 nil #'lsp-jt--do-refresh-lenses)))

(defun lsp-jt--create-launch-config (args no-debug? analyzer finished-function)
  (with-lsp-workspace (lsp-find-workspace 'jdtls nil)
    (require 'dap-java)
    (-let (((all &as &jt:JUnitLaunchArguments :working-directory :main-class
                 :project-name :classpath :vm-arguments :program-arguments) args)
           tcp-server-process)
      (list :type "java"
            :mainClass main-class
            :projectName project-name
            :noDebug no-debug?
            :debugOnEntry t
            :cwd working-directory
            :vmArgs (s-join " " vm-arguments)
            :classPaths classpath
            :startup-function
            (lambda (launch-args)
              (setf tcp-server-process
                    (make-network-process
                     :name "*java-tests-tcp-server*"
                     :buffer " *java-tests-tcp-server*"
                     :family 'ipv4
                     :service 0
                     :sentinel (lambda (proc _string)
                                 (set-process-filter
                                  proc
                                  (lambda (_ string)
                                    (funcall analyzer string))))
                     :server 't))
              (aset program-arguments 3 (number-to-string (cl-second (process-contact tcp-server-process))))
              (plist-put launch-args :args (s-join " " program-arguments)))
            :cleanup-function
            (lambda (_session)
              ;; cleanup after some time to ensure notifications over
              ;; tcp-server-process are received
              (run-with-idle-timer
               0.5
               nil
               (lambda ()
                 (when tcp-server-process
                   (delete-process tcp-server-process))
                 (funcall finished-function))))))))

(lsp-defun lsp-jt--render-test-node ((test-item &as &jt:TestItem :label :test-level :id :jdt-handler :uri :range :children))
  `(:key ,id
    :label ,label
    :icon ,(lsp-jt--get-test-icon id test-level)
    ,@(when (eq test-level 5)
        (list :children-async (lambda (_ callback)
                                (lsp-jt-search-methods
                                 jdt-handler
                                 (lambda (items)
                                   (funcall callback
                                            (-map
                                             #'lsp-jt--render-test-node
                                             items)))))))
    :children ,(-map #'lsp-jt--render-test-node children)
    :ret-action ,(lambda ()
                   (interactive)
                   (lsp-goto-location (ht->plist (ht (:uri uri) (:range range)))))
    :actions (["Run Test"   lsp-jt-run]
              ["Debug Test" lsp-jt-debug]
              ["Refresh"    lsp-jt-browser-refresh])
    :data ,test-item))

(declare-function lsp-treemacs-generic-refresh "lsp-treemacs" ())
(declare-function
 lsp-treemacs-render "lsp-treemacs"
 (tree title expand-depth &optional buffer-name right-click-actions))

(defun lsp-jt-browser-refresh ()
  (interactive)
  (with-current-buffer "*Java Tests*"
    (require 'lsp-treemacs)
    (lsp-treemacs-generic-refresh)))

;;;###autoload
(defun lsp-jt-browser ()
  (interactive)
  (require 'lsp-treemacs)
  (select-window
   (display-buffer-in-side-window
    (lsp-treemacs-render
     (-map
      (-lambda (root)
        (list :key root
              :label (f-filename root)
              :icon 'root
              :children-async (lambda (_ callback)
                                (lsp-jt-search (lsp--path-to-uri root) 
                                               (lambda (items)
                                                 (funcall callback
                                                          (-map
                                                           #'lsp-jt--render-test-node
                                                           items)))))
              :ret-action (lambda () (find-file root))
              :actions `(["Run Test"   lsp-jt-run]
                         ["Debug Test" lsp-jt-debug]
                         ["Refresh"    lsp-jt-browser-refresh])
              :data (lsp-make-jt-test-item :project (lsp--path-to-uri root)
                                           :level 1
                                           :location (lsp-make-location
                                                      :uri (lsp--path-to-uri root)))))
      (lsp-jt--roots)) "Java Test Browser" nil "*Java Tests*"
     `(["Refresh" lsp-jt-browser-refresh]))
    lsp-jt-browser-position-params))

  (add-hook 'lsp-jt-status-updated-hook #'lsp-jt-browser--scedule-refresh)
  (add-hook 'kill-buffer-hook (lambda ()
                                (remove-hook 'lsp-jt-status-updated-hook
                                             #'lsp-jt-browser--scedule-refresh))
            nil t)
  (lsp-jt-mode 1))

(lsp-defun lsp-jt--status ((&jt:TestItem :id :test-level))
  (if (eq test-level lsp-jt-kind-method)
      (pcase (plist-get (gethash id lsp-jt-results) :status)
        (:failed (cons "❌" 'lsp-jt-error-face))
        (:pass (cons "✔" 'lsp-jt-success-face))
        (:running (cons "⌛" 'lsp-jt-in-progress-face))
        (:pending (cons "⌚" 'lsp-jt-in-progress-face)))
    (pcase (plist-get (gethash id lsp-jt-results) :status)
      (:failed (cons "❌" 'lsp-jt-error-face))
      (:pass (cons "✔" 'lsp-jt-success-face))
      (:running (cons "⌛" 'lsp-jt-in-progress-face))
      (:pending (cons "⌚" 'lsp-jt-in-progress-face)))))

(defun lsp-jt--update-report ()
  (when (buffer-live-p (get-buffer "*Java Tests Results*"))
    (with-current-buffer (get-buffer "*Java Tests Results*")
      (lsp-jt-test-report-refresh))))

(defun lsp-jt--extract-name (item project)
  (or
   (-when-let ((_ _ name _ class) (s-match
                                   (rx
                                    (+ digit )
                                    ","
                                    (group (opt (or "@AssumptionFailure: " "@Ignore: ")))
                                    (group (*? any))
                                    (group (opt (*? digit)))
                                    "(" (group  (* any)) ")")
                                   item))
     (format "%s@%s#%s" project class name))
   (when-let ((class (last (s-split "," item))))
     (format "%s@$%s#<TestError>" project class))
   (lsp--warn "Unable to extract id from %s" item)
   ""))

(defvar lsp-jt--junit-test-start "%TESTS")
(defvar lsp-jt--junit-test-end "%TESTE")
(defvar lsp-jt--junit-test-failed "%FAILED")
(defvar lsp-jt--junit-test-error "%ERROR")
(defvar lsp-jt--junit-trace-start "%TRACES")
(defvar lsp-jt--junit-trace-end "%TRACEE")
(defvar lsp-jt--junit-ignore-test-prefix "@Ignore: ")
(defvar lsp-jt--junit-assumption-failed-test-prefix "@AssumptionFailure: ")

(lsp-defun lsp-jt--create-analyzer (project result)
  (let (recording-traces traces chunk parts)
    (lambda (item)
      (setf chunk (concat chunk item)
            parts (s-lines chunk)
            chunk (cl-first (last parts)))
      (mapc
       (lambda (item)
         (cond
          ((s-starts-with? lsp-jt--junit-test-start item)
           (-let* ((test-id (lsp-jt--extract-name item project))
                   ((&plist :status old-status :traces old-traces) (gethash test-id lsp-jt-results)))
             (puthash test-id
                      (list :start (time-to-seconds (current-time))
                            :status (or
                                     ;; parameterized test - keep failed status
                                     (when (eq old-status :failed) :failed)
                                     ;; skipped
                                     (when (s-contains? lsp-jt--junit-ignore-test-prefix
                                                        item)
                                       :skipped)
                                     :running)
                            :traces old-traces)
                      result))
           (run-hooks 'lsp-jt-status-updated-hook))
          ((s-starts-with? lsp-jt--junit-trace-start item)
           (setf recording-traces t))
          ((s-starts-with? lsp-jt--junit-trace-end item))
          ((s-starts-with? lsp-jt--junit-test-end item)
           (-when-let ((test-data &as &plist :start :status) (gethash
                                                              (lsp-jt--extract-name item project)
                                                              result))
             (-> test-data
                 (plist-put :duration (- (time-to-seconds (current-time))
                                         start))
                 (plist-put :status (if (or (eq status :running)
                                            (null status))
                                        :pass
                                      status))
                 (plist-put :traces (when traces (s-join "\n" (nreverse traces)))))
             (setf recording-traces nil
                   traces nil)
             (run-hooks 'lsp-jt-status-updated-hook)))
          ((or (s-starts-with? lsp-jt--junit-test-failed item)
               (s-starts-with? lsp-jt--junit-test-error item))
           (-when-let ((test-data &as &plist :start) (gethash
                                                      (lsp-jt--extract-name item project)
                                                      result))
             (-> test-data
                 (plist-put :duration (- (time-to-seconds (current-time))
                                         start))
                 (plist-put :status
                            (if (s-contains? lsp-jt--junit-assumption-failed-test-prefix item)
                                :skipped
                              :failed)))
             (run-hooks 'lsp-jt-status-updated-hook)))
          (recording-traces (push item traces))))
       (butlast parts)))))

(defun lsp-jt--get-test-status (id)
  (plist-get (gethash id lsp-jt-results) :status))

(defun lsp-jt--get-test-icon (id level)
  (cl-case (lsp-jt--get-test-status id)
    (:running 'java-test-running)
    (:pass 'java-test-pass)
    (:failed 'java-test-error)
    (:pending 'java-test-pending)
    (t (alist-get level '((lsp-jt-kind-root . java-test-package)
                          (lsp-jt-kind-workspace . java-test-package)
                          (lsp-jt-kind-workspace-folder . java-test-package)
                          (lsp-jt-kind-project . java-test-package)
                          (lsp-jt-kind-package . java-test-package)
                          (lsp-jt-kind-class . java-test-class)
                          (lsp-jt-kind-method . java-test-method))))))

(define-derived-mode lsp-jt-test-result-tab-mode tabulated-list-mode "lsp-jt-test-result-tab-mode"
  "Major mode for tabulated list example."
  (setq tabulated-list-format [("Time" 10 t)
                               ("Status" 10 t)
                               ("Duration (seconds)"  10 t)
                               ("Name" 30 t)
                               ("Class"  50 t)
                               ("Stack-Trace" 0 nil)])
  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key (cons "Time" nil)))


(defun lsp-jt--open-stack-trace (button)
  (with-help-window "**Stack Trace**"
    (princ
     (button-get button 'long-trace))))

(defun lsp-jt--create-stack-trace-button (stack-trace)
  `(,(concat (substring stack-trace 0 30) "...")
   face link
   help-echo "View stack-trace"
   follow-link t
   long-trace ,stack-trace
   action lsp-jt--open-stack-trace))


(defun lsp-jt--test-report ()
  (interactive)
  (let ((buffer (get-buffer-create "**LSP - Java Test Results**")))
    (with-current-buffer buffer
      (lsp-jt-test-result-tab-mode)
      (setq tabulated-list-entries nil)

      (->> lsp-jt-results
           (ht-keys)
           (-map
            (lambda (test-id)

              (-let* (((_ _ test name)
                       (s-match (rx (group (* any)) "@"
                                    (group (* any)) "#"
                                    (group (* any)))
                                test-id))
                      (class-name test)
                      (status (if (eq (lsp-jt--get-test-status test-id) :failed) "❌" "✅"))
                      (start-time (plist-get (ht-get lsp-jt-results test-id) :start))
                      (start-time (propertize (format-time-string "%FT%T%z" start-time) 'face 'lsp-lens-face))
                      (duration (plist-get (ht-get lsp-jt-results test-id) :duration))
                      (duration (or (when duration (propertize (format "%0.2fs" duration) 'face 'lsp-lens-face)) ""))
                      (stack-trace (plist-get (gethash test-id lsp-jt-results) :traces))
                      (stack-trace (if stack-trace (lsp-jt--create-stack-trace-button stack-trace) "")))


                (push (list test-id (vector start-time status duration name class-name stack-trace))
		              tabulated-list-entries)))))

      (tabulated-list-init-header)
      (tabulated-list-print))
    (switch-to-buffer-other-window buffer)))

(defun lsp-jt-test-report-refresh ()
  (lsp-treemacs-render
   (->> lsp-jt-results
        (ht-keys)
        (-sort #'string-lessp)
        (-map
         (lambda (test-id)
           (-let (((_ _ test name)
                   (s-match (rx (group (* any)) "@"
                                (group (* any)) "#"
                                (group (* any)))
                            test-id)))
             `(:key ,test-id
                    :label
                    ,(format "%s %s     %s"
                             name
                             (propertize test 'face 'lsp-lens-face)
                             (or (when-let ((duration (plist-get (ht-get lsp-jt-results test-id)
                                                                 :duration)))
                                   (propertize (format "%0.2fs" duration)
                                               'face 'lsp-lens-face))
                                 ""))

                    :icon ,(lsp-jt--get-test-icon test-id 4)
                    ,@(when (eq (lsp-jt--get-test-status test-id) :failed)
                        `(:children ((:key "stacktrace"
                                           :label ,(concat "Stack trace: \n"
                                                           (plist-get
                                                            (gethash test-id lsp-jt-results)
                                                            :traces))
                                           :icon none)))))))))
   "Java Test Results" nil "*Java Tests Results*"))
(defun lsp-jt--report-buffer-hook ()
  (remove-hook 'lsp-jt-status-updated-hook #'lsp-jt--update-report))

(defun lsp-jt-report-open ()
  (interactive)
  (pop-to-buffer (lsp-jt-test-report-refresh))
  (add-hook 'lsp-jt-status-updated-hook #'lsp-jt--update-report)
  (add-hook 'kill-buffer-hook #'lsp-jt--report-buffer-hook nil t))

(provide 'lsp-jt)
;;; lsp-jt.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
