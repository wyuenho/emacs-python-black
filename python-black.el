;;; python-black.el --- Reformat Python using python-black -*- lexical-binding: t; -*-

;; Author: wouter bolsterlee <wouter@bolsterl.ee>
;; Keywords: languages
;; URL: https://github.com/wbolster/emacs-python-black
;; Package-Requires: ((emacs "26.1") (dash "2.16.0") (reformatter "0.3") (request "0.3.2"))
;; Version: 1.0.0

;; Copyright 2019 wouter bolsterlee. Licensed under the 3-Clause BSD License.

;;; Commentary:

;; Commands for reformatting Python code via black (and black-macchiato).

;;; Code:

(require 'ansi-color)
(require 'cl-lib)
(require 'dash)
(require 'python)
(require 'reformatter)
(require 'request)
(require 'rx)

(defgroup python-black nil
  "Python reformatting using black."
  :group 'python
  :prefix "python-black-")

(defcustom python-black-command "black"
  "Name of the ‘black’ executable."
  :group 'python-black
  :type 'string)

(defcustom python-black-macchiato-command "black-macchiato"
  "Name of the ‘black-macchiato’ executable."
  :group 'python-black
  :type 'string)

(defvar python-black--base-args '("--quiet")
  "Base arguments to pass to black.")

(defcustom python-black-extra-args nil
  "Extra arguments to pass to black."
  :group 'python-black
  :type '(repeat string))

(defconst python-black--config-file "pyproject.toml")
(defconst python-black--config-file-marker-regex (rx bol "[tool.black]" eol))

(defcustom python-black-d-command "blackd"
  "Name of the `blackd' executable."
  :group 'python-black
  :type 'string)

(defcustom python-black-d-host "localhost"
  "Host of the `blackd' server."
  :group 'python-black
  :type 'string)

(defcustom python-black-d-port 45484
  "Port of the `blackd' server."
  :group 'python-black
  :type 'integer)

(defcustom python-black-d-request-timeout 1
  "Timeout for requests to `blackd' in seconds."
  :group 'python-black
  :type 'integer)

(defcustom python-black-d-request-headers-function 'python-black-d-default-request-headers-function
  "Function that returns `blackd' request headers.

When set, the function value must accept no argument and return an
alist of request headers or nil.  When this function is invoked,
the current buffer is the buffer being formatted."
  :group 'python-black
  :type '(choice function (const :tag "None" nil)))

(defvar python-black-d-process nil
  "The `blackd' process object.")

(defvar python-black-d-server-ready nil
  "Whether the `blackd' server is ready to accept requests.")

(defconst python-black-d-process-buffer-name "*blackd*"
  "The name of the `blackd' process buffer.")

(defvar python-black-d-task-queue nil
  "Queue for pending formatting operations while the server is starting up.")

(defun python-black-d-clear-queue ()
  "Clear `python-black-d-task-queue'."
  (setf python-black-d-task-queue nil))

(defun python-black-d-push-queue (op)
  "Enqueue a new opertaion OP to `python-black-d-task-queue'."
  (push op python-black-d-task-queue))

(defun python-black-d-process-task-queue ()
  "Process all pending formatting operations.

Duplicate operations are not repeated and only the first one will
be executed."
  (when python-black-d-task-queue
    (run-hooks (delete-dups (reverse python-black-d-task-queue)))
    (python-black-d-clear-queue)))

(defun python-black-d--clean-up ()
  "Internal function to clean up state variables."
  (python-black-d-clear-queue)
  (setf python-black-d-process nil
        python-black-d-server-ready nil))

;;;###autoload
(defun python-black-on-save-mode-enable-dwim ()
  "Enable ‘python-black-on-save-mode’ if appropriate."
  (interactive)
  (-when-let* ((file-name (buffer-file-name))
               (uses-black? (python-black--in-blackened-project-p file-name))
               (not-third-party? (not (python-black--third-party-file-p file-name))))
    (python-black-on-save-mode)))

;;;###autoload
(defun python-black-statement (&optional display-errors)
  "Reformats the current statement.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails."
  (interactive "p")
  (-when-let* ((beg (save-excursion
                      (python-nav-beginning-of-statement)
                      (line-beginning-position)))
               (end (save-excursion
                      (python-nav-end-of-statement)
                      (line-end-position)))
               (non-empty? (not (= beg end))))
    (python-black-region beg (min (point-max) (1+ end)) display-errors)))

;;;###autoload
(defun python-black-partial-dwim (&optional display-errors)
  "Reformats the active region or the current statement.

This runs ‘python-black-region’ or ‘python-black-statement’ depending
on whether the region is currently active.

When called interactively with a prefix argument, or when
DISPLAY-ERRORS is non-nil, shows a buffer if the formatting fails."
  (interactive "p")
  (if (region-active-p)
      (python-black-region (region-beginning) (region-end) display-errors)
    (python-black-statement display-errors)))

(defun python-black--command (beg end)
  "Helper to decide which command to run for span BEG to END."
  (if (python-black--whole-buffer-p beg end)
      python-black-command
    (unless (executable-find python-black-macchiato-command)
      (error "Partial formatting requires ‘%s’, but it is not installed"
             python-black-macchiato-command))
    python-black-macchiato-command))

(defun python-black--make-args (beg end)
  "Helper to build the argument list for black for span BEG to END."
  (append
   python-black--base-args
   (-when-let* ((file-name (buffer-file-name))
                (extension (file-name-extension file-name))
                (is-pyi-file (string-equal "pyi" extension)))
     '("--pyi"))
   python-black-extra-args
   (when (python-black--whole-buffer-p beg end)
     '("-"))))

(defun python-black--whole-buffer-p (beg end)
  "Return whether BEG and END span the whole buffer."
  (and (= (point-min) beg)
       (= (point-max) end)))

(defun python-black--in-blackened-project-p (file-name)
  "Determine whether FILE-NAME resides in a project that is using Black.

This looks for ‘[tool.black]’ in a ‘pyproject.toml’ file."
  (-when-let* ((project-directory (locate-dominating-file file-name python-black--config-file))
               (config-file (concat project-directory python-black--config-file))
               (config-file-contains-marker
                (with-temp-buffer
                  (insert-file-contents-literally config-file)
                  (re-search-forward python-black--config-file-marker-regex nil t 1))))
    t))

(defun python-black--third-party-file-p (file-name)
  "Determine whether FILE-NAME is likely a third party file."
  (-when-let* ((lib-python-dir (locate-dominating-file file-name "site-packages")))
    t))

(defun python-black-d-process-buffer ()
  "Process buffer for `python-black-d-process'."
  (or (get-buffer python-black-d-process-buffer-name)
      (with-current-buffer
          (get-buffer-create python-black-d-process-buffer-name)
        (special-mode)
        (read-only-mode 1)
        (current-buffer))))

(defun python-black-d-format-buffer (buffer success failure)
  "Request `blackd' to format BUFFER.

SUCCESS will be called when the server returns a formatted string
or if the buffer is already formatted.  FAILURE will be called is
the file consists of syntax errors or if an internal failure
occured on the server."
  (request
    (format "http://%s:%s" python-black-d-host python-black-d-port)
    :sync t
    :type "POST"
    :headers (and (functionp python-black-d-request-headers-function)
                  (with-current-buffer buffer
                    (funcall python-black-d-request-headers-function)))
    :data (buffer-string)
    :parser 'buffer-string
    :status-code `((200 . ,(cl-function
                            (lambda (&key data &allow-other-keys)
                              (let ((temp-file (reformatter-temp-file-in-current-directory nil data)))
                                (unwind-protect
                                    (with-current-buffer buffer
                                      (save-restriction
                                        (narrow-to-region (point-min) (point-max))
                                        (reformatter-replace-buffer-contents-from-file temp-file)))
                                  (funcall success "Buffer formatted")
                                  (delete-file temp-file))))))
                   (204 . ,(lambda (&rest _)
                             (funcall success "Buffer already formatted")))
                   (400 . ,(cl-function
                            (lambda (&key data &allow-other-keys)
                              (funcall failure (format "Syntax Error: %s" data)))))
                   (500 . ,(cl-function
                            (lambda (&key data &allow-other-keys)
                              (funcall failure (format "Error: %s" (car data)))))))))

(defun python-black-d-poll-until-ready (success failure)
  "Poll the `blackd' server every second until it is ready to format.

SUCCESS will be called when the server is ready.  If the server
returns an error during startup other than a timeout, this
function will stop polling and FAILURE is called with the error
message returned."
  (request
    (format "http://%s:%s" python-black-d-host python-black-d-port)
    :type "POST"
    :data "print('valid')"
    :parser 'buffer-string
    :timeout python-black-d-request-timeout
    :error (cl-function
            (lambda (&key data symbol-status &allow-other-keys)
              (if (eq symbol-status 'timeout)
                  (run-at-time python-black-d-request-timeout
                               nil
                               'python-black-d-poll-until-ready
                               success
                               failure)
                (funcall failure (format "`blackd' server error: %s" data)))))
    :status-code `((200 . ,(lambda (&rest _) (funcall success))))))

(defun python-black-d-start-server ()
  "Start the `blackd' server process.

If there are pending operations in `python-black-d-task-queue',
the operations will be executed in first-in-first-out order as
soon as the server is ready to accept requests."
  (unless python-black-d-process
    (setf python-black-d-process
          (make-process
           :name "blackd"
           :buffer (python-black-d-process-buffer)
           :filter (lambda (process string)
                     (when (buffer-live-p (process-buffer process))
                       (with-current-buffer (process-buffer process)
                         (let ((inhibit-read-only t)
                               (moving (= (point) (process-mark process))))
                           (save-excursion
                             (goto-char (process-mark process))
                             (insert (ansi-color-apply string))
                             (set-marker (process-mark process) (point)))
                           (if moving (goto-char (process-mark process)))))))
           :command (list python-black-d-command
                          "--bind-host" python-black-d-host
                          "--bind-port" (format "%s" python-black-d-port))
           :sentinel (lambda (process _)
                       (when (eq (process-status process) 'exit)
                         (when (buffer-live-p (process-buffer process))
                           (kill-buffer (python-black-d-process-buffer)))
                         (python-black-d--clean-up)
                         (message "blackd server has exited")))))

    (if (eq (process-status python-black-d-process) 'run)
        (progn
          (sit-for 2)
          (python-black-d-poll-until-ready
           (lambda ()
             (setf python-black-d-server-ready t)
             (message "blackd server ready")
             (python-black-d-process-task-queue))
           (lambda (err-msg)
             (python-black-d--clean-up)
             (message err-msg))))
      (python-black-d--clean-up)
      (message "Unable to start blackd process"))))

(defun python-black-d-stop-server ()
  "Stop the `blackd' server process."
  (when (process-live-p python-black-d-process)
    (kill-process python-black-d-process))
  (when (and python-black-d-process
             (buffer-live-p (process-buffer python-black-d-process)))
    (let ((kill-buffer-query-functions))
      (kill-buffer (process-buffer python-black-d-process))))
  (python-black-d--clean-up))

;;;###autoload
(defun python-black-d-restart-server ()
  "Restart the `blackd' server process."
  (interactive)
  (python-black-d-stop-server)
  (python-black-d-start-server))

;;;###autoload
(defun python-black-d-buffer ()
  "Reformats the current buffer with `blackd'."
  (interactive)
  (if python-black-d-server-ready
      (python-black-d-format-buffer (current-buffer) 'message 'message)
    (cond ((and (not (process-live-p python-black-d-process))
                (y-or-n-p "The blackd server process has not started, would you like to start it? "))
           (python-black-d-push-queue 'python-black-d-buffer)
           (python-black-d-start-server))
          (t (python-black-d-push-queue 'python-black-d-buffer)))))

;;;###autoload (autoload 'python-black-buffer "python-black" nil t)
;;;###autoload (autoload 'python-black-region "python-black" nil t)
;;;###autoload (autoload 'python-black-on-save-mode "python-black" nil t)
(reformatter-define python-black
  :program (python-black--command beg end)
  :args (python-black--make-args beg end)
  :lighter " BlackFMT"
  :group 'python-black)

(defun python-black-d-toggle ()
  "Set up `blackd' if possible.

If `python-black-on-save-mode' is non-nil, and
`python-black-d-command' is found, replace `python-black-buffer'
with `python-black-d-buffer' in the buffer-local
`before-save-hook'."
  (when (and python-black-d-command (executable-find python-black-d-command))
    (if python-black-on-save-mode
        (progn
          (python-black-d-start-server)
          (remove-hook 'before-save-hook 'python-black-buffer t)
          (add-hook 'before-save-hook 'python-black-d-buffer nil t))
      (python-black-d-stop-server)
      (remove-hook 'before-save-hook 'python-black-d-buffer t))))

(add-hook 'python-black-on-save-mode-hook 'python-black-d-toggle)

(provide 'python-black)
;;; python-black.el ends here
