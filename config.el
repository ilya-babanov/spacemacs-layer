;; enable linum-mode and scroll-margin in programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (progn
                              (linum-mode t)
                              (my-set-scroll-margin 17))))

;; disable scroll-margin in term mode
(add-hook 'term-mode-hook (lambda () (my-set-scroll-margin 0)))

;; do not wrap files by default
(setq-default truncate-lines 0)

;; configure org mode
(setq org-agenda-files '("~/Yandex.Disk.localized/org"))
(setq org-log-done t)
(setq org-agenda-span 40)
(setq org-agenda-start-day "-20d")
(setq org-capture-templates
      '(("p" "Projects" entry (file+headline "~/Yandex.Disk.localized/org/projects.org" "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("l" "LightSide" entry (file+headline "~/Yandex.Disk.localized/org/lightside.org" "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("h" "Home" entry (file+headline "~/Yandex.Disk.localized/org/home.org" "Tasks")
         "** TODO %?\n  %i\n  %a")
        ("w" "Work" entry (file+headline "~/Yandex.Disk.localized/org/work.org" "Tasks")
         "** TODO %?\n  %i\n  %a")
        ))

;; Makes scroll-margin var 'buffer-local' and sets variable to it
(defun my-set-scroll-margin (param)
  (progn
    (make-variable-buffer-local 'scroll-margin)
    (setq scroll-margin param)))

;; Deletes tern process (after that, tern restarts automatically)
(defun delete-tern-process ()
  (interactive)
  (delete-process "Tern"))

(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker jsxhint-checker
                              "A JSX syntax and style checker based on JSXHint."
                              :command ("jsxhint" source)
                              :error-patterns
                              ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
                              :modes (js-mode js2-mode js3-mode))
     (add-to-list 'flycheck-checkers 'jsxhint-checker)

     (flycheck-def-config-file-var flycheck-jscs javascript-jscs ".jscsr"
       :safe #'stringp)
     (flycheck-define-checker javascript-jscs
                              "A jscs code style checker."
                              :command ("jscs" "--reporter" "checkstyle" "--esnext"
                                        (config-file "--config" flycheck-jscs) source)
                              :error-parser flycheck-parse-checkstyle
                              :modes (js-mode js2-mode js3-mode))
     (add-to-list 'flycheck-checkers 'javascript-jscs)))

(eval-after-load 'projectile
  '(progn
     (add-to-list 'projectile-globally-ignored-directories "node_modules")))

(eval-after-load 'shell-pop
  '(progn
    (setq-default shell-pop-autocd-to-working-dir nil)
    (setq-default shell-pop-window-height 60)))

(defun core-grunt-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((core-close-after-success t))
    (core-try-start-process "grunt test")))

(defun core-npm-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((core-close-after-success t))
    (core-try-start-process "npm run test")))

(defun core-grunt-build ()
  "Invokes grunt buil and shows output"
  (interactive)
  (let* ((core-poll-timout 3)
         (core-close-after-success t))
    (core-try-start-process "grunt build")))

(defun core-test ()
  "Test function"
  (interactive)
  (let* ((core-open-after-error nil)
         (core-erase-process-buffer nil))
    (core-try-start-process "grunt test")))

(defvar core-close-after-success nil)
(defvar core-open-after-error t)
(defvar core-erase-process-buffer t)
(defvar core-process-mode 'shell-mode)
(defvar core-show-progress t)
(defvar core-poll-timout 0.2)

(defun core-try-start-process (cmd)
  "Invokes passed command in background"
  (interactive "sCommand:")
  (let* ((process (get-process cmd)))
    (if process
        (progn
          (message "Process already exist: %s" process)
          (core-try-refresh-process-window process))
      (core-run-process cmd))))

(defun core-run-process (cmd)
  (message "Running process '%s'" cmd)
  (let* ((buffer-name (concat "*" cmd "*"))
         (buffer (get-buffer-create buffer-name))
         (process (start-process-shell-command cmd buffer cmd)))
    (set-process-plist process (core-create-process-plist))
    (set-process-sentinel process 'core-handle-result)
    (core-handle-progress process)
    (core-config-process-buffer buffer)))

(defun core-create-process-plist ()
  (list 'poll-timeout core-poll-timout
        'close-after-success core-close-after-success
        'open-after-error core-open-after-error
        'show-progress core-show-progress
        'start-time (float-time)))

(defun core-config-process-buffer (buffer)
  (with-current-buffer buffer
    (if core-erase-process-buffer (erase-buffer))
    (funcall core-process-mode)))

(defun core-handle-progress (process)
  (if (process-live-p process)
      (let* ((show-progress (process-get process 'show-progress)))
        (if show-progress (core-show-progress-message process))
        (core-delay-progress-handler process))))

(defun core-delay-progress-handler (process)
  (let* ((poll-timeout (process-get process 'poll-timeout)))
    (run-at-time poll-timeout nil 'core-handle-progress process)))

(defun core-handle-result (process &optional event)
  (if (not (process-live-p process))
      (let* ((exit-code (process-exit-status process)))
        (core-refresh-process-buffer process)
        (if (= exit-code 0)
            (core-handle-success process)
          (core-handle-error process exit-code)))))

(defun core-handle-success (process)
  (core-show-success-message process)
  (let* ((buffer-window (core-get-process-window process))
         (close-after-success (process-get process 'close-after-success)))
    (if (and buffer-window close-after-success)
        (delete-window buffer-window))))

(defun core-handle-error (process exit-code)
  (core-show-error-message process exit-code)
  (let* ((buffer (process-buffer process))
         (buffer-window (get-buffer-window buffer))
         (open-after-error (process-get process 'open-after-error)))
    (if (and open-after-error (not buffer-window))
        (progn
          (setq buffer-window (split-window-vertically))
          (set-window-buffer buffer-window buffer)))
    (core-try-refresh-process-window process)))

(defun core-show-progress-message (process)
  (let* ((time-diff (core-get-process-time-diff process)))
    (message "%fs | %s" time-diff process)))

(defun core-show-success-message (process)
  (message "Success\nProcess: %s\nTime: %f"
           process (core-get-process-time-diff process)))

(defun core-show-error-message (process exit-code)
  (message "Error code: %s\nProcess: %s\nTime: %f"
           exit-code process (core-get-process-time-diff process)))

(defun core-get-process-time-diff (process)
  (let* ((start-time (process-get process 'start-time)))
    (- (float-time) start-time)))

(defun core-get-process-window (process)
  (let* ((buffer (process-buffer process)))
    (get-buffer-window buffer)))

(defun core-try-refresh-process-window (process)
  (let* ((window (core-get-process-window process)))
    (if window (core-refresh-process-window window))))

(defun core-refresh-process-window (window)
  (with-selected-window window
    (scroll-up-command (core-get-remaining-lines-count))))

(defun core-refresh-process-buffer (process)
  (with-current-buffer (process-buffer process)
    (ansi-color-apply-on-region (point-min) (point-max))))

(defun core-get-remaining-lines-count ()
    (count-lines (point) (buffer-end 1)))
