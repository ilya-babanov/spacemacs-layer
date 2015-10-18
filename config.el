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
  (let* ((core-close-after-success t))
    (core-try-start-process "grunt build")))

(defun core-test ()
  "Test function"
  (interactive)
  (let* ((core-erase-process-buffer nil))
    (core-try-start-process "grunt test")))

(defvar core-close-after-success nil)
(defvar core-erase-process-buffer t)
(defvar core-process-mode 'shell-mode)
(defvar core-hide-progress nil)
(defvar core-poll-timout 0.15)
(defvar core-run-statuses '(run listen open connect))

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
    (core-config-process-buffer buffer)
    (if core-hide-progress
        (set-process-sentinel process 'core-handle-result)
      (core-handle-progress process))))

(defun core-create-process-plist ()
  (list 'poll-timeout core-poll-timout
        'run-statuses core-run-statuses
        'close-after-success core-close-after-success))

(defun core-config-process-buffer (buffer)
  (with-current-buffer buffer
    (if core-erase-process-buffer (erase-buffer))
    (funcall core-process-mode)))

(defun core-handle-progress (process)
  (let* ((status (process-status process))
         (run-statuses (process-get process 'run-statuses)))
    (if (member status run-statuses)
        (progn
          (princ ".")
          (run-at-time (process-get process 'poll-timeout) nil 'core-handle-progress process))
      (core-handle-result process))))

(defun core-handle-result (process &optional event)
  (let* ((exit-code (process-exit-status process)))
    (core-refresh-process-buffer process)
    (if (= exit-code 0)
        (core-handle-success process exit-code)
      (core-handle-error process exit-code))))

(defun core-handle-success (process exit-code)
  (message "Process: %s\nCode: %s" process exit-code)
  (let* ((buffer-window (core-get-process-window process))
         (close-after-success (process-get process 'close-after-success)))
    (if (and buffer-window close-after-success)
        (delete-window buffer-window))))

(defun core-handle-error (process exit-code)
  (message "%s ERROR, CODE: %s" process exit-code)
  (let* ((buffer (process-buffer process))
         (buffer-window (get-buffer-window buffer)))
    (if (not buffer-window)
        (progn
          (setq buffer-window (split-window-vertically))
          (set-window-buffer buffer-window buffer)))
    (core-refresh-process-window buffer-window)))

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
