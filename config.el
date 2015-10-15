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
  (core-start-process "grunt test --no-color"))

(defun core-npm-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (core-start-process "npm run test --no-color"))

(defun core-grunt-build ()
  "Invokes grunt buil and shows output"
  (interactive)
  (core-start-process "grunt build --no-color"))

(defun core-run-cmd (cmd &optional args)
  (setq cmd-name (concat cmd " " args))
  (async-shell-command cmd-name))

(defun core-start-process (cmd)
  (if (not (get-process cmd))
      (progn
        (message "Running process '%s'" cmd)
        (setq buffer-name (concat "*" cmd "*"))
        (setq buffer (get-buffer-create buffer-name))
        (setq process (start-process-shell-command cmd buffer cmd))
        (core-handle-progress process))
    (message "Process already exist '%s'" cmd)))

(defun core-handle-progress (process)
  (setq status (process-status process))
  (if (string-equal status "exit")
      (core-handle-result process)
    (progn
      (princ ".")
      (run-at-time 0.1 nil 'core-handle-progress process))))

(defun core-handle-result (process)
  (setq status (process-status process))
  (setq exit-code (process-exit-status process))
  (if (= exit-code 0)
      (core-handle-success process status exit-code)
    (core-handle-error process exit-code)))

(defun core-handle-success (process status exit-code)
  (message "\nProcess: %s\nStatus: %s\nCode: %s\n"
                 process status exit-code))

(defun core-handle-error (process exit-code)
  (message "%s ERROR, CODE: %s" process exit-code)
  (setq buffer (process-buffer process))
  (setq buffer-window (get-buffer-window buffer))
  (if (not buffer-window)
      (progn
        (setq buffer-window (split-window-vertically))
        (set-window-buffer buffer-window buffer)))
  (core-show-window-bottom buffer-window))

(defun core-show-window-bottom (window)
  (with-selected-window window
    (scroll-up-command
     (- (core-get-remaining-lines-count) 5))))

(defun core-get-remaining-lines-count ()
    (count-lines (point) (buffer-end 1)))
