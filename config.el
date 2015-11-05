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
    (setq-default shell-pop-window-height 65)))

(add-to-list 'load-path "~/my-projects/emacs-bpr/")
(require 'bpr)

(setq bpr-close-after-success t)
(setq bpr-colorize-output t)

(defun core-grunt-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((bpr-scroll-direction -1))
    (bpr-spawn "grunt test --color")))

(defun core-grunt-build ()
  "Invokes grunt buil and shows output"
  (interactive)
  (let* ((bpr-poll-timout 0.3))
    (bpr-spawn "grunt build --color")))

(defun core-npm-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((bpr-scroll-direction -1))
    (bpr-spawn "npm run test --color")))

(defun core-bpr-package-tests ()
  "Tests emacs-bpr package"
  (interactive)
  (let* ((bpr-process-directory "~/my-projects/emacs-bpr/"))
    (bpr-spawn "emacs -batch -l ert -l ./bpr.el -l ./tests/bpr-tests.el -f ert-run-tests-batch-and-exit")))
