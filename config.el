;; enable linum-mode and scroll-margin in programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (progn
                              ;; (linum-mode t)
                              (my-set-scroll-margin 17))))

;; disable scroll-margin in term mode
(add-hook 'term-mode-hook (lambda () (my-set-scroll-margin 0)))

(add-hook 'emacs-lisp-mode-hook (lambda () (color-identifiers-mode t)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; do not wrap files by default
(setq-default truncate-lines 0)

;; configure default states for evil mode
(setq evil-default-state 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(add-hook 'prog-mode-hook 'core-enable-evil-normal-state)
(add-hook 'text-mode-hook 'core-enable-evil-normal-state)

(defun core-enable-evil-normal-state ()
  (unless (member major-mode evil-normal-state-modes)
    (message "Dynamically set evil state to 'normal for %s" (buffer-name))
    (evil-set-initial-state major-mode 'normal)))

(defun core-enable-evil-emacs-state ()
  (unless (member major-mode evil-emacs-state-modes)
    (message "Dynamically set evil state to 'emacs for %s" (buffer-name))
    (evil-set-initial-state major-mode 'emacs)))

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
    (setq-default shell-pop-window-height 65)))

(eval-after-load 'neotree
  '(progn (setq neo-vc-integration nil)))

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
  (bpr-spawn "grunt build --color"))

(defun core-npm-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((bpr-scroll-direction -1))
    (bpr-spawn "npm run test --color")))

(defun core-bpr-package-tests ()
  "Tests emacs-bpr package"
  (interactive)
  (let* ((bpr-process-directory "~/my-projects/emacs-bpr/"))
    (bpr-spawn "cask exec buttercup -L .")))
