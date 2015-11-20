;; configure bpr package
(add-to-list 'load-path "~/my-projects/emacs-bpr/")
(require 'bpr)
(setq bpr-close-after-success t)
(setq bpr-colorize-output t)

;; enable linum-mode and scroll-margin in programming modes
(add-hook 'prog-mode-hook (lambda ()
                            (progn
                              ;; (linum-mode t)
                              (core-set-scroll-margin 17))))

;; disable scroll-margin in term mode
(add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))

(add-hook 'emacs-lisp-mode-hook (lambda () (color-identifiers-mode t)))

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

;; do not wrap files by default
(setq-default truncate-lines 0)

;; configure default states for evil mode
(setq evil-default-state 'emacs)
(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'elfeed-show-mode 'motion)
(evil-set-initial-state 'fundamental-mode 'normal)
(add-hook 'prog-mode-hook 'core-enable-evil-normal-state)
(add-hook 'text-mode-hook 'core-enable-evil-normal-state)

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

(eval-after-load 'neotree
  '(progn (setq neo-vc-integration nil)))
