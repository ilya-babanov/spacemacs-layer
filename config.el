;; configure bpr package
(add-to-list 'load-path "~/my-projects/emacs-bpr/")
(require 'bpr)
(setq bpr-close-after-success t)
(setq bpr-colorize-output t)

(add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'shell-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'prog-mode-hook (lambda () (core-set-scroll-margin 15)))
(add-hook 'js2-mode-hook 'subword-mode)

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

(setq multi-term-program "/usr/bin/screen")

;; (setq-default truncate-lines 0)

;; set program for spell checking
(setq ispell-program-name "aspell")
;; force the English dictionary, support Camel Case spelling check (tested with aspell 0.6)
(setq ispell-extra-args
      '("--sug-mode=ultra"
        "--lang=en_US"
        "--run-together"
        "--run-together-limit=5"
        "--run-together-min=2"))

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

;; (eval-after-load 'flyspell
;;   '(progn
;;      (message "Flyspell loaded, add hook to prog-mode")
;;      (add-hook 'prog-mode-hook (lambda () (flyspell-prog-mode)))))

;; (shell :variables
;;        ;; shell-default-shell 'ansi-term
;;        shell-default-term-shell "/bin/zsh"
;;        ;; shell-default-position 'bottom
;;        shell-default-height 60)
;; (haskell :variables
;;          haskell-enable-hindent-style "johan-tibell"
;;          haskell-enable-ghc-mod-support t)
;; git
;; org
;; markdown
;; eyebrowse
;; spell-checking
;; version-control
;; auto-completion
;; syntax-checking
;; go
;; elm
;; python
;; javascript
;; emacs-lisp
;; common-lisp
;; core

;; dotspacemacs-default-font '("Hack"
;;                             :size 15
;;                             :weight normal
;;                             :width normal
;;                             :powerline-scale 1)

;; (defun dotspacemacs/user-config ()
;;   "Configuration function.
;;  This function is called at the very end of Spacemacs initialization after
;; layers configuration."
;;   (setq scroll-margin 17)
;;   ;; add GOPATH environment variable
;;   (exec-path-from-shell-initialize)
;;   (exec-path-from-shell-copy-env "GOPATH")
;;   (spacemacs/toggle-mode-line-minor-modes-off)
;;   (setq powerline-default-separator nil))
