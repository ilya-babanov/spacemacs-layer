(defconst core-packages
  '(flycheck
    flycheck-package
    buttercup
    realgud
    helm-dash
    shell-pop
    org
    ein
    evil
    yasnippet
    flyspell
    js2-mode
    racket-mode
    projectile
    evil-cleverparens
    web-mode
    helm-projectile
    neotree
    ;; magit
    plsense
    (bpr :location local)))

(defun core/init-helm-dash ())

(defun core/init-buttercup ())

(defun core/init-realgud ())

;; (defun core/post-init-magit ()
;;   (setq magit-status-headers-hook
;;         '(magit-insert-error-header
;;           magit-insert-head-branch-header))
;;   ;; magit-insert-upstream-branch-header
;;   ;; magit-insert-diff-filter-header
;;   ;; magit-insert-upstream-branch-header
;;   (setq magit-status-sections-hook
;;         '(magit-insert-status-headers
;;           magit-insert-merge-log
;;           magit-insert-rebase-sequence
;;           magit-insert-am-sequence
;;           magit-insert-sequencer-sequence
;;           magit-insert-bisect-output
;;           magit-insert-bisect-rest
;;           magit-insert-bisect-log
;;           magit-insert-untracked-files
;;           magit-insert-unstaged-changes
;;           magit-insert-staged-changes
;;           magit-insert-unpushed-to-upstream))
;;   ;; magit-insert-unpulled-from-upstream
;;   ;; magit-insert-unpulled-from-pushremote
;;   ;; magit-insert-unpushed-to-pushremote
;;   (setq magit-log-arguments '("-n128" "--decorate"))
;;   (setq magit-log-section-arguments magit-log-arguments))

(defun core/post-init-neotree()
  (setq neo-window-width 50))

(defun core/pre-init-helm-projectile ()
  (setq helm-projectile-fuzzy-match nil))

(defun core/post-init-web-mode ()
  (add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tmpl\\'" . web-mode)))

(defun core/post-init-racket-mode ()
  (add-hook
   'racket-mode-hook
   (lambda ()
     (setq eldoc-documentation-function #'racket-eldoc-function))))

(defun core/post-init-evil-cleverparens ()
  (add-hook 'clojure-mode-hook #'evil-cleverparens-mode)
  (add-hook 'racket-mode-hook #'evil-cleverparens-mode)
  (add-hook 'emacs-lisp-mode-hook #'evil-cleverparens-mode)
  (evil-cleverparens-mode))

(defun core/post-init-shell-pop ()
  (setq shell-pop-autocd-to-working-dir nil)
  (setq shell-pop-window-height 65)
  (add-hook 'shell-pop-in-hook 'core-shell-pop-save-project-root)
  (add-hook 'shell-pop-in-after-hook 'core-shell-pop-cd-project))

(defun core/post-init-ein ()
  (setq ein:use-auto-complete-superpack t)
  (add-hook
   'ein:notebook-multilang-mode-hook
   (lambda ()
     (auto-complete-mode 1)
     (smartparens-mode 1))))

(defun core/post-init-evil ()
  (evil-set-initial-state 'shell-mode 'normal)
  (setq evil-move-beyond-eol nil)
  (setq evil-move-cursor-back nil))

(defun core/post-init-yasnippet ()
  (push "~/.emacs.d/private/core/snippets" yas-snippet-dirs))

(defun core/post-init-flyspell ()
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args
        '("--sug-mode=ultra"
          "--lang=en_US"
          "--run-together"
          "--run-together-limit=5"
          "--run-together-min=2"))
  (add-hook 'prog-mode-hook 'flyspell-mode))

(defun core/post-init-js2-mode ()
  (setq js-curly-indent-offset 0)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-bounce-indent-p t)
  (setq js2-include-node-externs t))

(defun core/post-init-projectile ()
  (setq projectile-enable-caching t)
  (with-eval-after-load 'projectile
    (dolist (dir '("dist" "build" "node_modules"))
      (push dir projectile-globally-ignored-directories))))

(defun core/post-init-flycheck ()
  (add-hook
   'js2-mode-hook
   (lambda ()
     (setq flycheck-highlighting-mode 'lines)
     (setq flycheck-check-syntax-automatically '(save mode-enabled)))))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    (flycheck-package-setup)))

(defun core/init-bpr ()
  (use-package bpr
    :defer t
    :init
    (autoload 'bpr-spawn "~/projects/emacs-bpr/bpr.el")
    (autoload 'bpr-open-last-buffer "~/projects/emacs-bpr/bpr.el")
    :config
    (setq bpr-colorize-output t)))

(defun core/post-init-org ()
  (core-init-org))
