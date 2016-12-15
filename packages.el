(defconst core-packages
  '(editorconfig
    flycheck
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
    projectile
    (bpr :location local)))

(defun core/init-helm-dash ())

(defun core/init-buttercup ())

(defun core/init-realgud ())

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
  (add-hook 'prog-mode-hook 'flyspell-mode))

(defun core/post-init-js2-mode ()
  (setq js-curly-indent-offset 1)
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
     (setq flycheck-check-syntax-automatically '(save mode-enabled))))
  (dolist (mode '(markdown-mode git-commit-mode text-mode org-mode))
    (spacemacs/add-flycheck-hook mode)))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    (flycheck-package-setup)))

(defun core/init-editorconfig ()
  (use-package editorconfig
    :config
    (editorconfig-mode)))

(defun core/init-bpr ()
  (use-package bpr
    :defer t
    :init
    (autoload 'bpr-spawn "~/my/emacs-bpr/bpr.el")
    (autoload 'bpr-open-last-buffer "~/my/emacs-bpr/bpr.el")
    :config
    (setq bpr-close-after-success t)
    (setq bpr-colorize-output t)))

(defun core/post-init-org ()
  (setq org-agenda-files '("~/organizer.org"))
  (setq org-startup-indented t)
  (setq org-tags-column -140)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)))
  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file+headline "~/organizer.org" "Tasks")
           "* TODO %?\n%i\n%a\n%T\n")
          ("n" "Notes" entry
           (file+headline "~/organizer.org" "Notes")
           "* %?\n%i\n"))))
