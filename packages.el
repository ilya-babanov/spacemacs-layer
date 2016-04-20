;;; packages.el --- core Layer packages File for Spacemacs

(setq core-packages
    '(editorconfig
      flycheck
      flycheck-package
      buttercup
      realgud
      helm-dash
      (bpr :location local)))

(setq core-excluded-packages '())

(defun core/init-helm-dash ())

(defun core/init-buttercup ())

(defun core/init-editorconfig ())

(defun core/init-realgud ()
  (use-package realgud
    :defer t))

(defun core/post-init-flycheck ()
  (dolist (mode '(markdown-mode git-commit-mode text-mode org-mode))
    (spacemacs/add-flycheck-hook mode)))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    (flycheck-package-setup)))

(defun core/init-bpr ()
  (use-package bpr
    :defer t
    :init
    (autoload 'bpr-spawn "~/my/emacs-bpr/bpr.el")
    (autoload 'bpr-open-last-buffer "~/my/emacs-bpr/bpr.el")
    :config
    (setq bpr-close-after-success t)
    (setq bpr-colorize-output t)))
