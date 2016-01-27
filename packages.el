;;; packages.el --- core Layer packages File for Spacemacs

(setq core-packages
    '(editorconfig
      flycheck-package
      buttercup
      realgud
      (bpr :location local)))

(setq core-excluded-packages '())

(defun core/init-buttercup ())

(defun core/init-editorconfig ())

(defun core/init-realgud ()
  (use-package realgud
    :defer t))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    (flycheck-package-setup)))

(defun core/init-bpr ()
  (use-package bpr
    :defer t
    :init
    (autoload 'bpr-spawn "~/my-projects/emacs-bpr/bpr.el")
    (autoload 'bpr-open-last-buffer "~/my-projects/emacs-bpr/bpr.el")
    :config
    (setq bpr-close-after-success t)
    (setq bpr-colorize-output t)))
