;;; packages.el --- core Layer packages File for Spacemacs

(setq core-packages
    '(editorconfig
      jade-mode
      flycheck-package
      buttercup
      realgud))

(setq core-excluded-packages '())

(defun core/init-buttercup ())

(defun core/init-editorconfig ())

(defun core/init-jade-mode ())

(defun core/init-realgud ()
  (use-package realgud
    :defer t))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    '(flycheck-package-setup)))
