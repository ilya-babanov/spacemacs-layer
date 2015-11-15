;;; packages.el --- core Layer packages File for Spacemacs

(setq core-packages
    '(editorconfig
      jade-mode
      color-identifiers-mode
      flycheck-package
      buttercup
      notmuch
      paredit))

;; List of packages to exclude.
(setq core-excluded-packages '())

(defun core/init-buttercup ())

(defun core/init-editorconfig ())

(defun core/init-jade-mode ())

(defun core/init-color-identifiers-mode ())

(defun core/init-paredit ()
  (use-package paredit
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
      (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
      (add-hook 'ielm-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
      (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
      (add-hook 'scheme-mode-hook           #'enable-paredit-mode))))

(defun core/init-notmuch ()
  (use-package notmuch
    :defer t
    :config
    (progn
      (setq smtpmail-smtp-server "smtp.yandex.ru"))))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    '(flycheck-package-setup)))
