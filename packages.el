;;; packages.el --- core Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq core-packages
    '(
      ;; package cores go here
      editorconfig
      flycheck
      projectile
      jade-mode
      shell-pop
      ))

;; List of packages to exclude.
(setq core-excluded-packages '())

;; For each package, define a function core/init-<package-core>
;;
;; (defun core/init-my-package ()
;;   "Initialize my package"
;;   )
;;
;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package

(defun core/init-editorconfig())

(defun core/init-jade-mode())

(defun core/init-flycheck()
  (use-package flycheck
    :defer t
    :init
    ;; (add-hook 'js2-mode-hook
    ;;           (lambda ()
    ;;             (flycheck-select-checker 'jsxhint-checker)
    ;;             (flycheck-mode)))
    :config
    (progn
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
      (add-to-list 'flycheck-checkers 'javascript-jscs)
    )))

(defun core/init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (add-to-list 'projectile-globally-ignored-directories "node_modules")
    )))

(defun core/init-shell-pop ()
  (use-package shell-pop
    :defer t
    :config
    (setq-default shell-pop-autocd-to-working-dir nil)
    (setq-default shell-pop-window-height 60)))

