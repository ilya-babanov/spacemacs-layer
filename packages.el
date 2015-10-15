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
      jade-mode
      color-identifiers-mode
      js-comint
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

(defun core/init-color-identifiers-mode ())

(defun core/init-js-comint()
  (use-package js-comint
    :defer t
    :init
    (progn
      (setq inferior-js-program-command "node -i")
      (evil-leader/set-key-for-mode 'js2-mode "mjL" 'js-send-last-sexp)
      (evil-leader/set-key-for-mode 'js2-mode "mjl" 'js-send-last-sexp-and-go)
      (evil-leader/set-key-for-mode 'js2-mode "mjB" 'js-send-buffer)
      (evil-leader/set-key-for-mode 'js2-mode "mjb" 'js-send-buffer-and-go)
      (evil-leader/set-key-for-mode 'js2-mode "mjf" 'js-load-file-and-go))))
