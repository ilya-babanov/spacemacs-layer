
;; fix for ein package
(add-to-list 'load-path "~/.emacs.d/private/core/misc/")

(add-to-list 'load-path "~/my-projects/emacs-bpr/")
(require 'bpr)
(setq bpr-close-after-success t)
(setq bpr-colorize-output t)

(add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'shell-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'prog-mode-hook (lambda () (core-set-scroll-margin 15)))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

(setq multi-term-program "zsh")

(setq ispell-program-name "aspell")
(setq ispell-extra-args
      '("--sug-mode=ultra"
        "--lang=en_US"
        "--run-together"
        "--run-together-limit=5"
        "--run-together-min=2"))

(eval-after-load 'projectile
  '(add-to-list 'projectile-globally-ignored-directories "node_modules"))

(eval-after-load 'shell-pop
  '(progn
     (setq-default shell-pop-autocd-to-working-dir nil)
     (setq-default shell-pop-window-height 65)))

(eval-after-load 'neotree
  '(setq neo-vc-integration nil))

(eval-after-load 'flyspell
  '(add-hook 'prog-mode-hook 'flyspell-mode))

(eval-after-load 'yasnippet
  '(add-to-list 'yas-snippet-dirs "~/.emacs.d/private/core/snippets"))

(eval-after-load 'evil
  '(progn
     (setq evil-move-beyond-eol nil)
     (setq evil-move-cursor-back nil)))

(eval-after-load 'ein
  '(setq ein:use-auto-complete-superpack t))

(add-hook 'ein:notebook-multilang-mode-hook
          (lambda () (progn
                       (auto-complete-mode 1)
                       (smartparens-mode 1))))
