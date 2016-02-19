
;; fix for ein package
(add-to-list 'load-path "~/.emacs.d/private/core/misc/")

(add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'shell-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'prog-mode-hook (lambda () (core-set-scroll-margin 15)))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

(setq multi-term-program "/usr/local/bin/zsh")

(setq ispell-program-name "aspell")
(setq ispell-extra-args
      '("--sug-mode=ultra"
        "--lang=en_US"
        "--run-together"
        "--run-together-limit=5"
        "--run-together-min=2"))

(eval-after-load 'projectile
  '(add-to-list 'projectile-globally-ignored-directories "node_modules"))

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
  '(progn
     (setq ein:use-auto-complete-superpack t)
     (add-hook 'ein:notebook-multilang-mode-hook
               (lambda () (progn
                            (auto-complete-mode 1)
                            (smartparens-mode 1))))))

(with-eval-after-load 'shell-pop
  (setq-default shell-pop-autocd-to-working-dir nil)
  (setq-default shell-pop-window-height 65)
  (add-hook 'shell-pop-in-hook 'core-shell-pop-save-project-root)
  (add-hook 'shell-pop-in-after-hook 'core-shell-pop-cd-project))

(with-eval-after-load 'org
  (setq org-agenda-files '("~/Yandex.Disk.localized/org/organizer.org"))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)))
  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file+headline "~/Yandex.Disk.localized/org/organizer.org" "Tasks")
           "* TODO %?\n%i\n%a\n%T\n")
          ("n" "Notes" entry
           (file+headline "~/Yandex.Disk.localized/org/organizer.org" "Notes")
           "* NOTE %?\n%i\n%a\n%T\n")
          ("w" "Work Tasks" entry
           (file+headline "~/Yandex.Disk.localized/org/organizer.org" "Work Tasks")
           "* TODO %?\n%i\n%a\n%T\n")
          ("N" "Work Notes" entry
           (file+headline "~/Yandex.Disk.localized/org/organizer.org" "Work Notes")
           "* NOTE %?\n%i\n%a\n%T\n"))))
