;; fix for ein package
;; (add-to-list 'load-path "~/.emacs.d/private/core/misc/")

(setq scroll-step 1)
(setq auto-window-vscroll nil)
(setq scroll-conservatively 10000)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)

(add-hook 'prog-mode-hook (lambda () (core-set-scroll-margin 8)))
(add-hook 'text-mode-hook (lambda () (core-set-scroll-margin 8)))
(add-hook 'org-mode-hook (lambda () (core-set-scroll-margin 8)))
(add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'term-mode-hook (lambda () (core-set-scroll-margin 0)))
(add-hook 'shell-mode-hook (lambda () (core-set-scroll-margin 0)))

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)

(setq multi-term-program "bash")

(setq ispell-program-name "aspell")
(setq ispell-extra-args
      '("--sug-mode=ultra"
        "--lang=en_US"
        "--run-together"
        "--run-together-limit=5"
        "--run-together-min=2"))

(with-eval-after-load 'projectile
  (dolist (dir '("build.debug" "build.DEBUG" "build.host" "build.HOST"))
    (add-to-list 'projectile-globally-ignored-directories dir)))

(with-eval-after-load 'js2-mode
  (load-file "~/.emacs.d/private/core/js-indent.el")
  (setq js-curly-indent-offset 1)
  (setq js2-strict-inconsistent-return-warning nil)
  (setq js2-strict-trailing-comma-warning nil)
  (setq js2-bounce-indent-p t)
  (setq js2-include-node-externs t))

(with-eval-after-load 'exec-path-from-shell
  (setq exec-path-from-shell-variables '("PATH" "GOPATH" "LANG"))
  (exec-path-from-shell-initialize))

(with-eval-after-load 'projectile
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(with-eval-after-load 'neotree
  (setq neo-vc-integration nil))

(with-eval-after-load 'flyspell
  (add-hook 'prog-mode-hook 'flyspell-mode))

(with-eval-after-load 'yasnippet
  (add-to-list 'yas-snippet-dirs "~/.emacs.d/private/core/snippets"))

(with-eval-after-load 'evil
  (evil-set-initial-state 'shell-mode 'normal)
  (setq evil-move-beyond-eol nil)
  (setq evil-move-cursor-back nil))

(with-eval-after-load 'ein
  (setq ein:use-auto-complete-superpack t)
  (add-hook
   'ein:notebook-multilang-mode-hook
   (lambda ()
     (auto-complete-mode 1)
     (smartparens-mode 1))))

(with-eval-after-load 'flycheck
  (add-hook
   'js2-mode-hook
   (lambda ()
     (setq flycheck-highlighting-mode 'lines)
     (setq flycheck-check-syntax-automatically '(save mode-enabled)))))

(with-eval-after-load 'shell-pop
  (setq-default shell-pop-autocd-to-working-dir nil)
  (setq-default shell-pop-window-height 65)
  (add-hook 'shell-pop-in-hook 'core-shell-pop-save-project-root)
  (add-hook 'shell-pop-in-after-hook 'core-shell-pop-cd-project))

(with-eval-after-load 'org
  (setq org-agenda-files '("~/my/org/organizer.org"))
  (setq org-startup-indented t)
  (setq org-tags-column -110)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)))
  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file+headline "~/my/org/organizer.org" "Tasks")
           "* TODO %?\n%i\n%a\n%T\n")
          ("n" "Notes" entry
           (file+headline "~/my/org/organizer.org" "Notes")
           "* %?\n%i\n")
          ("w" "Work Tasks" entry
           (file+headline "~/my/org/organizer.org" "Work Tasks")
           "* TODO %?\n%i\n%a\n%T\n"))))

(with-eval-after-load 'gnus
  ;; (gnus-add-configuration
  ;;  '(article (vertical 1.0 (summary .35 point) (article 1.0))))
  ;; Group buffer on the left, summary buffer top-right, article buffer bottom-right:
  (gnus-add-configuration
   '(article
     (horizontal 1.0
                 (vertical 25 (group 1.0))
                 (vertical 1.0 (summary 0.25 point) (article 1.0)))))
  (gnus-add-configuration
   '(summary
     (horizontal 1.0
                 (vertical 25 (group 1.0))
                 (vertical 1.0 (summary 1.0 point)))))
  (setq user-mail-address "ilya.babanov@gmail.com")
  (setq user-full-name "Ilia Babanov")
  ;; Get email, and store in nnml
  (setq gnus-secondary-select-methods
        '((nnimap "gmail"
                  (nnimap-address "imap.gmail.com")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl))))
  ;; Make Gnus NOT ignore [Gmail] mailboxes
  (setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")
  ;; Subscribe
  ;; (dolist (folder '("nnimap+gmail:INBOX" "nnimap+gmail:[Gmail]/All Mail"
  ;;                   "nnimap+gmail:[Gmail]/Drafts" "nnimap+gmail:GitHub"
  ;;                   "nnimap+gmail:[Gmail]/Sent Mail" "nnimap+gmail:[Gmail]/Spam"
  ;;                   "nnimap+gmail:[Gmail]/Trash"))
  ;;   (gnus-subscribe-hierarchically folder))
  ;; Send email via Gmail:
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.gmail.com")
  ;; Archive outgoing email in Sent folder on imap.gmail.com:
  (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
        gnus-message-archive-group "[Gmail]/Sent Mail")
  ;; set return email address based on incoming email address
  (setq gnus-posting-styles
        '((".*@hola.org"
           (address "ilia@hola.org")
           (signature "Ilia"))))
  ;; store email in ~/gmail directory
  (setq nnml-directory "~/.mail/gmail")
  (setq message-directory "~/.mail/gmail"))
