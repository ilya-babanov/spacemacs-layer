;;; packages.el --- core Layer packages File for Spacemacs

(setq core-packages
    '(editorconfig
      jade-mode
      color-identifiers-mode
      flycheck-package
      buttercup
      elfeed
      notmuch
      paredit))

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
      (setq notmuch-hello-sections
            '(notmuch-hello-insert-header
              notmuch-hello-insert-saved-searches
              notmuch-hello-insert-search
              notmuch-hello-insert-alltags
              notmuch-hello-insert-footer))
      (setq notmuch-saved-searches
            '((:name "Recent" :query "tag:inbox AND date:2015-09-09.." :key "r" :sort-order newest-first)
              (:name "Unread" :query "tag:unread" :key "u" :sort-order newest-first)
              (:name "Flagged" :query "tag:flagged" :key "f" :sort-order newest-first)
              (:name "Sent" :query "tag:sent" :key "t" :sort-order newest-first)
              (:name "Drafts" :query "tag:draft" :key "d" :sort-order newest-first)
              (:name "All" :query "*" :key "a" :sort-order newest-first)))
      (setq notmuch-search-oldest-first nil)
      (setq smtpmail-smtp-server "smtp.yandex.ru"))))

(defun core/init-flycheck-package ()
  (use-package flycheck-package
    :defer t
    :config
    '(flycheck-package-setup)))

(defun core/init-elfeed ()
  (use-package elfeed
    :defer t
    :init
    (progn
      (setq-default elfeed-search-filter "@4-week-ago +unread ")
      (setq url-queue-timeout 15)
      (evil-set-initial-state 'elfeed-search-mode 'motion)
      (evil-set-initial-state 'elfeed-show-mode 'motion)
      (setq elfeed-feeds
            '(("http://hnrss.org/newest?points=150" hn)
              ("http://varlamov.ru/data/rss" blog)
              ("http://www.smashingmagazine.com/feed/" js)
              ("http://habrahabr.ru/rss/blogs/javascript/" js)
              ("http://www.theverge.com/rss/group/review/index.xml" review)
              ("http://www.reddit.com/r/emacs/.rss" emacs)
              ("http://planet.emacsen.org/atom.xml" emacs))))))
