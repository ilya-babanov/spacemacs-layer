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

(defun core/init-elfeed ()
  (use-package elfeed
    :defer t
    :init
    (progn
      (setq-default elfeed-search-filter "@1-week-ago +unread ")
      (setq url-queue-timeout 15)
      (setq elfeed-feeds
            '(("http://hnrss.org/newest?points=150" hn)
              ("http://varlamov.ru/data/rss" blog)
              ("http://javascriptweekly.com/rss" js)
              ("http://www.smashingmagazine.com/feed/" js)
              ("http://habrahabr.ru/rss/blogs/javascript/" js)
              ("http://www.reddit.com/r/emacs/.rss" js)
              ("http://planet.emacsen.org/atom.xml" emacs))))))
