(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up 5)))

(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down 5)))

(define-key evil-normal-state-map "§" 'helm-mini)

(define-key evil-normal-state-map ",f" 'neotree-find)

(define-key evil-normal-state-map "!" 'evil-search-highlight-persist-remove-all)

(global-set-key (kbd "M-§") 'helm-mini)
(global-set-key (kbd "<f4>") 'shell-pop-ansi-term)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(evil-set-initial-state 'shell-mode 'normal)

(evil-leader/set-key-for-mode 'js2-mode
  "mdf" 'js-doc-insert-function-doc
  "mdt" 'js-doc-insert-file-doc
  "mgb" 'tern-pop-find-definition
  "mtr" 'tern-rename-variable)

(evil-leader/set-key-for-mode 'elfeed-search-mode
  "mo" 'elfeed-search-show-entry
  "m+" 'elfeed-search-tag-all
  "m-" 'elfeed-search-untag-all
  "mg" 'elfeed-update
  "mG" 'elfeed-search-update--force
  "mS" 'elfeed-search-set-filter
  "mb" 'elfeed-search-browse-url
  "mn" 'next-line
  "mp" 'previous-line
  "mq" 'quit-window
  "mr" 'elfeed-search-untag-all-unread
  "ms" 'elfeed-search-live-filter
  "mu" 'elfeed-search-tag-all-unread
  "my" 'elfeed-search-yank)

(evil-leader/set-key-for-mode 'elfeed-show-mode
  "m+" 'elfeed-show-tag
  "m-" 'elfeed-show-untag
  "mb" 'elfeed-show-visit
  "md" 'elfeed-show-save-enclosure
  "mg" 'elfeed-show-refresh
  "mn" 'elfeed-show-next
  "mp" 'elfeed-show-prev
  "mq" 'elfeed-kill-buffer
  "ms" 'elfeed-show-new-live-search
  "my" 'elfeed-show-yank)

(evil-leader/set-key
  "ob" 'bpr-open-last-buffer
  "og" 'core-grunt-tests
  "oG" 'core-grunt-build
  "on" 'core-npm-tests
  "oe" 'core-bpr-package-tests
  "or" 'elfeed
  "om" 'notmuch
  "oW" 'core-restart-wifi-osx
  "oM" 'core-disable-scroll-margin
  "of" 'core-flyspell-save-word
  "gp" 'magit-pull-popup
  "gP" 'magit-push-popup
  "wn" 'eyebrowse-next-window-config)

(eval-after-load 'yasnippet
  '(progn
     (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
     (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)))

(eval-after-load 'elm-mode
  '(define-key elm-mode-map "\C-cc" 'company-elm))
