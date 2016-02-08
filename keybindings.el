(evil-set-initial-state 'magit-status-mode 'emacs)
(evil-set-initial-state 'elfeed-search-mode 'emacs)
(evil-set-initial-state 'shell-mode 'normal)

(define-key evil-normal-state-map
  (kbd "C-k")
  (lambda ()
    (interactive)
    (evil-scroll-up 5)))

(define-key evil-normal-state-map
  (kbd "C-j")
  (lambda ()
    (interactive)
    (evil-scroll-down 5)))

(define-key evil-normal-state-map "§" 'helm-mini)
(define-key evil-normal-state-map "!" 'evil-search-highlight-persist-remove-all)

(global-set-key (kbd "M-§") 'helm-mini)
(global-set-key (kbd "<f4>") 'shell-pop-ansi-term)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

(spacemacs/set-leader-keys-for-major-mode 'js2-mode
  "df" 'js-doc-insert-function-doc
  "dt" 'js-doc-insert-file-doc
  "gb" 'tern-pop-find-definition
  "tr" 'tern-rename-variable)

(spacemacs/set-leader-keys-for-major-mode 'python-mode
  "," 'core-eval-py)

(spacemacs/set-leader-keys
  "ob" 'bpr-open-last-buffer
  "oG" 'core-grunt-build
  "og" 'core-grunt-tests
  "on" 'core-npm-tests
  "oN" 'core-grunt-tests-nano
  "oe" 'core-bpr-package-tests
  "or" 'elfeed
  "om" 'notmuch
  "oW" 'core-restart-wifi-osx
  "oM" 'core-disable-scroll-margin
  "of" 'core-flyspell-save-word
  "gB" 'magit-branch-popup
  "hh" 'helm-semantic-or-imenu)

(eval-after-load 'yasnippet
  '(progn
     (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
     (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)))

(eval-after-load 'elm-mode
  '(define-key elm-mode-map "\C-cc" 'company-elm))
