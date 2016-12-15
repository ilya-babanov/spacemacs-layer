(define-key evil-normal-state-map
  "\C-k"
  (lambda () (interactive) (evil-scroll-up 10)))

(define-key evil-normal-state-map
  "\C-j"
  (lambda () (interactive) (evil-scroll-down 10)))

(define-key evil-normal-state-map "<" 'helm-mini)
(define-key evil-normal-state-map "§" 'helm-mini)
(define-key evil-normal-state-map "`" 'helm-mini)
(define-key evil-normal-state-map "!" 'evil-search-highlight-persist-remove-all)
(define-key global-map (kbd "M-§") 'helm-mini)
(define-key global-map (kbd "<f4>") 'spacemacs/default-pop-shell)

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
  "os" 'bpr-spawn
  "ob" 'bpr-open-last-buffer
  "ot" 'core-term
  "oe" 'core-bpr-package-tests
  "or" 'elfeed
  "om" 'notmuch
  "oW" 'core-restart-wifi-osx
  "oM" 'core-disable-scroll-margin
  "of" 'core-flyspell-save-word
  "gB" 'magit-branch-popup
  "hh" 'helm-semantic-or-imenu)

(with-eval-after-load 'yasnippet
  (define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
  (define-key yas-minor-mode-map (kbd "TAB") 'yas-expand))

(with-eval-after-load 'elm-mode
  (define-key elm-mode-map "\C-cc" 'company-elm))
