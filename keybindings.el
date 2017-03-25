(define-key evil-normal-state-map
  "\C-k"
  (lambda () (interactive) (evil-scroll-up 10)))

(define-key evil-normal-state-map
  "\C-j"
  (lambda () (interactive) (evil-scroll-down 10)))

(define-key evil-normal-state-map "§" 'helm-mini)
(define-key evil-normal-state-map "`" 'helm-mini)
(define-key evil-normal-state-map "!" 'spacemacs/evil-search-clear-highlight)

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
  "oa" 'core-boo-sync-app
  "ob" 'core-boo-sync-book
  "or" 'core-boo-restart-app
  "oR" 'core-boo-sync-restart-app
  "ol" 'bpr-open-last-buffer
  "oe" 'core-bpr-package-tests
  "oW" 'core-restart-wifi-osx
  "ot" 'core-term
  "oM" 'core-disable-scroll-margin
  "of" 'core-flyspell-save-word
  "op" (lambda () (interactive) (profiler-start 'cpu))
  "oP" 'profiler-report
  "gB" 'magit-branch-popup
  "hh" 'helm-semantic-or-imenu)
