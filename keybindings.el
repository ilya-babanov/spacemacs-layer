(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up 5)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down 5)))

(define-key evil-normal-state-map "§" 'helm-mini)

(define-key evil-normal-state-map ",tr" 'tern-rename-variable)

(define-key evil-normal-state-map ",f" 'neotree-find)

(define-key evil-normal-state-map ",df" 'js-doc-insert-function-doc)
(define-key evil-normal-state-map ",dt" 'js-doc-insert-file-doc)

(define-key evil-normal-state-map "!" 'evil-search-highlight-persist-remove-all)

(global-set-key (kbd "M-§") 'helm-mini)

(evil-leader/set-key "wn" 'eyebrowse-next-window-config)

(define-key yas-minor-mode-map (kbd "<tab>") 'yas-expand)
(define-key yas-minor-mode-map (kbd "TAB") 'yas-expand)

(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
