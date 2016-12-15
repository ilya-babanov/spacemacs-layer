(push (file-name-directory load-file-name) load-path)
(require 'core-org-config)

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
