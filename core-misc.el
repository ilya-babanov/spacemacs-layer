(defun core-init-misc ()
  (setq mouse-wheel-scroll-amount '(2 ((shift) . 2)))
  (setq mouse-wheel-progressive-speed nil)

  (setq powerline-default-separator nil)

  (spacemacs/toggle-mode-line-minor-modes-off)

  (add-hook 'comint-mode-hook 'spacemacs/toggle-truncate-lines-on)
  (add-hook 'prog-mode-hook 'spacemacs/toggle-truncate-lines-on)

  (add-hook 'prog-mode-hook (lambda () (core-set-scroll-margin 8)))
  (add-hook 'text-mode-hook (lambda () (core-set-scroll-margin 8)))
  (add-hook 'org-mode-hook (lambda () (core-set-scroll-margin 8)))

  (add-hook 'comint-mode-hook (lambda () (core-set-scroll-margin 0)))
  (add-hook 'term-mode-hook (lambda () (core-set-scroll-margin 0)))
  (add-hook 'shell-mode-hook (lambda () (core-set-scroll-margin 0)))

  (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'comint-mode-hook 'ansi-color-for-comint-mode-on)
  (add-hook 'term-mode-hook 'ansi-color-for-comint-mode-on)

  (add-hook 'css-mode-hook 'rainbow-mode)

  (setq helm-buffer-max-length 50)

  (setq eshell-prompt-function
        (lambda nil
          (concat
           (propertize (eshell/dirs) 'face `(:foreground "blue"))
           (propertize "$ " 'face `(:foreground "green")))))

  (setq eshell-highlight-prompt nil)

  (message "core-misc initialized"))

(provide 'core-misc)
