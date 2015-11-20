
(defun core-enable-evil-normal-state ()
  (unless (member major-mode evil-normal-state-modes)
    (message "Dynamically set evil state to 'normal for %s" (buffer-name))
    (evil-set-initial-state major-mode 'normal)))

(defun core-enable-evil-emacs-state ()
  (unless (member major-mode evil-emacs-state-modes)
    (message "Dynamically set evil state to 'emacs for %s" (buffer-name))
    (evil-set-initial-state major-mode 'emacs)))

(defun core-set-scroll-margin (param)
  "Makes scroll-margin var 'buffer-local' and sets variable to it"
  (progn
    (make-variable-buffer-local 'scroll-margin)
    (setq scroll-margin param)))

(defun core-disable-scroll-margin ()
  "Set scroll-margin to 0"
  (interactive)
  (core-set-scroll-margin 0))

(defun delete-tern-process ()
  "Deletes tern process (after that, tern restarts automatically)"
  (interactive)
  (delete-process "Tern"))

(defun core-grunt-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((bpr-scroll-direction -1))
    (bpr-spawn "grunt test --color")))

(defun core-grunt-build ()
  "Invokes grunt buil and shows output"
  (interactive)
  (bpr-spawn "grunt build --color"))

(defun core-npm-tests ()
  "Invokes grunt test task and shows output"
  (interactive)
  (let* ((bpr-scroll-direction -1))
    (bpr-spawn "npm run test --color")))

(defun core-bpr-package-tests ()
  "Tests emacs-bpr package"
  (interactive)
  (let* ((bpr-process-directory "~/my-projects/emacs-bpr/"))
    (bpr-spawn "cask exec buttercup -L .")))

(defun core-restart-wifi-osx ()
  "Restarts wifi on osx"
  (interactive)
  (let* ((bpr-process-directory "~/"))
    (bpr-spawn "networksetup -setairportpower en0 off; sleep 4; networksetup -setairportpower en0 on")))
