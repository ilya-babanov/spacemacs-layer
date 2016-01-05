
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
  (make-variable-buffer-local 'scroll-margin)
  (setq scroll-margin param))

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

(defun core-flyspell-save-word ()
  "Saves word to flyspell current dictionary"
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)
      (message "Saving word '%s' to dictionary" (car word))
      (flyspell-do-correct 'save nil
                           (car word)
                           current-location
                           (cadr word)
                           (caddr word)
                           current-location))))

(defun core-eval-line (eval-fn)
  "Evaluates current line"
  (save-excursion
    (end-of-line)
    (set-mark (line-beginning-position))
    (call-interactively eval-fn)))

(defun core-eval-region-or-line (eval-fn)
  "Evaluates selected region or current line"
  (if (use-region-p)
      (call-interactively eval-fn)
    (core-eval-line eval-fn)))

(defun core-eval-py ()
  "Evaluatetes current line as python code"
  (interactive)
  (core-eval-region-or-line 'python-shell-send-region))
