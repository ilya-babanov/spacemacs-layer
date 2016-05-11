
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

(defvar z-env-export "export DIST=APP, DEBUG=y, BUILD=debug; ")

(defun core-bpr-jmake-cm-release ()
  "Run jmake cm release"
  (interactive)
  (bpr-spawn (concat z-env-export "jmake cm release")))

(defun core-bpr-jmake-and-zlxc (group)
  "Run jmake cm release"
  (interactive "sZLXC run arguments:")
  (bpr-spawn (concat z-env-export "jmake cm release && zlxc run " group)))

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

(defvar core-shell-pop-project-root nil
  "Project root for shell-pop")

(defvar core-shell-pop-prev-project-root nil
  "Previous project root for shell-pop")

(defun core-shell-pop-save-project-root ()
  "Saves project root to global variable"
  (setq core-shell-pop-prev-project-root core-shell-pop-project-root)
  (setq core-shell-pop-project-root
        (condition-case nil
            (projectile-project-root)
          (error nil))))

(defun core-shell-pop-cd-project ()
  "cd to project root"
  (when (and (not (null core-shell-pop-project-root))
             (not (string=
                   core-shell-pop-prev-project-root
                   core-shell-pop-project-root)))
    (shell-pop--cd-to-cwd core-shell-pop-project-root)))

(defun core-eval-line (eval-region)
  "Evaluates current line"
  (save-excursion
    (end-of-line)
    (set-mark (line-beginning-position))
    (call-interactively eval-region)))

(defun core-eval-region-or-line (eval-region)
  "Evaluates active region or current line"
  (if (use-region-p)
      (call-interactively eval-region)
    (core-eval-line eval-region)))

(defun core-eval-py ()
  "Evaluatetes active region or current line as python code"
  (interactive)
  (core-eval-region-or-line 'python-shell-send-region))
