
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

(defun core-term (name)
  "Open term buffer with given name"
  (interactive "sName: ")
  (let ((buf-name (concat "*" name "*")))
    (shell buf-name)
    (switch-to-buffer (get-buffer buf-name))
    (run-at-time 0.4 nil 'core-term-cd-to-root)))

(defun core-term-cd-to-root ()
  (end-of-buffer)
  (insert (concat "cd " (projectile-project-root)))
  (term-send-input))

(defun core-npm-build ()
  "Runs 'npm run build' command"
  (interactive)
  (let ((bpr-scroll-direction -1))
    (bpr-spawn "npm run build")))

(defvar core-boo-role "app"
  "Default role for core-boo* commands")

(defun core-boo-set-role (role)
  "Sets default role for core-boo* commands"
  (interactive "sRole: ")
  (setq core-boo-role role))

(defun core-boo-run (command project)
  "Runs 'boo [command] [project]"
  (let ((bpr-scroll-direction -1))
    (bpr-spawn (concat "boo " command " " project))))

(defun core-boo-restart ()
  "Runs 'boo restart ROLE'"
  (interactive)
  (core-boo-run "restart" core-boo-role))

(defun core-boo-sync ()
  "Runs 'boo sync ROLE'"
  (interactive)
  (core-boo-run "sync" core-boo-role))

(defun core-boo-sync-restart ()
  "Runs 'boo sync ROLE && boo restart ROLE'"
  (interactive)
  (bpr-spawn (concat "boo sync " core-boo-role " && boo restart " core-boo-role)))

(defun core-boo-concat ()
  "Runs 'boo concat ROLE'"
  (interactive)
  (core-boo-run "concat" core-boo-role))

(defun core-bpr-package-tests ()
  "Tests emacs-bpr package"
  (interactive)
  (let* ((bpr-process-directory "~/my/emacs-bpr/")
         (bpr-scroll-direction -1))
    (bpr-spawn "~/.cask/bin/cask exec buttercup -L .")))

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

(defun core-open-work-org ()
  "Opens org buffers"
  (interactive)
  (find-file "~/my/org/organizer.org")
  (find-file-other-window "~/zon1/doc/report/ilia.txt")
  (split-window-vertically)
  (find-file "~/zon1/doc/design/version_plan.txt")
  (goto-char (point-min))
  (search-forward "(Ilia Babanov)"))

(defun core-copy-to-clipboard (text)
  "Copies text to clipboard"
  (when text
    (kill-new text)
    (message "Text is copied to clipboard: %s" text)))

(defun core-get-file-name ()
  "Returns file path of the current buffer"
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

(defun core-copy-file-name ()
  "Copies file name to clipboard"
  (interactive)
  (core-copy-to-clipboard (core-get-file-name)))

(defun core-copy-file-name-and-line-number ()
  "Copies file name with current line number to clipboard"
  (interactive)
  (core-copy-to-clipboard
   (concat (core-get-file-name) ":" (number-to-string (line-number-at-pos)))))

(defun core-magit-log-mine ()
  "Logs mine commits"
  (interactive)
  (magit-log nil '("-n128" "--decorate" "--author=babanov")))

(defun core-insert-g-arg ()
  "Inserts ' -g*.' text"
  (interactive)
  (insert-for-yank " -g*."))

(defun core-insert-g-js-arg ()
  "Inserts ' -g*.js' text"
  (interactive)
  (insert-for-yank " -g*.js"))
