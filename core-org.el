(defun core-init-org ()
  (setq org-agenda-files '("~/organizer.org"))
  (setq org-startup-indented t)
  (setq org-tags-column -140)
  (setq org-startup-with-inline-images t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)
     (perl . t)))

  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file "~/organizer.org")
           "* TODO %?\n%i\n%a\n%T\n")
          ("n" "Notes" entry
           (file "~/organizer.org")
           "* %?\n%i\n")))

  (message "core-org initialized"))

(provide 'core-org)
