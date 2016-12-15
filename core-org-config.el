(defun core-init-org ()
  (setq org-agenda-files '("~/organizer.org"))
  (setq org-startup-indented t)
  (setq org-tags-column -140)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)))

  (setq org-capture-templates
        '(("t" "Tasks" entry
           (file+headline "~/organizer.org" "Tasks")
           "* TODO %?\n%i\n%a\n%T\n")
          ("n" "Notes" entry
           (file+headline "~/organizer.org" "Notes")
           "* %?\n%i\n"))))

(provide 'core-org-config)
