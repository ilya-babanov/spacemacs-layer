(defun core-init-org ()
  (setq my-org-file "~/org/organizer.org")
  (setq org-agenda-files `(,my-org-file))
  (setq org-startup-indented t)
  (setq org-tags-column -100)
  (setq org-startup-with-inline-images t)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (python . t)
     (css . t)
     (shell . t)
     (perl . t)))

  (setq org-capture-templates
        `(("n" "Note" entry
           (file+headline ,my-org-file "Notes")
           "* %?\n\t\n%i\n\n")
          ("b" "Book" entry
           (file+headline ,my-org-file "Books")
           "* %i%?\nAuthor: \n")
          ("B" "Booking" entry
           (file+headline ,my-org-file "Booking")
           "* %?\n%i\n")))

  (message "core-org initialized"))

(provide 'core-org)
