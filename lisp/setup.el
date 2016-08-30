(defun org-toggle-latex-overlays (arg)
  "Toggle LaTeX fragments.  The prefix ARG is passed to `org-preview-latex-fragment'."
  (interactive "P")
  (if (org--list-latex-overlays)
      (org-remove-latex-fragment-image-overlays)
    (org-toggle-latex-fragment '(16)))
  nil)


(plist-put org-format-latex-options :scale 1.5)
