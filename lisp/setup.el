(defun org-toggle-latex-overlays (arg)
  "Toggle LaTeX fragments.  The prefix ARG is passed to `org-preview-latex-fragment'."
  (interactive "P")
  (if (org--list-latex-overlays)
      (org-remove-latex-fragment-image-overlays)
    (org-toggle-latex-fragment '(16)))
  nil)


(plist-put org-format-latex-options :scale 1.8)

(defun tq-turn-it-in ()
  "Save all buffers, add files, create a SYSTEM-INFO file, commit them and push.
Check *techela log* for error messages."
  (interactive)
  (tq-insert-system-info)

  ;; Let's assume turning in will work, and set the time.
  (gb-set-filetag "TURNED-IN" (current-time-string))

  ;; make sure all buffers are saved
  (save-some-buffers t t)
  
  (mygit "git add *")
  
  (let ((status (car (mygit "git commit -am \"turning in\""))))
    (unless (or (= 0 status)		; no problem
		(= 1 status))		; no change in files
      (gb-set-filetag "TURNED-IN"
		      (concat "Failed: " (current-time-string)))
      (switch-to-buffer "*techela log*")
      (error "Problem committing.  Check the logs")))

  (unless (= 0 (car (mygit "git push -u origin master")))
    (mygit "git commit --amend -m \"*** TURNING IN FAILED ***.\"")
    (gb-set-filetag "TURNED-IN" (concat "Failed: " (current-time-string)))
    (save-buffer)
    (switch-to-buffer "*techela log*")
    (error "Problem pushing to server.  Check the logs"))

  (save-buffer)
  (message
   (let ((choices '("Woohoo, you turned it in!"
		    "Awesome, you rocked that turn in!"
		    "Way to go, you turned it in!"
		    "Great job, you turned it in!"
		    "Sweet, you turned it in!"
		    "Booya, you turned it in!")))
     (nth (cl-random (length choices)) choices))))

(defun tq-get-assignment (label)
  "Clone the repo corresponding to LABEL and open the directory."
  (interactive
   (list (completing-read "Label: " (tq-get-assigned-assignments))))
  (let ((student-repo-dir (file-name-as-directory
			   (expand-file-name
			    label
			    tq-assignment-directory))))

    ;; Get the assignment by cloning if needed, and rest the remotes.
    (unless (file-directory-p student-repo-dir)
      (make-directory tq-assignment-directory t)
      (let ((default-directory (file-name-as-directory tq-assignment-directory))
	    (repo (format "assignments/%s" label)))
	;; clone and open label.org
	(tq-clone-repo repo)
	;; we need to reset the remotes now
	(with-current-directory
	 student-repo-dir
	 (mygit "git remote rename origin src")
	 (mygit
	  (mustache-render
	   "git remote add origin {{host}}:student-work/{{label}}/{{userid}}-{{label}}"
	   (ht ("host" (techela-course-techela-server tq-current-course))
	       ("label" label)
	       ("userid" (gethash "user-mail-address" (tq-read-user-data)))))))))

    (with-current-directory
     student-repo-dir
     (if (not (string= "" (shell-command-to-string
			   "git status --porcelain")))
	 ;; There are some local changes. We commit them, pull,
	 ;; and commit merges if there are any
	 (progn 
	   (mygit "git add *")
	   (mygit "git commit -am \"my changes\"") 
	   (mygit "git pull") 
	   (mygit "git commit -am \"commit post pull\""))
       (mygit "git pull"))

     ;; now, open the file
     (find-file (expand-file-name
		 (concat label ".org")
		 student-repo-dir)))))

;; * have students course under vc
;; except for me. I don't want to commit my changes automatically
(unless (string= "jkitchin@andrew.cmu.edu" (or user-mail-address ""))
  (with-current-directory
   tq-course-directory
   (mygit "git add *")
   (mygit "git commit -am \"my course changes.\"")))


;; * Ipython notebook

(org-add-link-type
 "ipynb"
 (lambda (path)
   (when (not (file-exists-p path))
     (with-temp-file path
       (insert "{
 \"cells\": [],
 \"metadata\": {},
 \"nbformat\": 4,
 \"nbformat_minor\": 0
}")))
   (start-process-shell-command "jupyter" nil (format "jupyter notebook %s" path))))


(load-file (expand-file-name "lisp/org-highlighter.el" tq-course-directory))
(define-key org-mode-map (kbd "M-h") 'org-highlighter/body)

;; * solution link

(org-add-link-type
 "solution"
 (lambda (label)
   (tq-check-internet)
   (with-current-directory
    tq-root-directory
    (unless (file-exists-p "solutions")
      (make-directory "solutions"))
    (with-current-directory
     "solutions"
     (if (file-exists-p label)
	 ;; we have the solution
	 (progn
	   (find-file (concat label "/" label ".org")))
       ;; no file
       (mygit (format "git clone %s:solutions/%s"
		      (techela-course-techela-server tq-current-course)
		      label))
       (find-file (concat label "/" label ".org")))))))
