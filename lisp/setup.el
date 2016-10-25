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

  (goto-char (point-min))
  (gb-set-filetag "EMAIL" (gethash "user-mail-address" (tq-read-user-data)))
  (gb-set-filetag "AUTHOR" (gethash "user-full-name" (tq-read-user-data)))

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
	       ("userid" (gethash "user-mail-address" (tq-read-user-data))))))
	 (mygit "git branch --set-upstream-to=origin/master"))))

    (with-current-directory
     student-repo-dir
     (mygit "git fetch src")
     (when (not (string= "" (shell-command-to-string
			     "git status --porcelain")))
       ;; There are some local changes. We commit them, pull,
       ;; and commit merges if there are any
       (mygit "git add *")
       (mygit "git commit -am \"my changes\"") 
       (mygit "git merge src/master")
       (mygit "git commit -am \"commit post pull from src\""))

     (mygit "git pull origin master")
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


;; * get setup hook to auto-update


(let ((preload (expand-file-name "user/preload.el" scimax-dir)))
  (unless (file-exists-p preload)
    (with-temp-file preload
      (insert ";; Do not delete this. It was added by techela.\n")
      (insert
       (prin1-to-string '(defvar scimax-preload-loaded nil
			   "Variable of whether preload has been run already."))) 
      (lispy-multiline)
      (insert "\n\n")
      (insert
       (prin1-to-string '(when (not scimax-preload-loaded)
			   (let ((default-directory scimax-dir))
			     (when (not (string= "" (shell-command-to-string "git status --porcelain")))
			       (shell-command "git add *")
			       (shell-command "git commit -am \"commiting scimax.\""))
			     (shell-command "git pull origin master")
			     (shell-command "git submodule init")
			     (shell-command "git submodule update"))))) 
      (lispy-multiline)
      (buffer-string))
    (with-current-directory scimax-dir
			    (when (not (string= "" (shell-command-to-string
						    "git status --porcelain")))
			      (shell-command "git add *")
			      (shell-command "git commit -am \"commiting scimax.\"")) 
			    (shell-command "git pull origin master")
			    (shell-command "git submodule update"))))



(defun autopep8 ()
  "Replace Python code block contents with autopep8 corrected code."
  (interactive)
  (unless (executable-find "autopep8")
    (if (executable-find "pip")
	(shell-command "python -c \"import pip; pip.main(['install','autopep8'])\"")
      (shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['-U','autopep8'])\"")))
  (let* ((src (org-element-context))
	 (beg (org-element-property :begin src))
	 (value (org-element-property :value src))) 
    (save-excursion
      (goto-char beg)
      (search-forward value)
      (shell-command-on-region
       (match-beginning 0)
       (match-end 0)
       "autopep8 -a -a -" nil t))))


;; set to nil so we can edit with special entities on.
(setq org-catch-invisible-edits nil)

;; * pycheck*
(defvar pylint-options
  '()
  "List of options to use with pylint.")

(setq pylint-options
      '("-r no "			 ; no reports
	;; we are not usually writing programs where it
	;; makes sense to be too formal on variable
	;; names.
	"--disable=invalid-name "
	;; don't usually have modules, which triggers
	;; this when there is not string at the top
	"--disable=missing-docstring "
	;; superfluous-parens is raised with print(),
	;; which I am promoting for python3
	;; compatibility.
	"--disable=superfluous-parens "	;

	;; these do not seem important for my work.
	"--disable=too-many-locals "	;

	;; this is raised in solving odes and is
	;; unimportant for us.
	"--disable=unused-argument "	;
	"--disable=unused-wildcard-import "
	"--disable=redefined-outer-name "
	;; this is triggered a lot from fsolve
	"--disable=unbalanced-tuple-unpacking "
	;; these are really annoying with how we use jasp
	"--disable=wildcard-import "
	"--disable=redefined-builtin "
	;; I dont mind semicolon separated lines
	"--disable=multiple-statements "
	;; pylint picks up np.linspace as a no-member error. That does not make sense.
	"--disable=no-member "
	"--disable=wrong-import-order "
	"--disable=unused-import "))




(defun pylint ()
  "Run pylint on a source block.
Opens a buffer with links to what is found. This function installs pylint if needed."
  (interactive)
  (let ((eop (org-element-at-point))
	(temporary-file-directory ".")
        (cb (current-buffer))
	(n) ; for line number
	(cn) ; column number
	(content) ; error on line
	(pb "*pylint*") 
	(link)
	(tempfile))

    (unless (executable-find "pylint")
      (if (executable-find "pip")
	  (shell-command "python -c \"import pip; pip.main(['install','pylint'])\"")
	(shell-command "python -c \"from setuptools.command import easy_install; easy_install.main(['pylint'])\"")))

    ;; rm buffer if it exists
    (when (get-buffer pb) (kill-buffer pb))

    ;; only run if in a python code-block
    (when (and (eq 'src-block (car eop))
	       (string= "python" (org-element-property :language eop)))

      ;; tempfile for the code
      (setq tempfile (make-temp-file "org-py-check" nil ".py"))
      ;; create code file
      (with-temp-file tempfile
	(insert (org-element-property :value eop)))
      
      ;; pylint
      (let ((status (shell-command
		     (concat
		      "pylint "
		      (mapconcat 'identity pylint-options " ")
		      " "
		      ;; this is the file to check.
		      (file-name-nondirectory tempfile))))

	    ;; remove empty strings
	    (output (delete "" (split-string
				(with-current-buffer "*Shell Command Output*"
				  (buffer-string)) "\n"))))

	;; also remove this line so the output is empty if nothing
	;; comes up
	(setq output (delete
		      "No config file found, using default configuration"
		      output))

	(kill-buffer "*Shell Command Output*")
	(if output
	    (progn
	      (set-buffer (get-buffer-create pb))
	      (insert (format "\n\n* pylint (status = %s)\n" status))
	      (insert "pylint checks your code for errors, style and convention. Click on the links to jump to each line.

")

	      (dolist (line output)
		;; pylint gives a line and column number
		(if
		    (string-match "[A-Z]:\\s-+\\([0-9]*\\),\\s-*\\([0-9]*\\):\\(.*\\)"
				  line)
		    (let ((line-number (match-string 1 line))
			  (column-number (match-string 2 line))
			  (content (match-string 3 line)))

		      (setq link (format "[[elisp:(progn (switch-to-buffer-other-window \"%s\")(goto-char %s)(forward-line %s)(forward-line 0)(forward-char %s))][%s]]\n"
					 cb
					 (org-element-property :begin eop)
					 line-number
					 column-number
					 line)))
		  ;; no match, just insert line
		  (setq link (concat line "\n")))
		(insert link)))
	  (message "pylint was clean!")))

      (when (get-buffer pb)
	;; open the buffer
	(switch-to-buffer-other-window pb)
	(goto-char (point-min))
	(insert "Press q to close the window\n")
	(org-mode)
	(org-cycle '(64))  ; open everything
	;; make read-only and press q to quit
	(setq buffer-read-only t)
	(use-local-map (copy-keymap org-mode-map))
	(local-set-key "q" #'(lambda () (interactive) (kill-buffer)))
	(switch-to-buffer-other-window cb))
      ;; final cleanup and delete file
      (delete-file tempfile))))


