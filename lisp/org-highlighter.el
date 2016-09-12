;;; org-highlighter.el --- Highlight text in org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/scimax/org-highlighter.el

;; Version: 0.1.0
;; Keywords: org-mode, highlight
;; Package-Requires: ((hydra "0.13.2"))
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; org-highlighter provides a lightweight way to highlight text and put notes on
;; them in org-mode.

;; There is a hydra menu to make accessing all the commands below convenient:
;; `org-highlighter/body'. I suggest you bind it to a key like H-h.

;; You can select text, and run these commands to add highlighting to it:
;; `org-highlight' will prompt you for a color, and highlight with it.
;; These convenience functions skip the color prompt.
;; `org-highlight-yellow'
;; `org-highlight-blue'
;; `org-highlight-green'
;; `org-highlight-pink'

;; `org-highlight-toggle-mouse-highlight' makes it possible to highlight text in
;; green using the mouse. Use a prefix arg to select the color.

;; `org-highlight-note' will prompt you for a color, highlight the text and add
;; a Note to it as a tooltip. Notes are still experimental. You can edit a note
;; with `org-highlight-note-edit'.

;; You can list all the highlights with `org-highlight-list' in a buffer that
;; you can easily navigate to each one from.

;; You can clear a single highlight with `org-highlight-clear'.
;; Remove all the highlights with `org-highlight-clear-all'.

;; org-highlighter uses a local save-buffer-hook to update the data when you
;; save the buffer. It also uses local file variables to load the highlights
;; when you open the file.

;; Known issues:

;; - Highlights do not export in org-mode. They are not part of
;; org-syntax, so you would have to use a preprocessing hook function to make it
;; work.

;; - Highlights are not visible everywhere. So far they don't seem to work in:
;; - tables, or code-blocks.
;; - equations
;; - probably other things that are fontified by org-mode.

;; Highlights don't seem to copy and paste. This is related to the text properties.

(require 'hydra)

;;; Code:
(defgroup org-highlighter nil
  "Customization group for `org-highlighter'."
  :tag "org-highlighter")


(defcustom org-highlight-mouse-color "Darkolivegreen1"
  "Color to use for mouse highlighting."
  :type 'string
  :group org-highlighter)


;; * Highlight text and functions

(defun org-highlight-color-chooser ()
  "Interactively choose a color."
  (plist-get (get-text-property
	      0 'face
	      (completing-read
	       "Color: "
	       (progn
		 (save-selected-window
		   (list-colors-display))
		 (prog1
		     (with-current-buffer (get-buffer "*Colors*")
		       (mapcar (lambda (line)
				 (append (list line) (s-split " " line t)))
			       (s-split "\n" (buffer-string))))
		   (kill-buffer "*Colors*")))))
	     :background))


;;;###autoload
(defun org-highlight (beg end &optional color)
  "Highlight region from BEG to END with COLOR.
COLOR is selected from `org-highlight-color-chooser' when run interactively."
  (interactive "r") x
  (unless color
    (setq color (org-highlight-color-chooser)))
  (if (get-text-property (point) 'org-highlighter)
      ;; update color
      (let ((beg (previous-single-property-change (point) 'org-highlighter))
	    (end (next-single-property-change (point) 'org-highlighter)))
	(set-text-properties
	 beg end
	 `(font-lock-face (:background ,color)
			  org-highlighter t
			  rear-nonsticky t)))
    (set-text-properties
     beg end
     `(font-lock-face (:background ,color)
		      org-highlighter t
		      rear-nonsticky t)))
  ;; add a local hook
  (add-hook 'after-save-hook 'org-highlight-save nil t))


;;;###autoload
(defun org-highlight-list ()
  "Make a list of highlighted text in another buffer."
  (interactive)
  (let* ((links (mapcar
		 (lambda (entry)
		   (format "[[elisp:(progn (find-file-other-window \"%s\")(goto-char %s))][link]] %s\n"
			   (buffer-file-name)
			   (nth 0 entry)
			   (buffer-substring (nth 0 entry) (nth 1 entry))))
		 (org-highlight-get-highlights))))
    (if links
	(progn
	  (when (= (length (window-list)) 1)
	    (split-window-horizontally))
	  (switch-to-buffer-other-window "*highlights*") (org-mode)
	  (setq buffer-read-only nil)
	  (erase-buffer)
	  (insert "Click on link to jump to the position. Press q to quit.\n\n")

	  (dolist (link links)
	    (insert link))
	  (use-local-map (copy-keymap org-mode-map))
	  (local-set-key "q"
			 #'(lambda ()
			     (interactive)
			     (delete-window)))
	  (setq buffer-read-only t))
      (message "No highlights found."))))


;;;###autoload
(defun org-highlight-yellow ()
  "Highlight region in yellow."
  (interactive)
  (org-highlight (region-beginning) (region-end) "Yellow"))


;;;###autoload
(defun org-highlight-blue ()
  "Highlight region in blue."
  (interactive)
  (org-highlight (region-beginning) (region-end) "LightBlue"))


;;;###autoload
(defun org-highlight-pink ()
  "Highlight region in pink."
  (interactive)
  (org-highlight (region-beginning) (region-end) "Pink"))


;;;###autoload
(defun org-highlight-green ()
  "Highlight region in green."
  (interactive)
  (org-highlight (region-beginning) (region-end) "Darkolivegreen1"))


(defvar org-highlight-mouse nil
  "Stores if highlight mouse mode is active.")


;; create the advice for use later
(defadvice mouse-set-region (after org-highlight () disable)
  "Advice for mouse highlighting."
  (org-highlight (region-beginning) (region-end) org-highlight-mouse-color))


;;;###autoload
(defun org-highlight-toggle-mouse-highlight (arg)
  "Toggle mouse highlighting.
The default color is `org-highlight-mouse-color'. Use a prefix
ARG to select a different color and save it."
  (interactive "P")
  (when arg
    (setq org-highlight-mouse-color (org-highlight-color-chooser)))
  
  (if org-highlight-mouse
      ;; Turn it off
      (progn (ad-disable-advice 'mouse-set-region 'after 'org-highlight)
	     (ad-deactivate 'mouse-set-region)
	     (setq org-highlight-mouse nil)
	     (message "Mouse highlighting off."))
    (ad-enable-advice 'mouse-set-region 'after 'org-highlight)
    (ad-activate 'mouse-set-region)
    (setq org-highlight-mouse t)
    (message "Mouse highlighting on.")))


;;;###autoload
(defun org-highlight-note (beg end &optional color note)
  "Highlight selected text from BEG to END with COLOR.
Add NOTE to it as a tooltip. If no text is selected, insert \" note \"
and propertize it."
  (interactive "r")
  (when (not (region-active-p))
    (insert " note ")
    (re-search-backward "note")
    (set-mark (point))
    (re-search-forward "note"))

  (setq beg (region-beginning)
	end (region-end))
  (unless color (setq color (org-highlight-color-chooser)))
  (unless note (setq note (read-input "Note: ")))
  (org-highlight beg end color)
  (add-text-properties
   beg end
   (list 'help-echo note
	 'org-highlighter t)))


;;;###autoload
(defun org-highlight-note-edit (new-note)
  "Set tooltip of highlight at point to NEW-NOTE."
  (interactive (list (read-input "Note: " (get-text-property (point) 'help-echo))))
  (let* ((region (button-lock-find-extent (point) 'org-highlighter))
	 (beg (car region))
	 (end (cdr region)))
    (put-text-property beg end 'help-echo new-note)))


;;;###autoload
(defun org-highlight-clear ()
  "Clear highlight at point."
  (interactive)
  (when (get-text-property (point) 'org-highlighter)
    (set-text-properties
     (next-single-property-change (point) 'org-highlighter)
     (previous-single-property-change (point) 'org-highlighter)
     nil)))


;;;###autoload
(defun org-highlight-clear-all ()
  "Clear all highlighted text.
They are really deleted when you save the buffer."
  (interactive)
  (mapcar
   (lambda (entry)
     (let ((beg (nth 0 entry))
	   (end (nth 1 entry)))
       (set-text-properties
	beg end nil)))
   (org-highlight-get-highlights))
  (when (get-buffer "*highlights*")
    (kill-buffer "*highlights*")))


;;;###autoload
(defhydra org-highlighter (:color blue) "highlighter"
  ("b" org-highlight-blue "blue")
  ("g" org-highlight-green "Green")
  ("p" org-highlight-pink "Pink")
  ;; define as many special colors as you like.
  ("s" (org-highlight (region-beginning) (region-end) "Lightsalmon1") "Salmon")
  ("y" org-highlight-yellow "yellow")
  ("c" org-highlight "Choose color")
  ("n" (org-highlight-note (region-beginning) (region-end) "Thistle") "Note")
  ("N" org-highlight-note "Note (c)")
  ("m" org-highlight-toggle-mouse-highlight "Toggle mouse")
  ("e" org-highlight-note-edit "Edit note")

  ;; Grading/feedback options
  ("t" org-highlight-typo "Typo")
  ("f" org-highlight-feedback "Feedback note")
  
  ("l" org-highlight-list "List highlights")
  ("d" org-highlight-clear "Delete")
  ("D" org-highlight-clear-all "Delete All"))


(defun org-highlighter-menu ()
  "Add org-highlighter to the Org menu."
  (easy-menu-change
   '("Org") "Highlighter"
   '(["Highlight (B)" org-highlight-blue]
     ["Highlight (G)" org-highlight-green]
     ["Highlight (P)" org-highlight-pink]
     ["Highlight (Y)" org-highlight-yellow]
     ["Highlight note" org-highlight-note]
     ["List highlights" org-highlight-list]
     ["Delete highlight" org-highlight-clear]
     ["Delete highlights" org-highlight-clear-all])
   "Show/Hide")
  (easy-menu-change '("Org") "--" nil "Show/Hide"))

(add-hook 'org-mode-hook 'org-highlighter-menu)


;; * Save and load functions
(defun org-highlight-get-highlights ()
  "Scan buffer for list of highlighted regions.
These are defined only by the highlighted property. That means
adjacent highlighted regions will be merged into one region with
the color of the first one.

Returns a list of (beg end color note)."
  (save-excursion
    (goto-char (point-min))
    (let ((highlights '())
	  (p)
	  (beg)
	  (end)
          (note)
	  (color))
      ;; corner case of first point being highlighted
      (when (get-text-property (point) 'org-highlighter)
	(setq beg (point)
	      end (next-single-property-change (point) 'org-highlighter)
	      color (background-color-at-point)
	      help-echo (get-text-property (point) 'help-echo))
	(add-to-list 'highlights (list beg end color help-echo) t)
	(goto-char end))

      ;; Now the rest of the buffer
      (while (setq p (next-single-property-change (point) 'org-highlighter))
	(setq beg (goto-char p))
	(setq color (background-color-at-point))
        (setq note (get-text-property (point) 'help-echo))
	(setq end (next-single-property-change (point) 'org-highlighter))
	(when (and beg end)
	  (goto-char end)
	  (add-to-list 'highlights (list beg
					 end
					 color
					 note)
		       t)
	  (goto-char end)))
      highlights)))


(defun org-highlight-save ()
  "Save highlight information.
Data is saved in an org-section in the document."
  (save-excursion
    (goto-char (point-min))
    (unless (re-search-forward "^\\* org-highlighter data" nil 'mv)
      (insert "* org-highlighter data :noexport:
  :PROPERTIES:
  :VISIBILITY: folded
  :ID: org-highlighter-data
  :END:\nDo not delete this section. It stores information about the highlights in this document. Any information in this section may be deleted if you remove the highlights in this document.\n#+name: org-highlighter-data\n#+BEGIN_SRC emacs-lisp :results code value replace\n(org-highlight-get-highlights)\n#+END_SRC")

      (add-file-local-variable 'eval '(progn (require 'org-highlighter) (org-highlight-load))))
    (org-save-outline-visibility nil
      (org-babel-goto-named-src-block "org-highlighter-data")
      (org-babel-execute-src-block)
      (let ((after-save-hook '()))
	(save-buffer)))))


(defun org-highlight-load ()
  "Load and apply highlighted text."
  (interactive)
  (setq font-lock-extra-managed-props (delq 'help-echo font-lock-extra-managed-props))
  (org-babel-goto-named-result "org-highlighter-data")
  (let ((hls (read (org-element-property :value (org-element-context)))))
    (mapcar
     (lambda (entry)
       (let ((beg (nth 0 entry))
	     (end (nth 1 entry))
	     (color (nth 2 entry))
	     (help-echo (nth 3 entry)))
	 (set-text-properties
	  beg end
	  `(font-lock-face (:background ,color)
			   help-echo ,help-echo
			   org-highlighter t
			   rear-nonsticky t))))
     hls))
  (add-hook 'after-save-hook 'org-highlight-save nil t))

;; add the local var we use as safe so we don't get annoyed by permission to run
;; it.
(add-to-list 'safe-local-eval-forms
	     '(progn (require (quote org-highlighter)) (org-highlight-load)))


;; * Feedback functions
;;;###autoload
(defun org-highlight-typo ()
  "Add a typo highlight."
  (interactive)
  (let* ((r1 (progn (re-search-backward "\\<") (set-mark (point)) (point)))
	 (r2 (progn (re-search-forward "\\>") (point))))
    (org-highlight-note r1 r2 "PaleVioletRed1" "typo")))


;;;###autoload
(defun org-highlight-feedback ()
  "Add a feedback highlight."
  (interactive)
  (let (r1 r2 comment)
    (if (region-active-p)
	(setq r1 (region-beginning)
	      r2 (region-end))
      ;; No region, so we make one
      (setq  r1 (progn (re-search-backward "\\<") (set-mark (point)) (point))
	     r2 (progn (re-search-forward "\\>") (point))))

    (setq comment (read-input "Comment: "))
    (org-highlight-note r1 r2 "LightBlue1" comment)))

(provide 'org-highlighter)

;;; org-highlighter.el ends here


