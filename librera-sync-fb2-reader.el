(require 'cl-lib)
(require 'dash)

;; TODO: remove just-started flag (forgot why I added it)

(defvar librera-sync-fb2-reader-debug 't)

(defun librera-sync--fb2-reader-debug-message (mess &rest formatters)
       (when librera-sync-fb2-reader-debug
	 (let ((infomess (concat mess " (press any key to continue)")))
	   (apply #'message infomess formatters)
	   (read-char))))

(defun librera-sync-fb2-reader-skip-page (previous-characters)
  (interactive "P")
  (setq previous-characters (or previous-characters 0))
  (let ((maxchars 33)
	(maxlines (- 33 0.3))		;it is necessary in some cases
					;(at least when title's height
					;is 6 strings - then max.
					;length became 32.8 < 33 ->
					;one unwanted extra line added
					;to page)
	(heightcoeff '((title . 1.4)))
	(heightcoeff-default 1)
	(lengthcoeff '((title . 1.4)))
	(lengthcoeff-default 1)
	(paragraph-prefix '((title . 2)
			    (poem . 0)))
	(paragraph-prefix-default 1.5)
	(line-prefix '((cite . 5)
		       (poem . 5)))
	(line-prefix-default 0)
	(curr-lines 0)
	(curr-chars previous-characters)
	(curr-word 0)
	(hyphen-flag nil)
	;; Just started flag used to avoid situations whe paragraph-prefix
	;; appended to string when parser start it's work from center of the string
	;; Not sure this is correct though
	(just-started t)
	(paragraph-first-word nil)
	last-tags
	prev-tags
	curr-tags
	parent-tag
	curr-lengthcoeff
	curr-heightcoeff
	curr-paragraphprefix
	curr-lineprefix
	curr-lengthstep
	curr-heightstep
	max-curr-heightstep
	prev-heightstep
	prev-char
	curr-char
	paragraph-ended
	paragraph-started
	title-started
	title-ended)
    (while (< curr-lines maxlines)
      (setq prev-char curr-char
	    curr-char (char-after)
	    prev-tags curr-tags
	    curr-tags (get-text-property (point) 'fb2-reader-tags)
	    last-tags (if prev-tags prev-tags last-tags)
	    tags-appears (-difference curr-tags last-tags)
	    tags-disappears (-difference last-tags curr-tags)
	    curr-tag (cl-first curr-tags)
	    parent-tag (cl-second curr-tags)
	    prev-heightstep curr-heightstep
	    curr-lengthstep (alist-get (if (member 'p curr-tags)
					   parent-tag curr-tag)
				       lengthcoeff lengthcoeff-default)
	    curr-heightstep (alist-get (if (member 'p curr-tags)
					   parent-tag curr-tag)
				       heightcoeff heightcoeff-default)
	    curr-paragraphprefix (alist-get parent-tag paragraph-prefix paragraph-prefix-default)
	    curr-lineprefix (alist-get parent-tag line-prefix line-prefix-default)
	    paragraph-started (and (member 'p tags-appears) (not just-started))
	    paragraph-ended (and (member 'p tags-disappears) (not just-started))
	    title-started (member 'title tags-appears)
	    title-ended (member 'title tags-disappears))
      
      (when paragraph-started
	(setq curr-chars (* (+ curr-paragraphprefix curr-lineprefix)
			    curr-lengthstep)
	      paragraph-first-word 't)
	(librera-sync--fb2-reader-debug-message
	 "paragrph started; cw: %s; chr %s; lns %s"
	 curr-word curr-chars curr-lines))

      (when title-ended
	(setq curr-lines (+ prev-heightstep curr-lines)
	      curr-chars 0
	      curr-word 0)
	(librera-sync--fb2-reader-debug-message
	 "title ended; cw: %s; chr %s; lns %s hstep: %s"
	 curr-word curr-chars curr-lines
	 prev-heightstep))

      ;; Additional lines between cites, stanzas etc.
      (let ((changed-tags (append tags-appears tags-disappears)))
	(when  (and (or (member 'stanza changed-tags)
		      (member 'cite changed-tags))
		    (not just-started)
		    curr-tags
		    (not prev-tags)
		;; (>= maxlines (+ curr-heightstep curr-lines))
		)
	  (setq curr-lines (1+ curr-lines))
	  (librera-sync--fb2-reader-debug-message
	   "empty line between stanzas/cites; cw: %s; chr %s; lns %s"
	   curr-word curr-chars curr-lines)))
      

      (cond (;new title not at the start of the page
	     ;(where it is current title obviously)
	     (and title-started (> curr-lines 0))
	     (librera-sync--fb2-reader-debug-message
	      "title started; cw: %s; chr %s; lns %s"
	      curr-word curr-chars curr-lines)
	     (setq curr-chars 1
		   curr-lines maxlines))
	    (paragraph-ended
	     (if (and (>  curr-word 0)
		      (> (+ curr-chars curr-lengthstep curr-word) maxchars))
		 (if (>= maxlines (+ (* 2 curr-heightstep) curr-lines))
		     (setq curr-lines (+ (* 2 prev-heightstep) curr-lines)
			   curr-chars 0
			   curr-word 0)
		   (setq curr-lines (+ (* 2 prev-heightstep) curr-lines)
			 ;; This is kinda tricky - in case last word in
			 ;; paragraph appears on next page, I'll set curr chars
			 ;; to curr word to move cursor back to the start
			 ;; of the string after while loop
			 ;; exits
			 curr-chars curr-word))
	       (setq curr-lines (+ prev-heightstep curr-lines)
		     curr-chars 0
		     curr-word 0))
	     (librera-sync--fb2-reader-debug-message
	      "paragraph ended; cw: %s; chr %s; lns %s"
	      curr-word curr-chars curr-lines))
	    (;space and word before it
	     (and (or (= curr-char 32)	;space
		      (and (= curr-char 10) curr-tags)) ;soft newline (inside tag)
		  (> curr-word 0))
	     (let ((separator (if (or just-started hyphen-flag paragraph-first-word)
				  0 curr-lengthstep)))
	       (if (and (> curr-word 0) (> (+ curr-chars separator curr-word)
					   maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars (+ (* curr-lengthstep curr-lineprefix)
					    curr-word)
			      curr-word 0)
			(librera-sync--fb2-reader-debug-message
			 "word ended; cw: %s; chr %s; l ns %s"
			 curr-word curr-chars curr-lines))
	       (setq curr-chars (+ curr-chars separator curr-word)
		     curr-word 0
		     paragraph-first-word nil)))
	     (setq hyphen-flag nil
		   just-started nil))
	    (;new line with empty-line tag
	     (and (= curr-char 10) (member 'empty-line curr-tags))
	     (setq curr-lines (+ curr-heightstep curr-lines))
	     (librera-sync--fb2-reader-debug-message
	      "empty line; cw: %s; chr %s; lns %s"
	      curr-word curr-chars curr-lines))
	    (;hyphen inbetween two words
	     (and curr-tags curr-word (equal prev-char 45) ;hyphen
		  (not (member curr-char '(10 32))))
	     (if (and (> curr-word 0) (> (+ curr-chars curr-lengthstep curr-word)
					 maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars curr-word
			      curr-word 1)
			(librera-sync--fb2-reader-debug-message
			 "hyphen; cw: %s; chr %s; lns %s"
			 curr-word curr-chars curr-lines))
	       (setq curr-chars (+ curr-chars curr-lengthstep curr-word)
		     curr-word 1
		     hyphen-flag 't)))
	    (;any character inside tag except space and newline
	     (and curr-tags (not (or (= curr-char 10) (= curr-char 32))))
	     (setq curr-word (+ curr-lengthstep curr-word))
	     (librera-sync--fb2-reader-debug-message
	      "curr lengthcoeff: %s; word: %s; chars %s; lines %s"
	      curr-lengthstep curr-word curr-chars curr-lines)
	     ))
      (forward-char)
      (unless (char-after)
	(setq curr-lines (1+ maxlines))))
    ;; (backward-char)
    (if librera-sync-fb2-reader-debug (message "Chars before: %s" curr-chars)
      (1+ curr-chars))))

(defun librera-sync-fb2-reader-pages ()
  (beginning-of-buffer)
  (let* ((chars-before 0)
	(page-num 1)
	(pages (list (cons page-num (point)))))
    (while (char-after)
      (setq chars-before (librera-sync-fb2-reader-skip-page chars-before)
	    page-num (1+ page-num))
      (save-excursion
	(backward-char chars-before)
	(push (cons page-num (point)) pages)))
    (message "pages: %s" page-num)
    (reverse pages)))


;; (setq-local librera-pages (librera-sync-fb2-reader-pages))
;; (goto-char (alist-get 49 librera-pages))
