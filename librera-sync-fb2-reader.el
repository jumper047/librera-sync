(require 'cl-lib)
(require 'dash)

(defun librera-sync--fb2-reader-skip-page (&optional debug)
  (interactive "P")
  (let ((maxchars 33)
	(maxlines 33)
	(heightcoeff '((title . 1.4)))
	(heightcoeff-default 1)
	(lengthcoeff '((title . 1.4)))
	(lengthcoeff-default 1)
	(paragraph-prefix '((title . 2)))
	(paragraph-prefix-default 1.5)
	(line-prefix '((cite . 5)))
	(line-prefix-default 0)
	(curr-lines 0)
	(curr-chars 0)
	(curr-word 0)
	(hyphen-flag nil)
	(just-started t)
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
	    parent-tag (cl-second curr-tags)
	    prev-heightstep curr-heightstep
	    curr-lengthstep (alist-get parent-tag lengthcoeff lengthcoeff-default)
	    curr-heightstep (alist-get parent-tag heightcoeff heightcoeff-default)

	    curr-paragraphprefix (alist-get parent-tag paragraph-prefix paragraph-prefix-default)
	    curr-lineprefix (alist-get parent-tag line-prefix line-prefix-default)
	    paragraph-ended (and prev-tags (not curr-tags) (not just-started))
	    paragraph-started (and curr-tags (not prev-tags) (not just-started))
	    title-started (and (not (member 'title last-tags)) (member 'title curr-tags))
	    title-ended (and (member 'title prev-tags) (not curr-tags)))
      
      (when prev-tags
	(setq last-tags prev-tags))

      (when paragraph-started
	(setq curr-chars (* (+ curr-paragraphprefix curr-lineprefix)
			    curr-lengthstep))
	(if debug (y-or-n-p (format "paragrph started; cw: %s; chr %s; lns %s"
				    curr-word curr-chars curr-lines))))

      (when title-ended
	     (setq curr-lines (+ prev-heightstep curr-lines)
		   curr-chars 0
		   curr-word 0)
	     (if debug (y-or-n-p (format "title ended; cw: %s; chr %s; lns %s"
					 curr-word curr-chars curr-lines))))

      (cond (;new title not at the start of the page (where it is current title obv.)
	     (and title-started (> curr-lines 0))
	     (if debug (y-or-n-p (format "title started; cw: %s; chr %s; lns %s"
					 curr-word curr-chars curr-lines)))
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
	     (if debug (y-or-n-p (format "paragraph ended; cw: %s; chr %s; lns %s"
					 curr-word curr-chars curr-lines))))
	    (;space and word before it
	     (and (or (= curr-char 32)	;space
		      (and (= curr-char 10) curr-tags)) ;soft newline (inside tag)
		  (> curr-word 0))
	     (let ((separator (if (or just-started hyphen-flag)
				  0 curr-lengthstep)))
	       (if (and (> curr-word 0) (> (+ curr-chars separator curr-word)
					   maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars (+ (* curr-lengthstep curr-lineprefix)
					    curr-word)
			      curr-word 0)
			(if debug
			    (y-or-n-p
			     (format "word ended; cw: %s; chr %s; l ns %s"
				     curr-word curr-chars curr-lines))))
	       (setq curr-chars (+ curr-chars separator curr-word)
		     curr-word 0)))
	     (setq hyphen-flag nil
		   just-started nil))
	    (;new line with empty-line tag
	     (and (= curr-char 10) (member 'empty-line curr-tags))
	     (setq curr-lines (+ curr-heightstep curr-lines))
	     (if debug (y-or-n-p (format "empty line; cw: %s; chr %s; lns %s"
					 curr-word curr-chars curr-lines))))
	    (;hyphen inbetween two words
	     (and curr-tags curr-word (equal prev-char 45) ;hyphen
		  (not (member curr-char '(10 32))))
	     (if (and (> curr-word 0) (> (+ curr-chars curr-lengthstep curr-word)
					 maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars curr-word
			      curr-word 1)
			(if debug (y-or-n-p (format
					     "hyphen; cw: %s; chr %s; lns %s"
					     curr-word curr-chars curr-lines))))
	       (setq curr-chars (+ curr-chars curr-lengthstep curr-word)
		     curr-word 1
		     hyphen-flag 't)))
	    (;any character inside tag except space and newline
	     (and curr-tags (not (or (= curr-char 10) (= curr-char 32))))
	     (setq curr-word (+ curr-lengthstep curr-word))
	     (if (and debug (> (car debug) 4))
		 (y-or-n-p (format
			    "curr lengthcoeff: %s; word: %s; chars %s; lines %s"
			    curr-lengthstep curr-word curr-chars curr-lines)))))
      (forward-char))
    (backward-char (1+ curr-chars))))
