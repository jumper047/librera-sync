(defun librera-sync--fb2-reader-skip-page ()
  (interactive)
  (let ((maxchars 33)
	(maxlines 33)
	(heightcoeff '(('title . 1.4)))
	(heightcoeff-default 1)
	(lengthcoeff '(('title . 1.4)))
	(lengthcoeff-default 1)
	(paragraph-prefix '(('title . 2)))
	(paragraph-prefix-default 1)
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
	prev-char
	curr-char
	paragraph-ended
	paragraph-started)
    (while (< curr-lines maxlines)
      (setq prev-char curr-char
	    curr-char (char-after)
	    prev-tags curr-tags
	    curr-tags (get-text-property (point) 'fb2-reader-tags)
	    parent-tag (cl-second curr-tags)
	    curr-lengthstep (alist-get parent-tag lengthcoeff lengthcoeff-default)
	    curr-heightstep (alist-get parent-tag heightcoeff heightcoeff-default)
	    curr-paragraphprefix (alist-get parent-tag paragraph-prefix paragraph-prefix-default)
	    paragraph-ended (and prev-tags (not curr-tags) (not just-started))
	    paragraph-started (and curr-tags (not prev-tags) (not just-started)))
      (when prev-tags
	(setq last-tags prev-tags))

      (when paragraph-started
	(setq curr-chars (* curr-paragraphprefix curr-lengthstep))
(y-or-n-p (format "add prefix because paragraph started; current word: %s; chars %s; lines %s" curr-word curr-chars curr-lines))
	)
      

      (cond (paragraph-ended
	     (if (and (>  curr-word 0)
		      (> (+ curr-chars curr-lengthstep curr-word) maxchars))
		 (setq curr-lines (+ curr-heightstep curr-lines))) 
	     (setq curr-lines (+ curr-heightstep curr-lines)
		   curr-chars 0
		   curr-word 0))
	    (;space and word before it
	     (and (or (= curr-char 32)	;space
		      (and (= curr-char 10) curr-tags)) ;soft newline (inside tag)
		  (> curr-word 0))
	     (let ((separator (if (or just-started hyphen-flag) 0 curr-lengthstep)))
	     (if (and (> curr-word 0) (> (+ curr-chars separator curr-word) maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars curr-word
			      curr-word 0)
			(y-or-n-p (format "new line because of space; current word: %s; chars %s; lines %s" curr-word curr-chars curr-lines))
			)
	       (setq curr-chars (+ curr-chars separator curr-word)
		     curr-word 0)))
	     (setq hyphen-flag nil
		   just-started nil))
	    (;new line with empty-line tag
	     (and (= curr-char 10) (member 'empty-line curr-tags))
	     (setq curr-lines (+ curr-heightstep curr-lines))
	     (y-or-n-p (format "empty tag;current word: %s; chars %s; lines %s" curr-word curr-chars curr-lines))
	     )
	    (;hyphen inbetween two words
	     (and curr-tags curr-word (equal prev-char 45) ;hyphen
		  (not (member curr-char '(10 32))))
	     (if (and (> curr-word 0) (> (+ curr-chars curr-lengthstep curr-word) maxchars))
		 (progn (setq curr-lines (+ curr-heightstep curr-lines)
			      curr-chars curr-word
			      curr-word 1)
			(y-or-n-p (format "in between current word: %s; chars %s; lines %s" curr-word curr-chars curr-lines))
			)
	       (setq curr-chars (+ curr-chars curr-lengthstep curr-word)
		     curr-word 1
		     hyphen-flag 't
		     ))
	     )
	    (;any character inside tag except space and newline
	     (and curr-tags (not (or (= curr-char 10) (= curr-char 32))))
	     (setq curr-word (+ curr-lengthstep curr-word))
      ;; (y-or-n-p (format "current word: %s; chars %s; lines %s" curr-word curr-chars curr-lines))	     
	     ))
      (forward-char))
    (backward-char (1+ curr-chars))
    )
  )
