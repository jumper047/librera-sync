(require 'cl-lib)
(require 'dash)

(defvar librera-sync-fb2-reader-debug 't)

(defvar-local librera-sync-fb2-debug-level 'char) ;char or word or page

(defun librera-sync--fb2-reader-debug-message (levels mess &rest formatters)
  (when (and librera-sync-fb2-reader-debug
	     (member librera-sync-fb2-debug-level levels))
    (let ((infomess (concat mess " (press any key to continue)"))
	  (key-pressed))
      (apply #'message infomess formatters)
      (setq librera-sync-fb2-debug-level
	    (if (equal (read-char) 99)
		'char
	      'word)))))

(defun librera-sync-fb2-reader-skip-page (previous-characters &optional previous-lines)
  (interactive "P")
  (setq previous-characters (or previous-characters 0))
  (setq previous-lines (or previous-lines 0))
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
			    (stanza . 0)))
	(paragraph-prefix-default 1.5)
	(line-prefix '((cite . 5)
		       (stanza . 5)))
	(line-prefix-default 0)
	(curr-lines (truncate previous-lines))
	(curr-chars previous-characters)
	(curr-word 0)
	(hyphen-flag nil)
	(multiple-hyphens-flag nil)
	;; first-char-after* flags used to avoid situations whe paragraph-prefix
	;; appended to string when parser start it's work from center of the string
	;; Not sure this is correct though
	(first-char-after-start 't)
	(first-word-after-start 't)
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
	    ;; Main goal of this flage is to prevent 'paragraph-started/ended
	    ;; from premature triggering; so it is safe set it to nil when
	    ;; last-tags (which used in paragraph detection) became non-nil
	    first-char-after-start (if last-tags nil t)
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
	    paragraph-started (and (or (member 'v tags-appears)
				       (member 'p tags-appears))
				   (not first-char-after-start))
	    paragraph-ended (and (or (member 'v tags-disappears)
				     (member 'p tags-disappears))
				 (not first-char-after-start))
	    title-started (member 'title tags-appears)
	    title-ended (member 'title tags-disappears))
      
      (when paragraph-started
	(setq curr-chars (* (+ curr-paragraphprefix curr-lineprefix)
			    curr-lengthstep)
	      paragraph-first-word 't)
	(librera-sync--fb2-reader-debug-message '(word char)
	 "paragrph started; cw: %s; chr %s; lns %s pprfx %s lprfx %s"
	 curr-word curr-chars curr-lines curr-paragraphprefix curr-lineprefix))

      (when title-ended
	(setq curr-lines (+ prev-heightstep curr-lines)
	      curr-chars 0
	      curr-word 0)
	(librera-sync--fb2-reader-debug-message '(word char)
	 "title ended; cw: %s; chr %s; lns %s hstep: %s"
	 curr-word curr-chars curr-lines
	 prev-heightstep))

      (cond (;new title not at the start of the page
	     ;(where it is current title obviously)
	     (and title-started (> curr-lines 0))
	     (librera-sync--fb2-reader-debug-message '(word char)
	      "title started; cw: %s; chr %s; lns %s"
	      curr-word curr-chars curr-lines)
	     (setq curr-chars 1
		   curr-lines maxlines))
	    (paragraph-ended
	     ;; At the end of the paragraph some tricky situation can appear:
	     ;; If last word before paragraph's end is also the only word on
	     ;; a new line (like prev line's end\nword.\nNext paragraph...)

	     ;; This case describes situation similiar to "space after word"
	     ;; but when paragraph ends after that word.
	     (if (and curr-word
		      (not multiple-hyphens-flag)
					;see explanation in hyphens section
					;actually here should be more complex logic
					;including word calculation (in case there
					;is only one word in paragprah; will impl
					;it if necessary)
		      (> (+ curr-chars curr-lengthstep curr-word) maxchars))
		      ;If there is at least one line available I just
		      ;add two lines - they'll became two lines or one line
		      ;and new page on paragraph end - it's fine.
		 (if (>= maxlines (+ (* 2 prev-heightstep) curr-lines))
		     (setq curr-lines (+ (* 2 prev-heightstep) curr-lines)
			   curr-chars 0
			   curr-word 0)
		   (setq curr-lines (+ (* 2 prev-heightstep) curr-lines)
			 ;; This is kinda tricky - in case last word in
			 ;; paragraph appears on next page, I'll set curr chars
			 ;; to maxchars to skip that line on next skip-page launch
			 curr-chars 0
			 curr-wod 0))
	       (setq curr-lines (+ prev-heightstep curr-lines)
		     curr-chars 0
		     curr-word 0))
	     (librera-sync--fb2-reader-debug-message
	      '(word char)
	      "paragraph ended; cw: %s; chr %s; lns %s"
	      curr-word curr-chars curr-lines))
	    (;space and word before it
	     (and (or (= curr-char 32)	;space
		      (and (= curr-char 10) curr-tags)) ;soft newline (inside tag)
		  (> curr-word 0))
	     (let ((separator (if (or first-word-after-start hyphen-flag paragraph-first-word)
				  0 curr-lengthstep)))
	       (if (and curr-word (> (+ curr-chars separator curr-word)
				     maxchars))
		   (if multiple-hyphens-flag
		       (progn (setq curr-lines (+ curr-heightstep curr-lines)
				    curr-chars 0
				    curr-word 0)
			      (librera-sync--fb2-reader-debug-message
			       '(word char)
			       "Multiple hyphens ended; cw: %s; chr %s; l ns %s"
			       curr-word curr-chars curr-lines))
		     (setq curr-lines (+ curr-heightstep curr-lines)
			   curr-chars (+ (* curr-lengthstep curr-lineprefix)
					 curr-word)
			   curr-word 0)
		     (librera-sync--fb2-reader-debug-message
		      '(word char)
		      "word ended; cw: %s; chr %s; l ns %s"
		      curr-word curr-chars curr-lines))
		 (setq curr-chars (+ curr-chars separator curr-word)
		       curr-word 0
		       paragraph-first-word nil)))
	     (setq hyphen-flag nil
		   multiple-hyphens-flag nil
		   first-word-after-start nil))
	    (;new line with empty-line or empty-line-special tags
	     (and (= curr-char 10) (or (member 'empty-line-special curr-tags)
				       (member 'empty-line curr-tags)))
	     ;; I'll ignore newline if it is first or last line on page
	     (unless (or (= curr-lines 0)
			 (<= maxlines (+ curr-heightstep curr-lines)))
	       (setq curr-lines (+ curr-heightstep curr-lines))
	       (librera-sync--fb2-reader-debug-message
		'(word char) "empty line; cw: %s; chr %s; lns %s"
		curr-word curr-chars curr-lines)
	       ))
	    (;hyphen inbetween two words
	     (and curr-tags curr-word (equal prev-char 45) ;hyphen
		  (not (member curr-char '(10 32))))
		  ;there is interesting corner case: word with multiple
		  ;hyphens is not splitting by rendered (for example something like
		  ;111-11-11---11-111)
	     (if hyphen-flag
		 (setq curr-word (1+ curr-word)
		       multiple-hyphens-flag 't)
	       (if (and curr-word (> (+ curr-chars curr-lengthstep curr-word)
				     maxchars))
		   (progn (setq curr-lines (+ curr-heightstep curr-lines)
				curr-chars (+ (* curr-lengthstep curr-lineprefix)
					      curr-word)
				curr-word 1
				hyphen-flag 't)
			  (librera-sync--fb2-reader-debug-message
			   '(word)
			   "hyphen; cw: %s; chr %s; lns %s"
			   curr-word curr-chars curr-lines))
		 (setq curr-chars (+ curr-chars curr-lengthstep curr-word)
		       curr-word 1
		       hyphen-flag 't))))
	    (;any character inside tag except space and newline
	     (and curr-tags (not (or (= curr-char 10) (= curr-char 32))))
	     (setq curr-word (+ curr-lengthstep curr-word))
	     (librera-sync--fb2-reader-debug-message
	      '(char)
	      "curr lengthcoeff: %s; word: %s; chars %s; lines %s"
	      curr-lengthstep curr-word curr-chars curr-lines)
	     ))
      (forward-char)
      (unless (char-after)
	(setq curr-lines (1+ maxlines))))
    ;; (backward-char)
    (if librera-sync-fb2-reader-debug (message "Chars before: %s; Lines before: %s" curr-chars curr-lines)
      (cons (1+ curr-chars) (- curr-lines maxlines)))))

(defun librera-sync-fb2-reader-pages ()
  (beginning-of-buffer)
  (let* ((chars-before 0)
	 (lines-before 0)
	(page-num 1)
	(pages (list (cons page-num (point)))))
    (while (char-after)
      (setq before (librera-sync-fb2-reader-skip-page chars-before lines-before)
	    chars-before (car before)
	    lines-before (cdr before)
	    page-num (1+ page-num))
      (save-excursion
	(backward-char chars-before)
	(push (cons page-num (point)) pages))
      )
    (message "pages: %s" page-num)
    (reverse pages)))


;; (setq-local librera-pages (librera-sync-fb2-reader-pages))
;; (goto-char (alist-get 49 librera-pages))
;; all ok till 164 and became worse to 168
;; 200-201 - just started issue
;; 675-677 anomaly
;; 679-680 - one line missing - anomaly at 678 th page
;; 705-706 - missing page
