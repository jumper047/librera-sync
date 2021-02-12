(defvar librera-sync--fb2-mode-length-coeff '((title . 1.4)))

(defvar librera-sync--fb2-mode-height-coeff '((title . 1.4)))

(defvar librera-sync--fb2-mode-line-prefix '((text . 2)
					     (cite . 7)
					     (stanza . 6))
  "Number of symbols to skip at first string.
Also includes suffix if needed (cite and stanza in fact has
3 symbols prefix and 3 symbols suffix).")

(defvar librera-sync--fb2-mode-wrap-prefix '((cite . 6)
					     (stanza . 6))
  "Number of symbols to skip at line's begin.
Also includes suffix if needed (cite and stanza in fact has
3 symbols prefix and 3 symbols suffix).")

(defvar librera-sync--fb2-mode-max-chars 33)
(defvar librera-sync--fb2-mode-max-lines 34)

(defvar librera-sync--fb2-mode-index '())


(defun librera-sync--fb2-mode-skip-page ()
  "Move cursor to last point of the page, starting from current pos."
  (interactive)

  (let ((curr-chars 0)
	(curr-word-chars 0)
	(curr-lines 0)
	(prev-tag nil)
	(curr-tag (get-text-property (point) 'fb2-tag))
	(tag (get-text-property (point) 'fb2-tag))
	(last-point nil)
	(tag-changed 'nil)
	(new-line 'nil)
	(max-lines librera-sync--fb2-mode-max-lines)
	)
    (while (and (char-after)
		(< curr-lines max-lines))
      (setq new-line 't)
      (while (and (char-after)
		  (< curr-chars librera-sync--fb2-mode-max-chars))
	(setq tag (get-text-property (point) 'fb2-tag)
	      tag-changed nil)
	(when (and tag
		   (not (eq tag curr-tag)))
	  (setq prev-tag curr-tag
		curr-tag tag
		tag-changed 't))
	;; Check if tag under point changed
	;; In Librera every part begans from page's start -
	;; so page must be braked if tag changed to "title"
	(if (and tag-changed
		 (eq tag 'title))
	    (setq curr-chars (1+ librera-sync--fb2-mode-max-chars)
		  curr-lines (1+ max-lines))
	  ;; Check if this is first title's newline - we need ignore it,
	  ;; so just move point one line down
	  (if (and (eq prev-tag nil)
		   (eq tag 'title)
		   (equal curr-lines 0))
	      (setq max-lines (+ max-lines (alist-get tag librera-sync--fb2-mode-height-coeff 1)))
	    )
 	  ;; First char in virtual line - check if we need to add some offset
	  (when (and (equal curr-chars 0)
		     (equal curr-word-chars 0))
	    (if (equal (char-before) 10);newline
		;; If first char in paragraph, add special offset
      		(setq curr-chars (alist-get tag librera-sync--fb2-mode-line-prefix 0))
	      (setq curr-chars (alist-get tag librera-sync--fb2-mode-wrap-prefix 0)))
	    ;; Skip first space in line if presented (assuming there are only one space possible in
	    ;; good book)
	    (if (equal (char-after) 32)
		(forward-char))
	    )
	  (cond ((equal (char-after) 10)	;newline
		 ;; Avoiding edge case:
		 ;; [almost string] [word not fitted to string][point] [newline]
		 ;; In that case i'll just add additional line to counter
		 (when (> (+ curr-chars 1 curr-word-chars) librera-sync--fb2-mode-max-chars)
		   (setq curr-lines (1+ curr-lines))
		   ;; (y-or-n-p "additional line")
		   )
		 ;; End the string if we have newline here
		 (setq curr-chars (1+ librera-sync--fb2-mode-max-chars)
		       curr-word-chars 0))
		((or (equal (char-after) 32)	;space
		     (and (equal (char-after) 45);"-" (for words like photo-video)
			  (not (member (char-before) '(10 32))))) ;I think it is more apropriate
					;check both before and after, but I don't know good way to jump
					;to next-after symbol. This check must be enough
		 ;; Add space if this is not first word
 		 (if (> curr-chars 0)
		     (setq curr-chars (+ (alist-get curr-tag librera-sync--fb2-mode-length-coeff 1)
					     curr-chars)))
		 ;; Try to add word to current string
		 (setq curr-chars (+ curr-chars
				     (* (alist-get
					 curr-tag librera-sync--fb2-mode-length-coeff 1)
					curr-word-chars)))
		 (if (<= curr-chars librera-sync--fb2-mode-max-chars)
		     (setq curr-word-chars 0
			   last-point (point)))
		 ;; (if (not (member (char-before) '(10 32))) ;let's hope previous char was part of
		 ;; 			;the word
		 ;;     (setq last-point (point)))
		 )
		(t
		 (setq curr-word-chars (1+ curr-word-chars))))
	  (setq new-line nil)
	  (forward-char)
	  )
	)
      (setq curr-lines (+ curr-lines (alist-get curr-tag librera-sync--fb2-mode-height-coeff 1)))
      (y-or-n-p (format "Lines: %s, curr-word-chars: %s chars in line: %s" curr-lines curr-word-chars curr-chars))
      (setq curr-chars 0)
      (when (and (> curr-word-chars 0)
		 last-point)
	(goto-char last-point)
	(setq curr-word-chars 0)
	(y-or-n-p "jumped back to pos")
 	)
      )
    ))


(defun librera-sync--fb2-mode-index-mark-buffer ()
  "Split buffer to pages and create index."

  (let ((max (point-max))
	(index '())
	(page-num 1)
	page-begin
	page-end)
    (while (< (point) max)
      (setq page-begin (point))
      (librera-sync--fb2-mode-skip-page)
      (setq page-end (point))
      (push (cons page-num (cons page-begin page-end)) index)
      (add-text-properties page-begin page-end (list 'librera-page page-num))
      (setq page-num (1+ page-num))
      (setq-local librera-sync--fb2-mode-index (reverse index))
      )
    )
  )

;; (defun librera-sync--goto-page (num)
;;   (alist-get num librera-sync--fb2-mode-index)
;;   )

(require 'pulse)
(defun librera-sync--fb2-mode-next-page ()
  (interactive)
  (let* ((curr-page (get-text-property (point) 'librera-page))
	 (next-page (1+ curr-page))
	 (beginend (alist-get next-page librera-sync--fb2-mode-index))
	 (begin (car beginend))
	 (end (cdr beginend)))
    (goto-char end)
    (pulse-momentary-highlight-region begin end)
    (message "Page %s" next-page)
    )
  )


