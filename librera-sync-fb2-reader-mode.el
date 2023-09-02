;;; librera-sync-fb2-reader-mode.el --- FB2-reader support for librera-sync -*- lexical-bindings: t; -*-

;; Copyright (c) 2023 Dmitriy Pshonko <jumper047@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; fb2-reader related functions for librera-sync

;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'pulse)
(require 'subr-x)

(defgroup librera-sync-fb2-reader nil
  "Settings related to FB2-reader mode."
  :group 'librera-sync
  :prefix "librera-sync-fb2-reader")

(defcustom librera-sync-fb2-reader-mode-page-width 27
  "Max number of characters in the line on Android device."
  :group 'librera-sync-fb2-reader-mode
  :type '(number))

(defcustom librera-sync-fb2-reader-mode-page-height 27
  "Number of the lines on a page on Android device."
  :group 'librera-sync-fb2-reader-mode
  :type '(number))

(defcustom librera-sync-fb2-reader-mode-highlight-page 't
  "Highlight page boundaries when changing location in currently opened book."
  :group 'librera-sync-fb2-reader-mode
  :type '(boolean))

(defvar librera-sync-fb2-reader-mode-index-file-name "fb2-reader-mode-indexes.el"
  "Name of the file which contains indexes for previously opened files.")
(defvar-local librera-sync-fb2-reader-mode-index nil
  "List with the positions of the beginnings of the pages.")

(defvar librera-sync-fb2-reader-mode-debug 'nil
  "If t various debugging messages will be printed during index calculation.")
(defvar-local librera-sync-fb2-reader-mode-debug-level 'char ;char or word or page
  "Tune up level of debugging currently needed.")

(defvar librera-sync-fb2-reader-mode--curr-pos 0
  "Current position in the buffer.")

(defun librera-sync-fb2-reader-mode-index-file ()
  "Get path to the file containing indexes for fb2 books."
  (expand-file-name librera-sync-fb2-reader-mode-index-file-name librera-sync-cache-directory))

(defun librera-sync--fb2-reader-mode-debug-message (levels mess &rest formatters)
  "Prints debug message if current level is needed, and waits until key pressed.

LEVELS is the list with debug levels, required to trigger debug message.
MESS is the message, FORMATTERS are formatters as for `format' command."
  (when (and librera-sync-fb2-reader-mode-debug
             (member librera-sync-fb2-reader-mode-debug-level levels))
    (let ((infomess (concat mess " (press any key to continue)"))
          (key-pressed))
      (apply #'message infomess formatters)
      (setq librera-sync-fb2-reader-mode-debug-level
            (if (equal (read-char) 99)
                'char
              'word)))))

(defun librera-sync-fb2-reader-mode--skip-page (previous-characters &optional previous-lines)
  "Skip the page as if it was displayed in Librera Reader."
  (interactive "P")
  (setq previous-characters (or previous-characters 0))
  (setq previous-lines (or previous-lines 0))
  (let ((maxchars librera-sync-fb2-reader-mode-page-width)
        ;;It is necessary in some cases (at least when title's height is 6 strings -
        ;;then max. length became 32.8 < 33 -> one unwanted extra line added to page)
        (maxlines (- librera-sync-fb2-reader-mode-page-height 0.3))
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
        ;; first-char-after* flags used to avoid situations when paragraph-prefix
        ;; appended to string when parser start it's work from center of the string
        ;; Not sure this is correct though
        (first-char-after-start 't)
        (first-word-after-start 't)
        (paragraph-first-word nil)
        last-tags
        prev-tags
        curr-tags
        parent-tag
        tags-appears
        tags-disappears
        curr-tag
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
        (librera-sync--fb2-reader-mode-debug-message '(word char)
                                                     "paragrph started; cw: %s; chr %s; lns %s pprfx %s lprfx %s"
                                                     curr-word curr-chars curr-lines curr-paragraphprefix curr-lineprefix))

      (when title-ended
        (setq curr-lines (+ prev-heightstep curr-lines)
              curr-chars 0
              curr-word 0)
        (librera-sync--fb2-reader-mode-debug-message '(word char)
                                                     "title ended; cw: %s; chr %s; lns %s hstep: %s"
                                                     curr-word curr-chars curr-lines
                                                     prev-heightstep))

      (cond (;;new title not at the start of the page
             ;;(where it is current title obviously)
             (and title-started (> curr-lines 0))
             (librera-sync--fb2-reader-mode-debug-message '(word char)
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
                         curr-word 0))
               (setq curr-lines (+ prev-heightstep curr-lines)
                     curr-chars 0
                     curr-word 0))
             (librera-sync--fb2-reader-mode-debug-message
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
                              (librera-sync--fb2-reader-mode-debug-message
                               '(word char)
                               "Multiple hyphens ended; cw: %s; chr %s; l ns %s"
                               curr-word curr-chars curr-lines))
                     (setq curr-lines (+ curr-heightstep curr-lines)
                           curr-chars (+ (* curr-lengthstep curr-lineprefix)
                                         curr-word)
                           curr-word 0)
                     (librera-sync--fb2-reader-mode-debug-message
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
               (librera-sync--fb2-reader-mode-debug-message
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
                          (librera-sync--fb2-reader-mode-debug-message
                           '(word)
                           "hyphen; cw: %s; chr %s; lns %s"
                           curr-word curr-chars curr-lines))
                 (setq curr-chars (+ curr-chars curr-lengthstep curr-word)
                       curr-word 1
                       hyphen-flag 't))))
            (;any character inside tag except space and newline
             (and curr-tags (not (or (= curr-char 10) (= curr-char 32))))
             (setq curr-word (+ curr-lengthstep curr-word))
             (librera-sync--fb2-reader-mode-debug-message
              '(char)
              "curr lengthcoeff: %s; word: %s; chars %s; lines %s"
              curr-lengthstep curr-word curr-chars curr-lines)))
      (forward-char)
      (unless (char-after)
        (setq curr-lines (1+ maxlines))))
    ;; (backward-char)
    (if librera-sync-fb2-reader-mode-debug (message "Chars before: %s; Lines before: %s" curr-chars curr-lines)
      (cons (1+ curr-chars) (- curr-lines maxlines)))))

(defun librera-sync-fb2-reader-mode-create-index ()
  "Return list with points of the every page's start."
  (save-excursion
    (goto-char (point-min))
    (let* ((chars-before 0)
           (lines-before 0)
           (pages (list (point)))
           before)
      (while (char-after)
        (setq before (librera-sync-fb2-reader-mode--skip-page chars-before lines-before)
              chars-before (car before)
              lines-before (cdr before))
        (save-excursion
          (backward-char chars-before)
          (push (point) pages)))
      ;; Delete last entry (every index entry is number of the page
      ;; and place where it starts, so last one will point to the end
      ;; of the last page, which is unwanted)
      (reverse pages))))

(defun librera-sync-fb2-reader-mode--indexes ()
  "Read index from cache."
  (if (f-exists-p (librera-sync-fb2-reader-mode-index-file))
      (with-temp-buffer
        (insert-file-contents (librera-sync-fb2-reader-mode-index-file))
        (goto-char (point-min))
        (read (current-buffer)))))

(defun librera-sync-fb2-reader-mode--write-indexes (indexes)
  "Write INDEXES to the cache."
  (with-temp-file (librera-sync-fb2-reader-mode-index-file)
    (set-buffer-file-coding-system 'utf-8)
    (insert ";; librera-sync-fb2-reader-mode.el -- sync position in fb2-reader-mode buffer with Librera Reader ")
    (insert "Don't edit this file manually!\n")
    (insert "\n")
    (insert (prin1-to-string indexes))))

(defun librera-sync-fb2-reader-mode-update-index (filename index)
  "Save INDEX for book FILENAME."
  (let ((indexes (remove filename (librera-sync-fb2-reader-mode--indexes)))
        (index-entry (list fb2-reader-file-name
                           (cons 'mtime (current-time))
                           (cons 'index index))))
    (push index-entry indexes)
    (librera-sync-fb2-reader-mode--write-indexes indexes)))

(defun librera-sync-fb2-reader-mode-load-index-actual (filename)
  "Load index for FILENAME if index is actual, return nil otherwise."
  (let* ((index-entry (alist-get filename (librera-sync-fb2-reader-mode--indexes) nil nil 'equal))
         (index-time (alist-get 'mtime index-entry))
         (fb2-reader-cache-time (fb2-reader-cache-creation-time filename)))
    (if (time-less-p fb2-reader-cache-time index-time)
        (alist-get 'index index-entry))))

(defun librera-sync-fb2-reader-mode--save-pos-maybe ()
  "Save position if visible area changed."
  (save-excursion
    (move-to-window-line 0)
    (when (not (eq librera-sync-fb2-reader-mode--curr-pos (point)))
      (setq librera-sync-fb2-reader-mode--curr-pos (point))
      (librera-sync-save))))

(defun librera-sync-fb2-reader-mode-prepare ()
  "Add librera advice to doc-view."
  (if-let ((index (librera-sync-fb2-reader-mode-load-index-actual fb2-reader-file-name)))
      (setq librera-sync-fb2-reader-mode-index index)
    (setq librera-sync-fb2-reader-mode-index (librera-sync-fb2-reader-mode-create-index))
    (librera-sync-fb2-reader-mode-update-index fb2-reader-file-name librera-sync-fb2-reader-mode-index))
  (add-hook 'post-command-hook #'librera-sync-fb2-reader-mode--save-pos-maybe nil 't))

(defun librera-sync-fb2-reader-mode-clean ()
  "Add librera advice to doc-view."
  (remove-hook 'post-command-hook #'librera-sync-fb2-reader-mode--save-pos-maybe 't))

(defun librera-sync-fb2-reader-mode-set-pos (position)
  "Set POSITION in doc-view buffer."
  (let* ((page-num (round (* position (length librera-sync-fb2-reader-mode-index))))
         (page-start (nth page-num librera-sync-fb2-reader-mode-index))
         (page-end (or (nth (1+ page-num) librera-sync-fb2-reader-mode-index) (window-end))))
    (goto-char page-start)
    (recenter-top-bottom 'top)
    (when librera-sync-fb2-reader-mode-highlight-page
      (pulse-momentary-highlight-region page-start page-end))))

(defun librera-sync-fb2-reader-mode-current-pos ()
  "Get current position in `fb2-reader-mode' buffer."
  (let* ((start-idx 0)
         (end-idx (length librera-sync-fb2-reader-mode-index))
         (curr-pos (point))
         middle-idx
         middle-item)
    (while (> (- end-idx start-idx) 1)
      (setq middle-idx (+ start-idx (/ (- end-idx start-idx) 2))
            middle-item (nth middle-idx librera-sync-fb2-reader-mode-index))
      (if (> curr-pos middle-item)
          (setq start-idx middle-idx)
        (setq end-idx middle-idx)))
    (/ (float start-idx) (length librera-sync-fb2-reader-mode-index))))

(defun librera-sync-fb2-reader-mode-book-name ()
  "Get current book name."
  (f-filename fb2-reader-file-name))

(defun librera-sync-fb2-reader-mode-book-path ()
  "Get path to current book."
  nil)

(provide 'librera-sync-fb2-reader-mode)

;;; librera-sync-fb2-reader-mode.el ends here
