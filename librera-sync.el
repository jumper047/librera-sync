;;; librera-sync.el --- Sync document's position with Librera Reader for Android  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Dmitriy Pshonko <jumper047@gmail.com>

;; Author: Dmitriy Pshonko <jumper047@gmail.com>
;; URL: https://github.com/jumper047/librera-sync
;; Keywords: sync ebook epub pdf
;; Version: 0.1
;; Package-Requires: ((emacs "26.0"))

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

;; 


;;; Code:

(require 'f)
(require 'json)

(defgroup librera-sync nil
  "A group for librera-sync related customizations"
  :group 'applications
  :prefix "librera-sync-"
  )

(defcustom librera-sync-directory nil
  "Path to Librera folder."
  :type '(directory)
  :group 'librera-sync)

(defcustom librera-sync-device-name (system-name)
  "Device name (should be unique across Librera instances)."
  :type '(string)
  :group 'librera-sync)

(defcustom librera-sync-interval 60
  "Sync interval in seconds."
  :type '(number)
  :group 'librera-sync)

(defcustom librera-sync-update-method 'inotify
  "Method to check progress update."
  :group 'librera-sync
  :type '(choice
	  (const :tag "Watching file changes with inotify" inotify)
	  (const :tag "Check updates by timer" timer)))

(defvar librera-sync-timer nil
  "Timer for interval progress checking.")

(defvar librera-sync-watchers '()
  "List of watch items from inotify.")

(defvar librera-sync-tracked-filenames '()
  "List of files currently opened.")

(defun librera-sync--device-names ()
  "Get list of available device names."
  (unless librera-sync-directory
    (error "Librera directory not set"))
  (let ((devnames '())
	(devdirs (f-join librera-sync-directory "profile.Librera")))
    (dolist (devdir (directory-files  devdirs nil "device.*") devnames)
      (setq devnames (cons (car (cdr (split-string (f-filename devdir)
						   "\\."))) devnames)))))

(defun librera-sync--device-dir (devname)
  "Get full path to conig dir of device DEVNAME."
  (unless librera-sync-directory (error "Librera directory not set"))
  (let ((devdir (f-join librera-sync-directory "profile.Librera" (concat "device." devname))))
    (if (f-exists-p devdir)
	devdir)))

(defun librera-sync--curr-device-dir ()
  "Get paths to all device directories as list."
  (unless librera-sync-directory
    (error "Librera directory not set"))
  (f-join librera-sync-directory "profile.Librera"
	  (concat "device." librera-sync-device-name)))

(defun librera-sync--curr-device-progr-json ()
  "Get path to currrent device's ap-Progress.json file."
  (f-join (librera-sync--curr-device-dir) "app-Progress.json")
  )
 
(defun librera-sync--time-msecs ()
  "Get time in format used by Librera."
  (round (* 1000 (float-time))))


(defun librera-sync--write-pos (position docname)
  "Save DOCNAME's POSITION to Librera."
  ;; Create directory and empty json file if there is noone already
  (if (not (f-exists? (librera-sync--curr-device-dir)))
      (make-directory (librera-sync--curr-device-dir)))

  (if (not (f-exists? (librera-sync--curr-device-progr-json)))
      (with-temp-file (librera-sync--curr-device-progr-json)
	(insert (json-encode (list (cons docname
					 (list (cons "cp" json-false)
					       (cons "d" 0)
					       (cons "dc" json-false)
					       (cons "dp" json-false)
					       (cons "lk" 2)
					       (cons "p" position)
					       (cons "s" 120)
					       (cons "sp" json-false)
					       (cons "t" 0)
					       (cons "x" 0)
					       (cons "y" 0)
					       (cons "z" 100)))))))
    (let* ((json-object-type 'hash-table)
	   (json-array-type 'list)
	   (json-key-type 'string)
	   (app-progress (json-read-file (librera-sync--curr-device-progr-json)))
	   (book-params (gethash docname app-progress)))

      (puthash "p" position book-params)
      (puthash "t" (librera-sync--time-msecs) book-params)
      (with-temp-file (librera-sync--curr-device-progr-json)
	(insert (json-encode app-progress))))))

(defun librera-sync-save ()
  "Save current position to Librera profile."

  (librera-sync--write-pos
   (librera-sync--current-pos) (buffer-name))
  )

(defun librera-sync--read-progress-from (devname)
  "Read progress from device DEVNAME config.
Returns hashtable with Librera's json structure"
  (when-let* ((json-object-type 'hash-table)
	      (json-array-type 'list)
	      (json-key-type 'string)
	      (devpath (librera-sync--device-dir devname))
	      (progress-path (f-join devpath
				     "app-Progress.json")))
    (json-read-file progress-path)))

(defun librera-sync--read-all-pos ()
  "Get latest positions for all tracked buffers.
Returns hash table with list '(POSITION TIME FILE)"
  (let ((positions (make-hash-table :test 'equal)))
    (dolist (devname
	     (librera-sync--device-names)
	     positions)
      (dolist (trbuf librera-sync-tracked-filenames)
	(when-let* ((progress-js (librera-sync--read-progress-from devname))
		    (bookparams (gethash trbuf progress-js))
		    (npos (gethash "p" bookparams))
		    (ntime (gethash "t" bookparams))
		    (trbufval (gethash trbuf positions '(0 0 "")))
		    (pos (car trbufval))
		    (time (car (cdr trbufval)))
		    (fresh (< time ntime)))
	  (puthash trbuf (list npos ntime devname) positions))))))
  

(defun librera-sync--read-pos-for (docname &optional devname)
  "Get position for certain DOCNAME.
If optional DEVNAME set returns position that device
Returns (position time devname)"
  (if devname
      (when-let* ((progress-js (librera-sync--read-progress-from devname))
		  (doc-params (gethash docname progress-js))
		  (position (gethash "p" doc-params))
		  (time (gethash "t" doc-params)))
	(list position time devname))
    (gethash docname (librera-sync--read-all-pos))))

(defun librera-sync--schedule-update-all ()
  "Schedule update for all buffers with new positions.
Function checks new positions for all buffers from other Librera instances."
  (let* ((positions (librera-sync--read-all-pos)))
    (dolist (filename librera-sync-tracked-filenames)
      (when-let* ((filepos (gethash filename positions))
		  (pos (car filepos))
		  (sname (car (nthcdr 2 filepos)))
		  (notsame (not (string-equal sname librera-sync-device-name))))
	(with-current-buffer filename
	  (setq-local librera-sync--new-position pos)
	  (setq-local librera-sync--update-source sname)
	  (add-hook 'post-command-hook 'librera-sync--update-cur-buffer 0 't)
	  ;; Kinda hacky - check if current buffer active and update it immediately
	  (if (eq (current-buffer) (window-buffer (selected-window)))
	      (librera-sync--update-cur-buffer)))))))

(defun librera-sync--update-cur-buffer ()
  "Update current buffer if it has deferred position."
  (if librera-sync--new-position
      (librera-sync--set-pos librera-sync--new-position))
  (message "Position loaded from %S" librera-sync--update-source)
  (setq-local librera-sync--new-position nil)
  (setq-local librera-sync--update-source nil)
  (remove-hook 'post-command-hook 'librera-sync--update-cur-buffer 't))


;; Inotify watcher functions

(defun librera-sync--start-watching ()
  "Watch device files via inotify."
  (dolist (name (librera-sync--device-names))
    (unless (string-equal name librera-sync-device-name)
      (push (file-notify-add-watch (librera-sync--device-dir name) '(change)
				   'librera-sync--watch-callback)
	    librera-sync-watchers))))

(defun librera-sync--stop-watching ()
  "Disable watchers."
  (dolist (watcher librera-sync-watchers)
    (file-notify-rm-watch watcher))
  (setq librera-sync-watchers '())
  )

(defun librera-sync--watch-callback (watchdata)
  "Callback for file-inotify-watch.
WATCHDATA contains some info about event"
  (when (and (string-equal "renamed" (car (cdr watchdata)))
	     (string-equal "app-Progress.json" (f-filename (car (nthcdr 3 watchdata)))))
    (librera-sync--schedule-update-all)))


;; Timer functions

(defun librera-sync--start-timer ()
  "Start timer for checking."
  (setq librera-sync-timer
	(run-with-timer librera-sync-interval
			librera-sync-interval
			'librera-sync--schedule-update-all)))

(defun librera-sync--stop-timer ()
  "Cancel Librera update timer."
  (cancel-timer librera-sync-timer))


;; PDF-tools

(defun librera-sync--set-pos-pdf-view (position)
  "Set POSITION in pdf-view buffer."
  (pdf-view-goto-page
   (round (* position (pdf-cache-number-of-pages)))))

(defun librera-sync--current-pos-pdf-view ()
  "Get current position in pdf-view buffer."
  (/ (pdf-view-current-page) (float (pdf-cache-number-of-pages))))


(defun librera-sync--current-pos ()
  "Calculate position for current buffer."
  (cond ((derived-mode-p 'pdf-view-mode)
	 (librera-sync--current-pos-pdf-view))))

(defun librera-sync--set-pos (position)
  "Set POSITION in current buffer."
  (cond ((derived-mode-p 'pdf-view-mode)
	 (librera-sync--set-pos-pdf-view position))))

(defun librera-sync-track-current-buffer ()
  "Save current buffer values and update if necessary."
  (push (buffer-name) librera-sync-tracked-filenames)
  (setq-local librera-sync--new-position nil)
  (setq-local librera-sync--update-source nil)
  )

(defun librera-sync-untrack-current-buffer ()
  "Stop tracking current buffer."
  (setq librera-sync-tracked-filenames
	(delete (buffer-name) librera-sync-tracked-filenames)))

;;;###autoload
(defun librera-sync-load ()
  "Load latest position for current buffer from Librera."

  (interactive)
  (let ((pos-params (librera-sync--read-pos-for (buffer-name))))
    (if pos-params
	(let ((pos (car pos-params))
	      (time (car (cdr pos-params)))
	      (devname (car (nthcdr 2 pos-params))))
	  (if (string-equal devname librera-sync-device-name)
	      (message "Already latest position")
	    (message "Position loaded from %s" devname)
	    (librera-sync--set-pos pos)))
      (message "There is no position for this document"))))

;;;###autoload
(defun librera-sync-load-from-device ()
  "Choose device to load position from."
  (interactive)
  (let* ((prompt (format "Load progress for %S from: " (buffer-name)))
	 (candidates (delete librera-sync-device-name (librera-sync--device-names)))
	 (devname (completing-read prompt candidates nil 't))
	 (pos-params (librera-sync--read-pos-for (buffer-name) devname)))
    (if pos-params
	(librera-sync--set-pos (car pos-params))
      (message "There is no progress for %S at this device" (buffer-name))
  )))

;;;###autoload
(define-minor-mode librera-sync-global-mode
  "Sync buffer states with Librera Reader"
  :global t
  :lighter " LS"
  :group 'librera-sync
  (if librera-sync-global-mode
      (progn (dolist (buf (buffer-list))
	       (with-current-buffer buf
		 (when (derived-mode-p 'pdf-view-mode)
		   (librera-sync-load)
		   (librera-sync-track-current-buffer))))

	     (if (eq librera-sync-update-method 'inotify)
		 (librera-sync--start-watching)
	       (librera-sync--start-timer))
	     (add-hook 'kill-buffer-hook 'librera-sync-untrack-current-buffer)
	     (add-hook 'pdf-view-mode-hook 'librera-sync-track-current-buffer)
	     (add-hook 'pdf-view-after-change-page-hook 'librera-sync-save)
	     (librera-sync--schedule-update-all))

    (if (eq librera-sync-update-method 'inotify)
	(librera-sync--stop-watching)
      (librera-sync--stop-timer))
    (remove-hook 'pdf-view-mode-hook 'librera-sync-track-current-buffer)
    (remove-hook 'pdf-view-after-change-page-hook 'librera-sync-save)
    (setq librera-sync-tracked-filenames '())))

(provide 'librera-sync)
;;; librera-sync.el ends here
