;;; librera-sync.el --- Sync document's position with Librera Reader for Android -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Dmitriy Pshonko <jumper047@gmail.com>

;; Author: Dmitriy Pshonko <jumper047@gmail.com>
;; URL: https://github.com/jumper047/librera-sync
;; Keywords: multimedia, sync
;; Version: 0.2.0
;; Package-Requires: ((emacs "26.1") (f "0.17") (dash "2.12.0"))

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

;; Minor mode to sync document's position with Librera Reader (book/documents reader for Android) or other Emacs instances.

;; Supported modes:
;; - doc-view
;; - pdf-tools
;; - fb2-reader-mode (experimental)

;; See repo's readme for installation and setup instructions!



;;; Code:

(require 'f)
(require 'json)
(require 'filenotify)
(require 'librera-sync-pdf-view)
(require 'librera-sync-doc-view)
(require 'librera-sync-fb2-reader-mode)

(defgroup librera-sync nil
  "A group for librera-sync related customizations"
  :group 'applications
  :prefix "librera-sync-")

(defcustom librera-sync-directory nil
  "Path to Librera folder."
  :type '(directory)
  :group 'librera-sync)

(defcustom librera-sync-cache-directory (expand-file-name "librera-sync" user-emacs-directory)
  "Path to temp directory used by librera plugins."
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

(defcustom librera-sync-blacklist '()
  "Files from that list will not be synced.
Ignored if whitelist is not empty"
  :group 'librera-sync
  :type '(repeat string))

(defcustom librera-sync-whitelist '()
  "Only files from that list will not be synced."
  :group 'librera-sync
  :type '(repeat string))

(defvar librera-sync-supported-modes '(pdf-view-mode
                                       doc-view-mode
                                       fb2-reader-mode)
  "Major modes supported by librera.")

(defvar librera-sync-timer nil
  "Timer for interval progress checking.")

(defvar librera-sync-watchers '()
  "List of watch items from inotify.")

(defvar librera-sync-tracked-filenames '()
  "List of files currently opened.")

(defvar librera-sync--new-position nil
  "Buffer-local variable stores desired postition for curr buffer.")

(defvar librera-sync--update-source nil
  "Buffer-local variable stores source of new position for current buffer.")

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
  (f-join (librera-sync--curr-device-dir) "app-Progress.json"))
 
(defun librera-sync--time-msecs ()
  "Get time in format used by Librera."
  (round (* 1000 (float-time))))

(defun librera-sync-ensure-dir (dir)
  "Create DIR if necessary."
  (unless (f-exists-p dir)
    (make-directory dir)))

(defun librera-sync--write-pos (position docname)
  "Save DOCNAME's POSITION to Librera."
  ;; Create directory and empty json file if there is noone already
  (librera-sync-ensure-dir (librera-sync--curr-device-dir))

  (let ((json-array-type 'list)
        (json-key-type 'string)
        (default-book-params (list (cons "cp" json-false)
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
                                   (cons "z" 100))))
    (if (not (f-exists? (librera-sync--curr-device-progr-json)))
        (with-temp-file (librera-sync--curr-device-progr-json)
          (insert (json-encode (list (cons docname default-book-params)))))

      (setq default-book-params-ht (make-hash-table :test 'equal)
            app-progress (json-read-file (librera-sync--curr-device-progr-json))
            book-params (alist-get docname app-progress default-book-params 'nil 'equal))
      (setf (alist-get "p" book-params nil nil 'equal) position
            (alist-get "t" book-params nil nil 'equal) (librera-sync--time-msecs)
            (alist-get docname app-progress nil nil 'equal) book-params)
      (with-temp-file (librera-sync--curr-device-progr-json)
        (insert (json-encode app-progress))))))

(defun librera-sync-save ()
  "Save current position to Librera profile.
If buffer has position to restore, do nothing"
  (unless librera-sync--new-position
  (librera-sync--write-pos
   (librera-sync--current-pos) (librera-sync--book-name))))

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
Returns hash table with list (POSITION TIME FILE)"
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
Returns (position time devname)
Docname must be already added to tracked buffers list"
  (if devname
      (when-let* ((progress-js (librera-sync--read-progress-from devname))
                  (doc-params (gethash docname progress-js))
                  (position (gethash "p" doc-params))
                  (time (gethash "t" doc-params)))
        (list position time devname))
    (gethash docname (librera-sync--read-all-pos))))

(defun librera-sync--schedule-update-all-tracked ()
  "Schedule update for all buffers with new positions.
Function checks new positions for all buffers from other Librera instances."
  (let* ((positions (librera-sync--read-all-pos)))
    (dolist (filename librera-sync-tracked-filenames)
      (when-let* ((filepos (gethash filename positions))
                  (pos (car filepos))
                  (sname (car (nthcdr 2 filepos)))
                  (notsame (not (string-equal sname librera-sync-device-name))))
        (with-current-buffer filename
          (librera-sync--schedule-update-cur-buffer pos sname))))))

(defun librera-sync--schedule-update-cur-buffer (pos sname)
  "Schedule set current buffer to position POS.
resource name is SNAME"
  (unless (member (librera-sync--book-name) librera-sync-tracked-filenames)
    (error "Can't schedule update of non tracked buffer"))
  (setq-local librera-sync--new-position pos)
  (setq-local librera-sync--update-source sname)
  ;; If buffer is active update immediately else schedule
  (if (eq (current-buffer) (window-buffer (selected-window)))
      (librera-sync--update-cur-buffer)
    (add-hook 'post-command-hook #'librera-sync--update-cur-buffer 0 't)))

(defun librera-sync--update-cur-buffer ()
  "Update current buffer if it has deferred position."
  (if librera-sync--new-position
      (librera-sync--set-pos librera-sync--new-position))
  (message "Position loaded from %S" librera-sync--update-source)
  (setq-local librera-sync--new-position nil)
  (setq-local librera-sync--update-source nil)
  (remove-hook 'post-command-hook #'librera-sync--update-cur-buffer 't))


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
  (setq librera-sync-watchers '()))

(defun librera-sync--watch-callback (watchdata)
  "Callback for file-inotify-watch.
WATCHDATA contains some info about event"
  (when (and (string-equal "renamed" (car (cdr watchdata)))
             (string-equal "app-Progress.json" (f-filename (car (nthcdr 3 watchdata)))))
    (librera-sync--schedule-update-all-tracked)))


;; Timer functions

(defun librera-sync--start-timer ()
  "Start timer for checking."
  (setq librera-sync-timer
        (run-with-timer librera-sync-interval
                        librera-sync-interval
                        'librera-sync--schedule-update-all-tracked)))

(defun librera-sync--stop-timer ()
  "Cancel Librera update timer."
  (when librera-sync-timer
  (cancel-timer librera-sync-timer)
  (setq librera-sync-timer nil)))


(defun librera-sync--major-mode-command (command &rest args)
  "Execute function for major mode in current buffer.
Full function name will be librera-sync-- + \"major-mode\" + COMMAND
Returns nil if there is no suitable function
ARGS will be passed to function"
  (when (member major-mode librera-sync-supported-modes)
    (let ((function-name (format "librera-sync-%s-%s" major-mode command)))
      (apply (intern-soft function-name) args))))

(defun librera-sync--prepare-major-mode ()
  "Run some major mode specific code."
  (librera-sync--major-mode-command "prepare"))

(defun librera-sync--clean-major-mode ()
  "Clean major mode from librera hooks."
  (librera-sync--major-mode-command "clean"))

(defun librera-sync--current-pos ()
  "Calculate position for current buffer."
  (librera-sync--major-mode-command "current-pos"))

(defun librera-sync--set-pos (position)
  "Set POSITION in current buffer."
  (librera-sync--major-mode-command "set-pos" position))

(defun librera-sync--book-name ()
  "Return book name used in librera config."
  (librera-sync--major-mode-command "book-name"))

(defun librera-sync-track-current-buffer ()
  "Save current buffer values and update if necessary."
  (if (member (librera-sync--book-name) librera-sync-tracked-filenames)
      (message "Buffer %S already tracked" (librera-sync--book-name))
    (librera-sync--prepare-major-mode)
    (push (librera-sync--book-name) librera-sync-tracked-filenames)
    (setq-local librera-sync--new-position nil)
    (setq-local librera-sync--update-source nil)
    ;; Trying to restore position
    (when-let* ((pos-params (librera-sync--read-pos-for (librera-sync--book-name)))
                (position (car pos-params))
                (source (car (nthcdr 2 pos-params))))
      (librera-sync--schedule-update-cur-buffer position source))
    (add-hook 'kill-buffer-hook
              (lambda () (librera-sync-mode -1)) -100 't)))

(defun librera-sync-untrack-current-buffer ()
  "Stop tracking current buffer."
  (setq librera-sync-tracked-filenames
        (delete (librera-sync--book-name) librera-sync-tracked-filenames))
  (librera-sync--clean-major-mode))

;;;###autoload
(defun librera-sync-load ()
  "Load latest position for current buffer from Librera."

  (interactive)
  (let ((pos-params (librera-sync--read-pos-for (librera-sync--book-name))))
    (if pos-params
        (let ((pos (car pos-params))
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
  (let* ((prompt (format "Load progress for %S from: " (librera-sync--book-name)))
         (candidates (delete librera-sync-device-name (librera-sync--device-names)))
         (devname (completing-read prompt candidates nil 't))
         (pos-params (librera-sync--read-pos-for (librera-sync--book-name) devname)))
    (if pos-params
        (librera-sync--set-pos (car pos-params))
      (message "There is no progress for %S at this device" (librera-sync--book-name)))))

;;;###autoload
(define-minor-mode librera-sync-mode
  "Sync current buffer with Librera Reader"
  :lighter " LS"
  :group 'librera-sync
  (if librera-sync-mode
      (progn (librera-sync-ensure-dir librera-sync-cache-directory)
             (cond ((and (eq librera-sync-update-method 'inotify)
                         (not librera-sync-watchers))
                    (librera-sync--start-watching))
                   ((and (eq librera-sync-update-method 'timer)
                         (not librera-sync-timer))
                    (librera-sync--start-timer)))
             (librera-sync-track-current-buffer))
    (librera-sync-untrack-current-buffer)
    (cond ((and (eq librera-sync-update-method 'inotify)
                (not librera-sync-tracked-filenames))
           (librera-sync--stop-watching))
          ((and (eq librera-sync-update-method 'timer)
                (not librera-sync-tracked-filenames))
           (librera-sync--stop-timer)))))

(defun librera-sync--global-turn-on ()
  "Enable librera-sync mode in buffer if major mode supported."
  (when (and (memq major-mode librera-sync-supported-modes)
             (cond (librera-sync-whitelist
                    (member (librera-sync--book-name) librera-sync-whitelist))
                   (librera-sync-blacklist
                    (not (member (librera-sync--book-name) librera-sync-blacklist)))
                   (t t)))
    (librera-sync-mode +1)))

;;;###autoload
(define-globalized-minor-mode global-librera-sync-mode librera-sync-mode
  librera-sync--global-turn-on)

(provide 'librera-sync)

;;; librera-sync.el ends here

