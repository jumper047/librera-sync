;;; librera-sync-doc-view.el --- Doc-view support for librera-sync -*- lexical-bindings: t; -*-

;; Copyright (c) 2021 Dmitriy Pshonko <jumper047@gmail.com>

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

;; doc-view related functions for librera-sync

;;; Code:

(require 'dash)
(require 'f)


(declare-function doc-view-goto-page "doc-view" (page))
(declare-function doc-view-last-page-number "doc-view" ())
(declare-function librera-sync-save "librera-sync")


(defun librera-sync--doc-view-mode-save (page)
  "Function to advice goto-page, PAGE arg added for signature compatibility."
  (librera-sync-save))

(defun librera-sync-doc-view-mode-prepare ()
  "Add librera advice to doc-view."
  (advice-add 'doc-view-goto-page :after #'librera-sync--doc-view-mode-save))

(defun librera-sync-doc-view-mode-clean ()
  "Remove librera advice from doc-view."
  (advice-remove 'doc-view-goto-page #'librera-sync--doc-view-mode-save))

(defun librera-sync-doc-view-mode-set-pos (position)
  "Set POSITION in doc-view buffer."
  (doc-view-goto-page
   (round (* position (doc-view-last-page-number)))))

(defun librera-sync-doc-view-mode-current-pos ()
  "Get current position in pdf-view buffer."
  (/ (eval `(doc-view-current-page))
     (float (doc-view-last-page-number))))

(defun librera-sync-doc-view-mode-book-name ()
  "Get current book name."
  (f-filename (buffer-file-name)))
 
(provide 'librera-sync-doc-view)

;;; librera-sync-doc-view.el ends here
