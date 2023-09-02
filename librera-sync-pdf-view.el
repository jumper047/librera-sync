;;; librera-sync-pdf-view.el --- Pdf-tools support for librera-sync -*- lexical-bindings: t; -*-

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

;; pdf-tools related functions for librera-sync

;;; Code:


(require 'dash)
(require 'f)


(declare-function pdf-view-goto-page "pdf-view" (page))
(declare-function pdf-cache-number-of-pages "pdf-cache" ())

;;; PDF-tools

(defun librera-sync-pdf-view-mode-prepare ()
  "Add librera hooks to PDF buffer."
  (add-hook 'pdf-view-after-change-page-hook 'librera-sync-save 0 't))

(defun librera-sync-pdf-view-mode-clean ()
  "Remove librera hooks from PDF buffer."
  (remove-hook 'pdf-view-after-change-page-hook 'librera-sync-save 't))

(defun librera-sync-pdf-view-mode-set-pos (position)
  "Set POSITION in pdf-view buffer."
  (pdf-view-goto-page
   (round (* position (pdf-cache-number-of-pages)))))

(defun librera-sync-pdf-view-mode-current-pos ()
  "Get current position in pdf-view buffer."
  (/ (eval `(pdf-view-current-page))
     (float (pdf-cache-number-of-pages))))

(defun librera-sync-pdf-view-mode-book-name ()
  "Get current book name."
  (f-filename (buffer-file-name)))

(defun librera-sync-pdf-view-mode-book-path ()
  "Get path to current document."
  (buffer-file-name))

(provide 'librera-sync-pdf-view)

;;; librera-sync-pdf-view.el ends here
