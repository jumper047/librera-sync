(require 'dash)
(require 'f)


(declare-function doc-view-goto-page "doc-view" (page))
(declare-function doc-view-last-page-number "doc-view" ())


(defun librera-sync--doc-view-mode-save (page)
  "Dumb function to advice doc-view's goto-page"
  (librera-sync-save))

(defun librera-sync--doc-view-mode-prepare ()
  "Add librera advice to doc-view."
  (advice-add 'doc-view-goto-page :after #'librera-sync--doc-view-mode-save)
)

(defun librera-sync--doc-view-mode-clean ()
  "Remove librera advice from doc-view."
  (advice-remove 'doc-view-goto-page #'librera-sync--doc-view-mode-save)
)

(defun librera-sync--doc-view-mode-set-pos (position)
  "Set POSITION in doc-view buffer."
  (doc-view-goto-page
   (round (* position (doc-view-last-page-number)))))

(defun librera-sync--doc-view-mode-current-pos ()
  "Get current position in pdf-view buffer."
  (/ (eval `(doc-view-current-page))
     (float (doc-view-last-page-number))))

(defun librera-sync--doc-view-mode-book-name ()
  "Get current book name."
  (f-filename (buffer-file-name))
  )
 
(provide 'librera-sync-doc-view)
