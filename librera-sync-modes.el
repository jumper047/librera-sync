(defvar librera-sync-supported-modes '(pdf-view-mode)
  "Major modes supported by librera.")

(declare-function librera-sync-save "librera-sync" ())

(declare-function pdf-view-goto-page "pdf-view" (page))
(declare-function pdf-cache-number-of-pages "pdf-cache" ())

;;; PDF-tools

(defun librera-sync--pdf-view-mode-prepare ()
  "Add librera hooks to PDF buffer."
  (add-hook 'pdf-view-after-change-page-hook 'librera-sync-save 0 't)
)

(defun librera-sync--pdf-view-mode-clean ()
  "Remove librera hooks from PDF buffer."
  (remove-hook 'pdf-view-after-change-page-hook 'librera-sync-save 't)
)

(defun librera-sync--pdf-view-mode-set-pos (position)
  "Set POSITION in pdf-view buffer."
  (pdf-view-goto-page
   (round (* position (pdf-cache-number-of-pages)))))

(defun librera-sync--pdf-view-mode-current-pos ()
  "Get current position in pdf-view buffer."
  (/ (eval `(pdf-view-current-page))
     (float (pdf-cache-number-of-pages))))

(provide 'librera-sync-modes)
