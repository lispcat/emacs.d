;;; my-org-extras.el --- org extras

;;; Commentary:

;;; Code:

(defun my/org-priority-to-anki ()
  (interactive)
  ;; check connection with anki
  (unless (or (boundp 'anki-editor-mode) anki-editor-mode)
    (anki-editor-mode 1))
  (anki-editor-api-check)
  ;; delete anki_note_type and/or anki_note_id for each w/o a priority
  (save-excursion
    (let ((points-no-priority
           (org-ql-query
             :select #'point-marker
             :from (current-buffer)
             :where
             '(and (not (priority))
                   (or (property "ANKI_NOTE_ID")
                       (property "ANKI_NOTE_TYPE"))))))
      (dolist (p (reverse points-no-priority))
        (goto-char p)
        (when (org-find-property "ANKI_NOTE_ID")
          (anki-editor-delete-note-at-point))
        (when (org-find-property "ANKI_NOTE_TYPE")
          (org-delete-property "ANKI_NOTE_TYPE")))))
  ;; ensure all priority headings have anki_note_type set
  (save-excursion
    (let ((points-yes-priority
           (org-ql-query
             :select #'point-marker
             :from (current-buffer)
             :where '(priority))))
      (dolist (p (reverse points-yes-priority))
        (goto-char p)
        (unless (org-entry-get nil "ANKI_NOTE_TYPE")
          (anki-editor-set-note-type nil "Basic"))))))

(defun my/org-clone-with-fraction (days time effort)
  "Clone subtree with time shifts, prefixing each subheading with fraction prefix."
  (interactive
   (list
    (read-number "How many days to complete it over?: ")
    (read-number "How many minutes do you expect this task to take?: ")
    (read-number "On a scale of 1-10, how much effort will this take?: ")))
  (setq days (1- days))
  ;; create clones
  (org-clone-subtree-with-time-shift days "-1d")
  (org-set-property "TIME" (format "%s" time))
  (org-set-property "EFFORT" (format "%s" effort))
  ;; adjust appropriately
  (save-excursion
    (org-next-visible-heading 1)
    ;; first, sort
    (cl-loop for depth from (1- days) downto 1 do
             (save-excursion
               ;; shift
               (dotimes (_ depth)
                 (org-metadown))))
    ;; add todo and demote
    (save-excursion
      (cl-loop repeat (1- days) do
               (org-next-visible-heading 1))
      (cl-loop for depth from (1- days) downto 0 do
               (let ((frac (format "%d/%d" (1+ depth) days))
                     (time-daily (/ time days)))
                 (org-demote)
                 (let ((org-special-ctrl-a/e t))
                   (org-beginning-of-line))
                 (insert (concat frac " "))
                 (org-set-property "FRACTION" frac)
                 (org-set-property "TIME" (format "%s" time-daily))
                 (org-set-property "EFFORT" (format "%s" effort))
                 (org-next-visible-heading -1))))))


(provide 'my-org-extras)
;;; my-org-extras.el ends here
