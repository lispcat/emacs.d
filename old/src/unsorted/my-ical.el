;; https://www.reddit.com/r/emacs/comments/ioenk2/ical_import_in_emacs_calendar/

(require 'diary-lib)

(setq my/calendars
      (with-temp-buffer
        (insert-file-contents "~/Private/elisp/calendar-urls.el")
        (read (current-buffer))))

(defun my/ical-pull-all ()
  (interactive)
  (find-file diary-file)
  (erase-buffer)
  (message "Cleared diary file")
  (mapcar (lambda (url)
            (let ((tmpfile (url-file-local-copy url)))
              (message "Importing ")
              (icalendar-import-file tmpfile diary-file)
              (kill-buffer (car (last (split-string tmpfile "/"))))))
          my/calendars))


(provide 'my-ical)
