# λ +log - logging functions

*Author:* lispcat <187922791+lispcat@users.noreply.github.com><br>

logging functions

# > post-init

## ‣ log src-dir state


```emacs-lisp
(require 'dash)

(defun +log-src-dir-state ()
  "Returns paths under `+emacs-src-dir' that may be excluded from `load-path'.

Intended to be ran post-initialization.

Elisp files under the `+emacs-src-dir' must be manually loaded with `require',
typically from a single file. To catch possible files that should be loaded, but
were not loaded, they're identified and logged using this function."
  (let* ((matching-dirs
          (-some->> load-path
            ;; is elem from load-path under src-dir? exists? dir?
            (-filter (-andfn (-partial #'string-match +emacs-src-dir)
                             #'file-exists-p
                             #'file-directory-p))))

         ;; debug
         (_ (funcall (-debug "Log: +emacs-src-dir: dirs found") matching-dirs))

         (elisp-files
          (-some->> matching-dirs
            ;; collect all subfiles
            (-mapcat (lambda (f) (directory-files
                             f directory-files-no-dot-files-regexp)))
            ;; keep only elisp files
            (-filter (-partial #'string-match emacs-lisp-file-regexp))))

         (valid-elisp-files
          (-some->> elisp-files
            ;; keep only valid/require-able files
            (-filter
             (lambda (f)
               (with-temp-buffer
                 (insert-file-contents f)
                 (goto-char (point-min))
                 (search-forward
                  (concat "(provide '"
                          (file-name-base f)
                          ")")
                  nil t))))))

         ;; debug
         (_ (funcall (-debug "Log: +emacs-src-dir: invalid elisp files")
                     (-map #'file-name-nondirectory
                           (-difference elisp-files valid-elisp-files))))
         
         (remove-existing
          ;; remove files already require'd
          (let ((removed-lst
                 (-remove (-compose #'featurep
                                    #'intern
                                    #'file-name-base)
                          valid-elisp-files)))
            (if removed-lst
                (message "WARNING: +emacs-src-dir: non-loaded file: %s"
                         removed-lst)
              (message "Log: +emacs-src-dir: no issues"))))

         (result remove-existing))

    ;; return mesg, or deal with nil
    (or result
        (message "ERROR: +emacs-src-dir: error in logging func?"))))


(add-hook 'emacs-startup-hook #'+log-src-dir-state)

```

+log.el ends here



---

*Last updated: August 14, 2025*
