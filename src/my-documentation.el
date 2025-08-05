;;; my-documentation.el --- auto docs generator      -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lispcat

;; Author: lispcat <187922791+lispcat@users.noreply.github.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Automatically generates documentation using el2markdown.
;; Converts elisp files into markdown docs.

;;; Code:

(leaf el2markdown
  :require t
  :config

  (require 'dash)
  (require 'el2markdown)

  (defun my/el2markdown-write-file (&optional file-name overwrite-without-confirm)
    "Convert comment section to markdown and write to file."
    (interactive
     (let ((suggested-name (and (buffer-file-name)
                                (concat (file-name-sans-extension
                                         (buffer-file-name))
                                        ".md"))))
       (list (read-file-name "Write markdown file: "
                             default-directory
                             suggested-name
                             nil
                             (file-name-nondirectory suggested-name)))))
    (unless file-name
      (setq file-name (concat (buffer-file-name) ".md")))
    (let ((buffer (current-buffer))
          (orig-buffer-file-coding-system buffer-file-coding-system))
      (with-temp-buffer
        ;; Inherit the file coding from the buffer being converted.
        (setq buffer-file-coding-system orig-buffer-file-coding-system)
        (let ((standard-output (current-buffer)))
          (with-current-buffer buffer
            (my/el2markdown-convert))
          ;; Note: Must set `require-final-newline' inside
          ;; `with-temp-buffer', otherwise the value will be overridden by
          ;; the buffers local value.
          (let ((require-final-newline nil))
            (write-file file-name (not overwrite-without-confirm)))))))

  (defun my/el2markdown-convert (&optional include-end)
    "Print commentart section of current buffer as MarkDown.

After conversion, `el2markdown-post-convert-hook' is called.  The
functions in the hook should accept one argument, the output
stream (typically the destination buffer).  When the hook is run
current buffer is the source buffer."
    (save-excursion
      (goto-char (point-min))
      (el2markdown-convert-title)
      (el2markdown-convert-formal-information)
      (el2markdown-skip-to-commentary)
      (while
          (el2markdown-convert-section))
      (terpri)
      (princ "---")
      (terpri)
      (when include-end
        (let ((file-name (buffer-file-name))
              (from ""))
          (if file-name
              (setq from (concat " from `"
                                 (file-name-nondirectory file-name)
                                 "`")))
          (princ (concat
                  "Converted" from
                  " by "
                  "[*el2markdown*](https://github.com/Lindydancer/el2markdown)."))
          (terpri)))
      (run-hook-with-args 'el2markdown-post-convert-hook standard-output)))

  (defun my/remove-prefix-or-err (prefix str)
    (if (string-prefix-p prefix str)
        (string-remove-prefix prefix str)
      (error "%s is not a prefix of %s" prefix str)
      nil))

  (defun my/zip-lists (list1 list2)
    (let ((len1 (length list1))
          (len2 (length list2)))
      (unless (eq len1 len2)
        (error "Lists to zip differ in length (%s %s): %s %s"
               len1 len2 list1 list2))
      (-zip-pair list1 list2)))

  (defun my/assert-f-dir-p (path)
    (if (f-dir-p path)
        path
      (error (format "Path not exist: %s" path))
      nil))

  (defun my/create-docs (root-directory infiles-directory outfiles-directory
                                        &optional extra-infiles)
    (interactive
     (let* ((root-directory
             (read-file-name "Root dir: " default-directory
                             `,(buffer-file-name) 'confirm))
            (infiles-directory
             (read-file-name "Infiles dir: " root-directory nil 'confirm))
            (outfiles-directory
             (read-file-name "Outfiles dir: " root-directory nil 'confirm)))
       ;; ret
       (list root-directory infiles-directory outfiles-directory)))

    ;; expand
    (setq root-directory (expand-file-name root-directory))
    (setq infiles-directory (expand-file-name infiles-directory))
    (setq outfiles-directory (expand-file-name outfiles-directory))
    (setq extra-infiles (mapcar #'expand-file-name extra-infiles))

    ;; fs checks
    (my/remove-prefix-or-err root-directory infiles-directory)
    (my/remove-prefix-or-err root-directory outfiles-directory)

    (my/assert-f-dir-p root-directory)
    (my/assert-f-dir-p infiles-directory)

    ;; proceed?
    (when (y-or-n-p
           (format
            "Prefix: %s\nInfiles: %s\nOutfiles: %s\n> Proceed? "
            root-directory infiles-directory outfiles-directory))

      ;; body
      (let* (;; collect all infiles
             (infiles-all
              (append
               (reverse
                (-filter #'file-regular-p
                         (directory-files-recursively infiles-directory ".*")))
               extra-infiles))

             ;; derive all outfiles for each infile
             ;; - given infile:
             ;;   - remove prefix infiles-directory, but keep the dirname
             ;;   - replace extension .el -> .md
             ;;   - prepend with prefix outfiles-directory
             (outfiles-all
              (mapcar (lambda (infile)
                        (-as-> infile x
                               (my/remove-prefix-or-err (file-name-directory infiles-directory) x)
                               (file-name-sans-extension x)
                               (file-name-with-extension x ".md")
                               (expand-file-name x outfiles-directory)))
                      infiles-all))

             ;; create alist from infiles-all and outfiles-all
             (alist
              (my/zip-lists infiles-all outfiles-all))

             (alist-uniq-parts
              (mapcar
               (lambda (pair)
                 (let* ((infile (car pair))
                        (outfile (cdr pair))
                        (infile-uniq (my/remove-prefix-or-err
                                      (file-name-directory infiles-directory)
                                      infile))
                        (outfile-uniq (my/remove-prefix-or-err
                                       outfiles-directory
                                       outfile))
                        (infile-uniq-no-slash (replace-regexp-in-string
                                               "^/+" "" infile-uniq))
                        (outfile-uniq-no-slash (replace-regexp-in-string
                                                "^/+" "" outfile-uniq)))
                   (cons infile-uniq-no-slash
                         outfile-uniq-no-slash)))
               alist)))

                                        ; checks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

        ;; check: ensure unique part (minus extension) is matching:
        (dolist (pair alist-uniq-parts)
          (unless (equal (file-name-sans-extension (car pair))
                         (file-name-sans-extension (cdr pair)))
            (error "Found pair with non-equal uniq part: %s"
                   pair)))
        ;; check: ensure all infiles exist
        (dolist (infile infiles-all)
          (unless (file-exists-p infile)
            (error "Found an infile that doesn't exist: %s"
                   infile)))
        ;; check: ensure all targets are under the outfiles dir:
        (dolist (outfile outfiles-all)
          (unless (my/remove-prefix-or-err outfiles-directory outfile)
            (error "outfile not a prefix of outfiles-directory: %s"
                   outfile)))

        ;; delete outfiles-directory
        (when (f-dir-p outfiles-directory)
          (when (y-or-n-p (format "Trash outfiles-directory (%s)? " outfiles-directory))
            (move-file-to-trash outfiles-directory)))

        ;; action
        (with-output-to-temp-buffer "*File Copy Preview*"
          (princ "Files to be copied:\n")
          (princ "==================\n\n")
          (dolist (pair alist)
            (princ (format "%s\n%s\n"
                           (car pair)
                           (cdr pair)))))
        ;; make all parent dirs?
        (when (y-or-n-p (format "Create all parent dirs?"))
          (dolist (path outfiles-all)
            (message "DEBUG: path: %s" path)
            (let ((dir (file-name-directory path)))
              (message "DEBUG: dir: %s" dir)
              (unless (f-dir-p dir)
                (make-directory dir t)))))

        ;; proceed?
        (when (y-or-n-p "Proceed?")
          (mapcar (lambda (pair)
                    (let ((infile (car pair))
                          (outfile (cdr pair)))
                      (message "%s" pair)
                      (with-temp-buffer
                        (insert-file-contents infile)
                        (my/el2markdown-write-file outfile t))))
                  alist)))))

  (defun my/create-docs-default ()
    (interactive)
    (my/create-docs "~/.emacs.d/"
                    "~/.emacs.d/src"
                    "~/.emacs.d/docs/docs_src"
                    '("~/.emacs.d/init.el" "~/.emacs.d/early-init.el"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     end                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-documentation)
;;; my-documentation.el ends here
