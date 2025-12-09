;;; el2markdown-fork.el --- Convert commentary of elisp files to markdown. -*- lexical-binding: t; -*-

;; Copyright (C) 2013-2017,2025 Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.8
;; Created: 2013-03-26
;; URL: https://github.com/Lindydancer/el2markdown

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package converts *Commentary* section in Emacs Lisp modules to
;; text files in MarkDown format, a format supporting headings, code
;; blocks, basic text styles, bullet lists etc.
;;
;; The MarkDown is used by many web sites as an alternative to plain
;; texts. For example, it is used by sites like StackOverflow and
;; GitHub.

;; What is converted:
;;
;; Everything between the `Commentary:' and `Code:' markers are
;; included in the generated text. In addition, the title and *some*
;; metadata are also included.

;; How to write comments:
;;
;; The general rule of thumb is that the Emacs Lisp module should be
;; written using plain text, as they always have been written.
;;
;; However, some things are recognized. A single line ending with a
;; colon is considered a *heading*. If this line is at the start of a
;; comment block, it is considered a main (level 2) heading. Otherwise
;; it is considered a (level 3) subheading. Note that the line
;; precedes a bullet list or code, it will not be treated as a
;; subheading.
;;
;; Use Markdown formatting:
;;
;; It is possible to use markdown syntax in the text, like *this*, and
;; **this**.
;;
;; Conventions:
;;
;; The following conventions are used when converting elisp comments
;; to MarkDown:
;;
;; * Code blocks using either the Markdown convention by indenting the
;;   block with four extra spaces, or by starting a paragraph with a
;;   `('.
;;
;; * In elisp comments, a reference to `code' (backquote - quote),
;;   they will be converted to MarkDown style (backquote - backquote).
;;
;; * In elisp comments, bullets in lists are typically separated by
;;   empty lines. In the converted text, the empty lines are removed,
;;   as required by MarkDown.
;;

;; Example:
;;
;;
;;     ;; This is a heading:
;;     ;;
;;     ;; Bla bla bla ...
;;
;;     ;; This is another heading:
;;     ;;
;;     ;; This is a paragraph!
;;     ;;
;;     ;; A subheading:
;;     ;;
;;     ;; Another paragraph.
;;     ;;
;;     ;; This line is *not* as a subheading:
;;     ;;
;;     ;; * A bullet in a list
;;     ;;
;;     ;; * Another bullet.

;; Installation:
;;
;; Place this package somewhere in Emacs `load-path' and add the
;; following lines to a suitable init file:
;;
;; (autoload 'el2markdown-view-buffer  "el2markdown" nil t)
;; (autoload 'el2markdown-write-file   "el2markdown" nil t)
;; (autoload 'el2markdown-write-readme "el2markdown" nil t)

;; Usage:
;;
;; To generate the markdown representation of the current buffer to a
;; temporary buffer, use:
;;
;;     M-x el2markdown-view-buffer RET
;;
;; To write the markdown representation of the current buffer to a
;; file, use:
;;
;;     M-x el2markdown-write-file RET name-of-file RET
;;
;; In sites like GitHub, if a file named README.md exists in the root
;; directory of an archive, it is displayed when viewing the archive.
;; To generate a README.md file, in the same directory as the current
;; buffer, use:
;;
;;     M-x el2markdown-write-readme RET

;; Post processing:
;;
;; To post-process the output, add a function to
;; `el2markdown-post-convert-hook'.  The functions in the hook should
;; accept one argument, the output stream (typically the destination
;; buffer).  When the hook is run current buffer is the source buffer.

;; Batch mode:
;;
;; You can run el2markdown in batch mode. The function
;; `el2markdown-write-readme' can be called directly using the `-f'
;; option. The others can be accessed with the `--eval' form.
;;
;; For example,
;;
;;     emacs -batch -l el2markdown.el my-file.el -f el2markdown-write-readme

;;; Code:

(require 'dash)

;;; Variables

;; The `{{{' and `}}}' and sequences are used by the package
;; `folding.el'.
(defvar el2markdown-empty-comment "^;;+ *\\(\\({{{\\|}}}\\).*\\)?$"
  "Regexp of lines that should be considered empty.")


(defvar el2markdown-cob-regexp ";;;+\n;; *\\(.*?\\) *;+\n;;;+"
  "Regexp of comment block.")


(defvar el2markdown-coh-regexp "^;;; --+ \\(.*\\) -+$"
  "Regexp of comment header.")


(defvar el2markdown-cow-regexp "^;;;? --?$"
  "Regexp of comment-wrap")

(defvar el2markdown-heading-regexp "^\\(;;;+\\) \\(.*\\)$"
  "Regexp of comment headers")


(defvar el2markdown-translate-keys-within-markdown-quotes nil
  "When non-nil, match key sequences found between backquotes.

By default, this package only converts things quoted using
backquote and quote, which is the standard elisp way to quote
things in comments.")


(defvar el2markdown-keys '("RET" "TAB")
  "List of keys that sould be translated to <key>...</key>.")


(defvar el2markdown-post-convert-hook nil
  "Hook that is run after a buffer has been converted to MarkDown.

The functions in the hook should accept one argument, the output
stream (typically the destination buffer).  When the hook is run
current buffer is the source buffer.")

;;; Functions

;;;; Comment

;;;###autoload
(defun el2markdown-view-buffer ()
  "Convert comment section to markdown and display in temporary buffer."
  (interactive)
  (with-output-to-temp-buffer "*el2markdown*"
    (el2markdown-convert)))


;;;###autoload
(defun el2markdown-write-file (&optional file-name overwrite-without-confirm)
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
          (el2markdown-convert))
        ;; Note: Must set `require-final-newline' inside
        ;; `with-temp-buffer', otherwise the value will be overridden by
        ;; the buffers local value.
        (let ((require-final-newline nil))
          (write-file file-name (not overwrite-without-confirm)))))))


;;;###autoload
(defun el2markdown-write-readme ()
  "Generate README.md, designed to be used in batch mode."
  (interactive)
  (el2markdown-write-file "README.md" noninteractive))


(defun el2markdown-convert (&optional include-end)
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

    ;; TODO: move this code thing into -convert-code
    ;; (el2markdown-emit-header 1 "Code")
    (forward-line)

    (el2markdown-convert-code)
    (terpri)

    (terpri)

    (princ "---")
    (terpri)
    (terpri)
    (princ
     (format "*Last updated: %s*"
             (format-time-string "%B %e, %Y"
                                 (current-time))))
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


(defun el2markdown-skip-empty-lines ()
  "Move point to the beginning of the next non-empty line."
  (while (and (looking-at "^[ \t]*$")
              (not (eobp)))
    (forward-line)))


;; Some packages place lincense blocks in the commentary section,
;; ignore them.
(defun el2markdown-skip-license ()
  "Skip license blocks."
  (when (looking-at  "^;;; License:[ \t]*$")
    (forward-line)
    (while (not (or (eobp)
                    (looking-at "^;;;")))
      (forward-line))))


(defun el2markdown-translate-string (string)
  (let ((res "")
        (end-quote (if el2markdown-translate-keys-within-markdown-quotes
                       "[`']"
                     "'")))
    (while (string-match (concat "`\\([^`']*\\)" end-quote) string)
      (setq res (concat res (substring string 0 (match-beginning 0))))
      (let ((content (match-string 1 string))
            (beg "`")
            (end "`"))
        (setq string (substring string (match-end 0)))
        (when (save-match-data
                (let ((case-fold-search nil))
                  (string-match (concat "^\\([SCM]-[^`']+\\|"
                                        (regexp-opt el2markdown-keys)
                                        "\\)$") content)))
          (setq beg "<kbd>")
          (setq end "</kbd>"))
        (setq res (concat res beg content end))))
    (concat res string)))


(defun el2markdown-convert-title ()
  (when (looking-at ";;+ \\(.*\\)\\.el --+ \\(.*\\)$")
    (let ((package-name (match-string-no-properties 1))
          (title        (match-string-no-properties 2)))
      (when (string-match " *-\\*-.*-\\*-" title)
        (setq title (replace-match "" nil nil title)))
      (el2markdown-emit-header 1 (concat package-name " - " title)
                               "λ ")
      (forward-line))))


(defun el2markdown-convert-formal-information ()
  (save-excursion
    (goto-char (point-min))
    (let ((limit (save-excursion
                   (re-search-forward "^;;; Commentary:$" nil t))))
      (when limit
        (el2markdown-convert-formal-information-item "Author"  limit)
        (el2markdown-convert-formal-information-item "Version" limit)
        (el2markdown-convert-formal-information-item "URL"     limit 'link)
        (terpri)))))


(defun el2markdown-convert-formal-information-item (item lim &optional link)
  (when (re-search-forward (concat "^;;+ " item ": *\\(.*\\)") nil t)
    (let ((s (match-string-no-properties 1)))
      (if link
          (setq s (concat "[" s "](" s ")")))
      (princ (concat "*" item ":* " s "<br>"))
      (terpri))))


(defun el2markdown-skip-to-commentary ()
  (if (re-search-forward ";;; Commentary:$" nil t)
      (forward-line)))



(defun el2markdown-convert-section ()
  (el2markdown-skip-empty-lines)
  (el2markdown-skip-license)
  (if (or (looking-at  "^;;; Code:$")
          (eobp))
      nil
    (let ((p (point)))
      (el2markdown-emit-rest-of-comment)
      (not (eq p (point))))))

;;; Code parsing logic:

;; - emit code till next cob or coh or cow

;; - iff at cob:
;;   - if has cow before next cob or coh:
;;     - emit text till cow
;;       emit cow
;;       emit text till next cob or coh
;;   - else:
;;     - emit code till next cob or coh
;; - iff at coh:
;;   - if has cow before next cob or coh:
;;     - emit text till cow
;;       emit cow
;;       emit text till next cob or coh
;;   - else:
;;     - emit code till next cob or coh
;; - iff at cow:
;;   - emit code till next cow (capture whole cow)
;;     emit text till next cob or coh

;;; New code parsing logic:

;; - emit code till next heading or cow

;; - iff at heading:
;;   - if has cow before next heading
;;     - emit text till next cow
;;       emit cow
;;       emit text till next heading
;;   - else:
;;     - emit code till next heading
;; - iff at cow:
;;   - emit code till next cow (capture whole cow)
;;     emit text till next heading

;;; -- new stuff start --------------------------------------------------------

;; --

;; OBSOLETE
;; (defun el2markdown-handle-cob-or-coh-body ()
;;   ;; is there no cow in this body?
;;   (if (not (eq 'cow
;;                (cdr-safe
;;                 (el2markdown-get-till-next '(cob coh cow)))))
;;       ;; there is no cow in this body, so all is code
;;       (el2markdown-emit-till-next '(cob coh) 'code)
;;     ;; there /is/ a cow in this body
;;     (el2markdown-handle-cow-body)))

;; NEW
(defun el2markdown-handle-heading-body ()
  ;; is there no cow in this body?
  (if (not (eq 'cow
               (cdr-safe
                (el2markdown-get-till-next '(heading cow)))))
      ;; there is no cow in this body, so all is code
      (el2markdown-emit-till-next '(heading) 'code)
    ;; there /is/ a cow in this body
    (el2markdown-handle-cow-body)))

;; UPDATED
(defun el2markdown-handle-cow-body ()
  (and
   ;; emit text till cow
   (el2markdown-emit-till-next '(cow) 'text)
   (forward-line)
   ;; does this cow have a closing cow?
   (if (eq 'cow
           (cdr-safe
            (el2markdown-get-till-next '(heading cow))))
       ;; yes, it does
       (progn
         (and
          ;; finish cow
          (el2markdown-emit-till-next '(cow) 'code)
          (forward-line 1)
          ;; rest as text if necessary
          (el2markdown-emit-till-next '(heading) 'text)))
     ;; no, it doesn't have a closing cow
     (el2markdown-emit-till-next '(heading) 'code))))

;; UPDATED
(defun el2markdown-convert-code ()
  (if (eobp)
      nil
    ;; start
    (when-let* ((c-type
                 (el2markdown-emit-till-next '(heading cow) 'code)))
      ;; rest, main loop
      (while
          (if (eobp)
              nil
            ;; conditional
            (pcase c-type
              ;; ('cob
              ;;  (el2markdown-emit-cob)
              ;;  (setq c-type (el2markdown-handle-cob-or-coh-body)))
              ;; ('coh
              ;;  (el2markdown-emit-coh)
              ;;  (setq c-type (el2markdown-handle-cob-or-coh-body)))
              ('heading
               (el2markdown-emit-heading)
               (setq c-type (el2markdown-handle-heading-body)))
              ('cow
               (setq c-type (el2markdown-handle-cow-body)))
              (_ (error "Invalid c-type: %s" c-type))))))))

(defun el2markdown-emit-till-next (till-any emit-type)
  (when-let* ((pair (el2markdown-get-till-next till-any))
              (till (car pair))
              (c-type (cdr pair)))
    (and (el2markdown-emit-till-point till emit-type)
         c-type)))

;; UPDATED
(defun el2markdown-get-till-next (till-any)
  ;; check for errors
  (unless (and (listp till-any)
               (-every? (lambda (e) (-contains? '(;; cob coh
                                             cow heading) e))
                        till-any))
    (error "Expected list of any '(cow heading): %S" till-any))
  ;; main
  (let* ((start (point))
         (_ (message "DEBUG: start: %s" start))
         ;; (next-cob-dist
         ;;  (when (-contains? till-any 'cob)
         ;;    (save-excursion
         ;;      (when (re-search-forward el2markdown-cob-regexp nil t)
         ;;        (let ((new-point
         ;;               (match-beginning 0)))
         ;;          (when (>= new-point start)
         ;;            new-point))))))
         ;; (_ (message "DEBUG: next-cob-dist: %s" next-cob-dist))
         ;; (next-coh-dist
         ;;  (when (-contains? till-any 'coh)
         ;;    (save-excursion
         ;;      (re-search-forward el2markdown-coh-regexp nil t))))
         ;; (_ (message "DEBUG: next-coh-dist: %s" next-coh-dist))
         ;; only needed ---
         (next-heading-dist
          (when (-contains? till-any 'heading)
            (save-excursion
              (re-search-forward el2markdown-heading-regexp nil t))))
         (_ (message "DEBUG: next-heading-dist: %s" next-heading-dist))
         (next-cow-dist
          (when (-contains? till-any 'cow)
            (save-excursion
              (re-search-forward el2markdown-cow-regexp nil t))))
         (_ (message "DEBUG: next-cow-dist: %s" next-cow-dist))
         ;; only needed ---
         (values-lst `(,next-heading-dist ,next-cow-dist)))
    (if (-every? #'null values-lst)
        (cons (point-max) 'max)
      (if-let*
          ((_ (message "DEBUG: values: %s" values-lst))
           (vars-lst '(heading cow))
           ;; order doesn't matter
           (smallest-value
            (-some-> values-lst
              (-non-nil)
              (-min)))
           (_ (message "DEBUG: smallest-value: %s" smallest-value))
           ;; retrieve key with value
           (smallest-var
            (nth (--find-index (and it (= smallest-value it))
                               values-lst)
                 vars-lst))
           (_ (message "DEBUG: 2: %s" smallest-var))
           (result (cons smallest-value smallest-var)))
          ;; do if true
          (progn
            (message "closest-dist: %s" result)
            result)
        ;; do if else
        (progn
          (message "no matches")
          nil)))))

(defun el2markdown-emit-till-point (till-point emit-type)
  (unless (and (symbolp emit-type)
               (-contains? '(code text) emit-type))
    (error "Expected symbol of one of '(code text): %S" emit-type))
  (let ((start (point)))
    ;; fix till-point to be beginning of line
    (setq till-point
          (save-excursion
            (goto-char till-point)
            (beginning-of-line)
            (point)))
    (if (not (> (- till-point start) 1))
        (forward-line)
        t
      (pcase emit-type
        ('code (goto-char till-point)
               (let ((main-text (buffer-substring-no-properties start (point))))
                 (unless (string-blank-p main-text)
                   ;; begin printing
                   (princ "```emacs-lisp")
                   (terpri)
                   (princ  main-text)
                   (princ "```")
                   (terpri)
                   (terpri)))
               t)
        ('text (beginning-of-line)
               (when (< (point) till-point)
                 (el2markdown-emit-rest-of-comment)
                 (while (< (point) till-point)
                   (forward-line)
                   (el2markdown-emit-rest-of-comment))
                 (forward-line 1))
               t)
        (_ (error "no matching emit type" emit-type))))))

;; --

(defun el2markdown-emit-till-next-cob ()
  (let ((start (point)))
    (when (re-search-forward "^;;;+$" nil t)
      (beginning-of-line)
      ;; verify it's a comment block
      (when (looking-at el2markdown-cob-regexp)
        (princ "```emacs-lisp")
        (terpri)
        (princ (buffer-substring-no-properties start (point)))
        (princ "```")
        (terpri)
        (terpri)))))


(defun el2markdown-emit-till-end-of-buffer ()
  (princ "```emacs-lisp")
  (terpri)
  (princ (buffer-substring-no-properties (point) (point-max)))
  (terpri)
  (princ "```")
  (terpri)
  (goto-char (point-max)))


;; (defun el2markdown-emit-cob ()
;;   (when (looking-at el2markdown-cob-regexp)
;;     (let ((title (match-string 1)))
;;       (forward-line 3)               ; Move past the comment block into the body
;;       (el2markdown-emit-header 1 title "% ")
;;       ;; (terpri)
;;       )))

;; (defun el2markdown-emit-coh ()
;;   (when (looking-at el2markdown-coh-regexp)
;;     (let ((title (match-string 1)))
;;       (forward-line 1)               ; Move past the comment block into the body
;;       (el2markdown-emit-header 2 title "> ")
;;       ;; (terpri)
;;       )))

;; NEW
(defun el2markdown-emit-heading ()
  (when (looking-at el2markdown-heading-regexp)
    (let* ((semicolons (match-string 1))
           (title (match-string 2))
           (depth (- (length semicolons) 2))
           (symbol
            (pcase depth
              (1 "% ")
              (2 "> ")
              (3 "‣ "))))
      (forward-line 1)               ; Move past the comment block into the body
      (el2markdown-emit-header depth title symbol)
      ;; (terpri)
      )))

;; "⋮ "
;; "⋯ "
;; "↳ "
;; "λ "
;; "○ "
;; "‣ "
;;; "⊢ "

;;; -- new stuff end ----------------------------------------------------------

(defun el2markdown-emit-header (count title &optional prefix postfix)
  (princ (make-string count ?#))
  (princ " ")
  (when prefix
    (princ prefix))
  ;; Strip trailing ".".
  (let ((len nil))
    (while (progn
             (setq len (length title))
             (and (not (equal len 0))
                  (eq (elt title (- len 1)) ?.)))
      (setq title (substring title 0 (- len 1)))))
  (princ (el2markdown-translate-string title))
  (when postfix
    (princ postfix))
  (terpri)
  (terpri))


(defun el2markdown-is-at-bullet-list ()
  "Non-nil when next non-empty comment line is a bullet list."
  (save-excursion
    (while (looking-at "^;;$")
      (forward-line))
    ;; When more then 4 spaces, the line is a code block.
    (looking-at ";;+ \\{0,4\\}[-*]")))

(defun el2markdown-emit-rest-of-comment ()
  (let ((first t))
    (while (and (looking-at "^;;")
                (not (looking-at "^;; --$")))
      ;; Skip empty lines.
      (while (looking-at el2markdown-empty-comment)
        (forward-line))
      (if (and (looking-at ";;+ \\(.*\\):$")
               (save-excursion
                 (save-match-data
                   (forward-line)
                   (looking-at el2markdown-empty-comment)))
               ;; When preceding code or bullet list, don't treat as
               ;; sub-header.
               (or first
                   (not (save-excursion
                          (save-match-data
                            (forward-line)
                            (while (looking-at el2markdown-empty-comment)
                              (forward-line))
                            (or (el2markdown-is-at-bullet-list)
                                (looking-at ";;+ *(")
                                (looking-at ";;+     ")))))))
          ;; Header
          (progn
            (el2markdown-emit-header (if first 2 3)
                                     (match-string-no-properties 1))
            (forward-line 2))
        ;; Section of text. (Things starting with a parenthesis is
        ;; assumes to be code.)
        (let ((is-code (looking-at ";;+ *("))
              (is-bullet (el2markdown-is-at-bullet-list)))
          (while (looking-at ";;+ ?\\(.+\\)$")
            (if is-code
                (princ "    "))
            (princ (el2markdown-translate-string
                    (match-string-no-properties 1)))
            (terpri)
            (forward-line))
          ;; Insert empty line between sections of code (unless
          ;; between bullet lists.)
          (if (and is-bullet
                   (el2markdown-is-at-bullet-list))
              nil
            (terpri))))
      (setq first nil))))

;;; end

(provide 'el2markdown-fork)

;;; el2markdown-fork.el ends here
