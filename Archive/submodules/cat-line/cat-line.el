;;; cat-line.el --- a mode-line framework            -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lispcat

;; Author: lispcat <187922791+lispcat@users.noreply.github.com>
;; Keywords: extensions

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

;; TODO

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;;; Vars:

(defgroup cat-line nil
  "A personal mode-line."
  :group 'mode-line)

(defcustom cat-line-height '(0.20 . 0.25)
  "Height of the mode-line, above and below."
  :type '(cons (float :tag "Top spacing")
               (float :tag "Bottom spacing"))
  :group 'cat-line)

(defcustom cat-line-position 'bottom
  "Position for modeline."
  :type '(choice (symbol :tag "Top" 'top)
                 (symbol :tag "Bottom" 'bottom))
  :group 'cat-line)

;;;; Faces:

(defface cat-line-empty-face
  `((t (:foreground ,(face-foreground 'default))))
  "Empty face for resetting header-line.")

(defface cat-line-face-doc-no-slant
  '((t font-lock-doc-face (:slant normal)))
  "Basically `font-lock-doc-face' but with no slant.")

(defface cat-line-header-line-active
  '((t :inherit mode-line-active))
  "A copy of mode-line-active for the header-line")

(defface cat-line-header-line-inactive
  '((t :inherit mode-line-inactive))
  "A copy of mode-line-inactive for the header-line")

;;;; Segments:

;;;;; helper segments:

(defun cat-line-segment-half-space ()
  (propertize " "
              'display '((space :relative-width 0.5))))

(defun cat-line-segment-tall-space ()
  "A space character that increases height of mode-line."
  (let* ((h-up (car cat-line-height))
         (h-down (cdr cat-line-height))
         (top (propertize " " 'display `(raise ,h-up)))
         (bot (propertize " " 'display `(raise ,(- h-down)))))
    (concat top " " bot)))

;;;;; cursor-position

(defun cat-line-segment-cursor-position ()
  (propertize (format-mode-line "%l:%c")
              'face 'shadow))

;;;;; scroll

(defun cat-line-segment--pdf-page ()
  (when (eq major-mode 'pdf-view-mode)
    (let ((page-current (image-mode-window-get 'page))
          (page-total (pdf-cache-number-of-pages)))
      (propertize (format "%d/%d " page-current page-total)))))

(defun cat-line-segment-scroll ()
  (or (cat-line-segment--pdf-page)
      (propertize (replace-regexp-in-string
                   "%" "%%" (format-mode-line "%p%") nil 'literal)
                  'face 'shadow)))

;;;;; persp-name

(defun cat-line-segment-persp-name ()
  (propertize (concat ""
                      (string-trim
                       (substring-no-properties
                        (format-mode-line persp-lighter)))
                      "")))

;;;;; buffer-status

(defun cat-line-buffer-status ()
  (if (not (buffer-file-name (buffer-base-buffer)))
      (when (buffer-narrowed-p)
        (propertize "▼"
                    'face 'font-lock-doc-face))
    (cond
     ;; narrowed
     ((buffer-narrowed-p)
      (propertize "▼"
                  'face (cond
                         ((buffer-modified-p) 'error)
                         (buffer-read-only 'shadow)
                         (t 'font-lock-doc-face))))
     ;; modified
     ((buffer-modified-p)
      (propertize "●"
                  'face 'error))
     ;; read only
     (buffer-read-only
      (propertize "■"
                  'face 'shadow)))))

;;;;; buffer-name

(defun cat-line--get-project-root ()
  (when-let*
      ((path
        (or
         ;; project?
         (and (fboundp 'project-current)
              (when-let* ((project (project-current)))
                (expand-file-name
                 (if (fboundp 'project-root)
                     (project-root project)
                   (car (with-no-warnings
                          (project-roots project)))))))
         ;; projectile?
         (and (bound-and-true-p projectile-mode)
              (projectile-project-root)))))
    ;; if project path, then abbreviate it
    (abbreviate-file-name path)))

(defun cat-line-segment-buffer-name ()
  (if-let* ((buffer-path buffer-file-name)
            (project-root (cat-line--get-project-root))
            (project-name (file-name-nondirectory
                           (directory-file-name project-root))))
      (concat
       ;; project directory
       (propertize (concat project-name "/")
                   'face
                   ;; `((:weight normal) mode-line-buffer-id)
                   `((:weight normal) mode-line-emphasis)
                   ;; `(mode-line-emphasis)
                   )
       ;; relative path
       (propertize (when-let*
                       ((relative-path
                         (file-relative-name
                          (or (file-name-directory
                               (or (abbreviate-file-name buffer-path)
                                   buffer-path))
                              "./")
                          project-root)))
                     (if (string= relative-path "./")
                         ""
                       (substring (shrink-path--dirs-internal relative-path t)
                                  1)))
                   ;; TODO: make this inherit the non-focused effects of mode-line-buffer-id
                   'face `(font-lock-string-face))
       ;; file name
       (propertize (file-name-nondirectory buffer-path)
                   'face `((:inherit bold) mode-line-buffer-id)))
    ;; fallback
    (propertize (buffer-name)
                'face `mode-line-buffer-id)))

;;;;; meow-mode

(defvar cat-line-segment-meow-state-alist
  `((normal "[N]" . (success (:inherit bold)))
    (insert "[I]" . (font-lock-keyword-face (:inherit bold)))
    (beacon "[B]" . (warning (:inherit bold)))
    (keypad "[K]" . (mode-line (:inherit bold)))
    (motion "[M]" . (cat-line-face-doc-no-slant (:inherit bold)))))

(defun cat-line-segment-meow-state ()
  (when (bound-and-true-p meow--current-state)
    (let ((icon-face-pair (alist-get meow--current-state
                                     cat-line-segment-meow-state-alist)))
      (concat (propertize (car icon-face-pair)
                          'face (cdr icon-face-pair))))))

;;;;; major-mode

(defun cat-line-segment-major-mode ()
  "Return major-mode."
  (propertize (substring-no-properties
               (format-mode-line mode-name))
              'face 'bold))

;;;;; checker

;; (defun cat-line-segment-checker ()
;;   "Return flycheck/flymake status."
;;   (cond
;;    ((bound-and-true-p flycheck-mode)
;;     mood-line-segment-checker--flycheck-text)
;;    ((bound-and-true-p flymake-mode)
;;     mood-line-segment-checker--flymake-text)))

;;;;; process

(defun cat-line-segment-process ()
  "Return `mode-line-process'."
  (let ((info (format-mode-line mode-line-process)))
    (unless (string-blank-p info)
      (string-trim info))))

;;;;; git

(defun cat-line-git--states (state)
  (when-let*
      ((sym (pcase state
              ('up-to-date "-")
              ('edited "+")
              ('needs-update "u")
              ('needs-merge "m")
              ('unlocked-changes "U")
              ('added "A")
              ('removed "R")
              ('conflict "!")
              ('missing "0")
              ('ignored "i")
              ('unregistered "?")
              (_ nil)))
       (prefix ""))
    (concat prefix sym)))

(defun cat-line-git ()
  (when vc-mode
    (when-let* ((file (buffer-file-name))
                (branch (substring-no-properties vc-mode 5))
                (state (vc-state file))
                (state (cat-line-git--states state)))
      (propertize (format "%s %s" state branch)))))

;;;;; misc-info

(defun cat-line-segment-misc-info ()
  "Return `mode-line-misc-info'."
  (let ((misc-info (format-mode-line mode-line-misc-info)))
    (unless (string-blank-p misc-info)
      (propertize (string-trim misc-info)
                  'face 'shadow))))

;;;; Collections

;;;;; helper collections:

(defun cat-line-collection-empty ()
  "A list of segments with only a tall space."
  (cat-line-segment-tall-space))

;;;;; format spec

(defvar cat-line-format-spec-default
  '(:left
    ((" " "")
     (;; (cat-line-segment-half-space)
      " "
      (cat-line-segment-meow-state)
      ;; (cat-line-segment-half-space)
      " "
      )
     (" " (cat-line-buffer-status)) ;; TODO: ((cat-line-buffer-status) :left " ")
     (" " (cat-line-segment-buffer-name) " ")
     ;; (" " (cat-line-segment-pdf-page))
     (" " (cat-line-segment-scroll))
     (" " (cat-line-segment-cursor-position))
     (nil (cat-line-segment-tall-space)))

    :right
    ((nil (cat-line-segment-misc-info) " | ")
     (nil (cat-line-git) " | ")
     (nil (cat-line-segment-persp-name) " | ")
     ;; TODO: flycheck/flymake checker
     (nil (cat-line-segment-process) " | ")
     (nil (cat-line-segment-major-mode) " ")
     ;; (" " "")
     )))

(defcustom cat-line-format-spec cat-line-format-spec-default
  "User-side specification for mode-line-format."
  :group 'cat-line
  :type '(list :tag "Left and Right segments."
               (repeat :tag "Left-bound" sexp)
               (repeat :tag "Right-bound" sexp)))

;;;;; generating content

(defvar cat-line-face-remap-cookie nil)

(defun cat-line--generate-format ()
  '(:eval (progn
            ;; (when (eq cat-line-position 'top)
            ;; (face-remap-set-base 'header-line 'cat-line-empty-face)
            (if (mode-line-window-selected-p)
                (progn
                  ;; (when cat-line-face-remap-cookie
                  ;;   (face-remap-remove-relative cat-line-face-remap-cookie)
                  ;;   (setq-local cat-line-face-remap-cookie nil))
                  ;; (face-remap-add-relative 'header-line 'mode-line)
                  ;; (face-remap-set-base 'header-line :inherit 'mode-line-active)
                  (cat-line-format-spec--process cat-line-format-spec))
              ;; (face-remap-remove-relative 'header-line)
              ;; (when cat-line-face-remap-cookie
              ;;   (face-remap-remove-relative cat-line-face-remap-cookie)
              ;;   (setq-local cat-line-face-remap-cookie nil))
              ;; (face-remap-add-relative 'header-line 'mode-line)
              ;; (face-remap-set-base 'header-line :inherit 'mode-line-inactive)
              (cat-line-collection-empty)
              ))))

;; (add-hook 'post-command-hook
;;           (defun cat-line-update-header-line ()
;;             (if (mode-line-window-selected-p)
;;                 (face-remap-set-base 'header-line :inherit 'mode-line-active)
;;               (face-remap-set-base 'header-line :inherit 'mode-line-inactive))))

(defun cat-line-format-spec--process (spec)
  "Create padded string for SPEC based on current width of window."
  (let* ((left-seq (plist-get spec :left))
         (left-seq (cat-line--process-pad left-seq))
         (left-seq (cat-line--process-eval left-seq))
         (left-str (cat-line--process-concat left-seq))
         (right-seq (plist-get spec :right))
         (right-seq (cat-line--process-pad right-seq))
         (right-seq (cat-line--process-eval right-seq))
         (right-str (cat-line--process-concat right-seq)))
    (let ((res (concat
                left-str
                (propertize " "
                            'display `((space :align-to (- right (- 0 right-margin)
                                                           ,(length right-str)))))
                right-str)))
      ;; (face-remap-set-base 'header-line :inherit 'mode-line-active)
      res
      ;; (mapcar (lambda (str)
      ;;           (let ((s (copy-sequence str)))
      ;;             (add-face-text-property 0 (length s)
      ;;                                     `(:background ,(face-attribute 'mode-line-active
      ;;                                                                    :background nil t))
      ;;                                     t
      ;;                                     s)
      ;;             s))
      ;;         res)

      ;; (propertize res
      ;;             'face `(:background ,(face-attribute 'mode-line-active
      ;;                                                  :background nil t)))
      )))

(defun cat-line--process-pad (segments)
  (mapcar (lambda (args)
            (let ((a (nth 0 args))
                  (b (nth 1 args))
                  (c (nth 2 args)))
              ;; (message "DEBUG: %S: [%S %S %S]" segments a b c)
              `(when ,b
                 (concat ,a ,b ,c))))
          segments))

(defun cat-line--process-eval (segments)
  (mapcar #'eval segments))

(defun cat-line--process-concat (segments)
  (mapconcat #'identity segments))

;; TODO: this doesn't eval deeper?
;; (defun cat-line--eval-concat-segments (segments)
;;   (let* ((padded (mapcar (lambda (args)
;;                            `(apply #'cat-line-format-pad ,args))
;;                          segments))
;;          (evaled (message "DEBUG: padded: ")
;;                  (mapcar #'eval
;;                          padded))
;;          (concatted (mapconcat #'identity
;;                                evaled)))
;;     concatted))

;;;; Mode definition

(defun cat-line--revert-for-all-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (force-mode-line-update t))))

(defvar cat-line--orig-format-bottom nil)
(defvar cat-line--orig-format-top nil)
(defvar cat-line--current-position nil)

(defun cat-line--init-bar ()
  (setq cat-line--current-position cat-line-position)
  (pcase cat-line-position
    ('bottom
     ;; save defaults
     (setq cat-line--orig-format-bottom mode-line-format)
     (setq cat-line--orig-format-top nil)
     ;; enable
     (setq-default mode-line-format (cat-line--generate-format)))
    ('top
     ;; save defaults
     (setq cat-line--orig-format-bottom mode-line-format)
     (setq cat-line--orig-format-top header-line-format)
     ;; enable
     (setq-default mode-line-format nil)
     (setq mode-line-format nil)
     (setq-default header-line-format (cat-line--generate-format)))
    (_ (error "Invalid `cat-line-position': %S" cat-line-position)))
  ;; update
  (cat-line--revert-for-all-buffers))

(defun cat-line--deinit-bar ()
  ;; restore mode-line
  (setq-default mode-line-format cat-line--orig-format-bottom)
  (setq cat-line--orig-format-bottom nil)
  ;; restore header-line if position: top
  (when cat-line--current-position
    (setq-default header-line-format cat-line--orig-format-top)
    (setq cat-line--orig-format-top nil))
  ;; update
  (cat-line--revert-for-all-buffers))

;;;###autoload
(define-minor-mode cat-line-mode
  "Toggle cat-line."
  :group 'cat-line
  :global t
  :lighter nil
  (if cat-line-mode
      (cat-line--init-bar)
    (cat-line--deinit-bar)))

;;;; Caching (implement in the future)

;; TODO: it would be a lot cleaner if i enforce manually creating a cache with
;; depend-names, before the function definition (only name, no funcname needed).

;; (defvar cat-line-cache (make-hash-table :test 'eq :size 10)
;;   "A cache to store previous cat-line segment outputs.
;; Each key is a symbol and each value is an alist of the form:

;; (('cached . previous-result)
;;  ('depend-names . list-of-variables-to-check)
;;  ('depend-values . last-checked-value-of-each-in-depend-names))")

;; (cl-defmacro cat-line-cache-new (name depend-names)
;;   (declare (indent defun))
;;   ;; type checking
;;   (unless (symbolp name) (error "symbolp: %S" name))
;;   (unless (listp depend-names) (error "listp: %S" depend-names))
;;   ;; make and set
;;   `(let ((alist (list
;;                  (cons 'cached nil)
;;                  (cons 'depend-names ,depend-names)
;;                  (cons 'depend-values ,(mapcar (lambda (sym)
;;                                                  (when (boundp sym)
;;                                                    (symbol-value sym)))
;;                                                depend-names)))))
;;      (prog1 alist
;;        (puthash ,name alist))))

;; (defun cat-line-cache-update-cached (name cache-new)
;;   "Update the value of attr 'cached with CACHE-NEW for key NAME in hash."
;;   (setcdr (assq 'cached (gethash name cat-line-cache))
;;           cache-new))

;; (defmacro cat-line-use-cache-if-valid (name &rest body)
;;   (declare (indent defun))
;;   (if-let* (;; get existing hash entry
;;             (entry (gethash name cat-line-cache))
;;             ;; get entry's attrs
;;             (depend-names (alist-get 'depend-names entry))
;;             (depend-values (alist-get 'depend-values entry))
;;             ;; get current values of depend-values
;;             (depend-currrent
;;              (mapcar (lambda (sym)
;;                        (when (boundp sym)
;;                          (symbol-value sym)))
;;                      depend-values))
;;             ;; if depend-values and depend-current are equal,
;;             ;; the cache is still valid. fetch from entry.
;;             (valid-cache
;;              (when (equal depend-values depend-current)
;;                (alist-get 'cached entry))))
;;       ;; cache is valid, return.
;;       valid-cache
;;     ;; otherwise, result = eval body, save result to cache, return result.
;;     (message "cat-line: Cache for %s not registered" name)
;;     (let ((ret (progn ,@body)))
;;       (prog1 ret
;;         (cat-line-cache-update-cached name ret)))))

;;; End:

(provide 'cat-line)
;;; cat-line.el ends here
