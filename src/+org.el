;;; +org.el --- org mode setup                     -*- lexical-binding: t; -*-

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

;; Configuration for Org-mode and friends.

;; Org is a markup language and plain text file format, much like Markdown, but
;; a lot more powerful.

;;; Code:

;; NOTE: ensure that the newest version of org is installed right after elpaca
;; setup

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     org                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General config options for org-mode.

;; --

(leaf org :elpaca nil
  :custom
  ;; default org directory
  (org-directory . "~/Notes/org")

  ;; column where tags are indented to
  (org-tags-column . -55)

  ;; default folding mode
  (org-startup-folded . 'nofold)

  ;; indent headings and its body
  (org-startup-indented . t)

  ;; more ergonomic C-a/C-e
  (org-special-ctrl-a/e . t)

  ;; edit src blocks in the same window
  (org-src-window-setup . 'current-window)

  ;; RET can open links
  (org-return-follows-link . t)

  ;; hide formatting chars (* / ~ = etc)
  (org-hide-emphasis-markers . t)

  ;; remove annoying leading whitespace in code blocks
  (org-src-preserve-indentation . t)

  ;; TODO: not sure what this does
  ;; (org-fontify-whole-heading-line . t)

  ;; custom ellipses when folded
  ;; (org-ellipsis . " ‣")
  (org-ellipsis . " ›")
  ;; (org-ellipsis . " …")
  ;; (org-ellipsis . " ⤵")
  ;; (org-ellipsis . " ▾")

  :init
  (leader-bind
    "o" '(:ignore t :wk "org"))

  :config

  ;; set org font sizes
  (dolist
      ;; (pair '((org-document-title :height 1.9 :weight bold)
      ;;         (org-level-1 :height 1.7 :weight bold)
      ;;         (org-level-2 :height 1.4 :weight bold)
      ;;         (org-level-2 :height 1.1)
      ;;         (org-level-3 :height 1.1)))
      (pair '((org-document-title :height 1.9)))
    (apply #'set-face-attribute (car pair) nil (cdr pair)))

  ;; fix syntax <> matching with paren
  (add-hook 'org-mode-hook (lambda ()
                             (modify-syntax-entry ?< ".")
                             (modify-syntax-entry ?> ".")))


  ;; keywords override

  (defun +org-todo-color-override (&rest _)
    "Set org-todo-keyword-faces only if not already set by the theme."
    (setq org-todo-keyword-faces
          `(("NEXT" :foreground ,(or (ignore-error
                                         (face-attribute 'highlight :foreground nil 'default))
                                     "yellow")))))

  ;; Advise the load-theme function to run our color override
  (advice-add 'load-theme :after #'+org-todo-color-override)

  ;; Run once immediately to set colors if no theme is loaded
  (+org-todo-color-override)

  ;; Shortcut for M-RET M-<right>
  ;; In org-mode, this usually translates to either:
  ;; - new subheading
  ;; - new sublist
  (defun +org-meta-ret-meta-right ()
    "Shortcut for M-RET M-<right>."
    (interactive)
    (org-meta-return)
    (org-metaright))

  :bind
  (org-mode-map
   ("C-M-<return>" . +org-meta-ret-meta-right)))

;; --

;;; -- org-tempo --------------------------------------------------------------

;; Ease the creation of src blocks (code blocks).

;; To create src blocks, usually, you would press `C-c C-,` or run
;; `org-insert-structure-template'.

;; But with `org-tempo', with the config below, you can type `<sh TAB` to create
;; an src block like this:
;;
;; ```
;; #+begin_src shell
;;
;; #+end_src
;; ```
;;

;; --

(leaf org-tempo :elpaca nil
  :after org
  :config
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c")))

;; --

;;; -- org-download -----------------------------------------------------------

(leaf org-download
  :after org
  :config
  (org-download-enable)
  :setq-default
  (org-download-image-dir . "_images"))

;;; -- org-bullets ------------------------------------------------------------

;; TODO: replace with org-superstar
(leaf org-bullets
  :hook org-mode-hook
  :setq
  (org-bullets-bullet-list
   . '("◉"
       "●"
       "○"
       "■"
       "□"
       "✦"
       "✧"
       "✿")))

;;; -- toc-org ----------------------------------------------------------------

(leaf toc-org
  :hook org-mode-hook)

;;; -- anki-editor ------------------------------------------------------------

(leaf anki-editor
  :commands (anki-editor-push-note-at-point
             anki-editor-push-notes
             anki-editor-push-new-notes)
  :setq
  (anki-editor-latex-style . 'mathjax)
  :defer-config
  (defun +ensure-anki-editor-mode (note)
    "Ensure `anki-editor-mode' is enabled before pushing notes."
    (unless anki-editor-mode
      (anki-editor-mode 1)))
  (advice-add #'anki-editor--push-note :before #'+ensure-anki-editor-mode))

;;; -- image-slicing ----------------------------------------------------------

(leaf image-slicing :ensure nil
  :hook org-mode-hook
  :setq
  (image-slicing-newline-trailing-text . nil))

;;; -- org-auto-tangle --------------------------------------------------------

(leaf org-auto-tangle
  :hook org-mode-hook)

;;; -- org-agenda -------------------------------------------------------------

(leaf org-agenda :elpaca nil
  :after org
  :init
  (leader-bind
    "oa" 'org-agenda)

  :bind (org-agenda-mode-map
         (")" . 'org-agenda-todo))

  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)"
                    "|"
                    "DONE(d/!)")))
  (setq org-agenda-files
        (list ;; "~/Notes/org/Inbox.org"
         ;; "~/Notes/org/agenda.org"
         "~/Notes/denote/20250728T235116--todo__todo.org"))
  (defun +open-org-agenda-file ()
    (interactive)
    (find-file (car org-agenda-files)))

  (leader-bind
    "oo" '+open-org-agenda-file)

  (setq org-tag-alist
        '(;; Places
          ("@home"   . ?H)
          ("@school" . ?S)
          ;; ("@work" . ?W)
          ;; Activities
          ("@task" . ?t)
          ("@studying" . ?s)
          ("@errands"  . ?e)
          ("@tidy" . ?y)
          ("@creative" . ?c)
          ("@art" . ?a)
          ("@programming" . ?p)
          ("@today" . ?T)
          ;; ("@calls" . ?l)
          ;; Devices
          ("@phone" . ?P)
          ("@computer" . ?C)))
  (setq org-agenda-prefix-format
        `((agenda
           . ,(concat " %i "
                      "%?-12t"
                      "[%3(+org-get-prop-effort)]    "
                      ;; "%3(+org-get-prop-effort)  "
                      "% s"))
          (todo   . " %i ")
          (tags   . " %i %-12:c")
          ;; (search . " %i %-12:c")
          (search . " %c")
          ))

  (defun +org-get-prop-effort ()
    (if (not (eq major-mode 'org-mode)) ""
      (let ((val (org-entry-get nil "EFFORT")))
        (if (not val) ""
          (format "%s" (string-trim val))))))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t))

;;; -- org-super-agenda -------------------------------------------------------

(leaf org-super-agenda
  :after org-agenda
  :require t
  :config
  (org-super-agenda-mode 1)
  :setq
  (org-agenda-custom-commands
   . `(
       ("a" "main agenda"
        ((agenda ""
                 ((org-agenda-show-future-repeats nil)
                  (org-agenda-start-on-weekday nil)
                  (org-agenda-span 'week)
                  (org-habit-show-habits nil)
                  (org-agenda-skip-deadline-if-done t)
                  (org-agenda-skip-scheduled-if-done t)))
         (todo "NEXT")
         (agenda ""
                 ((org-agenda-span 1)
                  (org-agenda-use-time-grid nil)
                  (org-super-agenda-groups
                   '((:name none
                            :habit t)
                     (:discard (:anything t)))))))))))

;;;; -- org-ql ----------------------------------------------------------------

(leaf org-ql
  :after org)

;;; -- org-pomodoro -----------------------------------------------------------

(leaf org-pomodoro
  :after org)

;;; -- org-noter --------------------------------------------------------------

(leaf org-noter
  :after org
  :bind (("C-c o n" . org-noter)
         ("C-c d n" . org-noter-start-from-dired)
         ("C-c o p" . +org-noter-set-prop-current-page))
  :setq
  (org-noter-doc-split-fraction . '(0.7 . 0.6))
  :config
  (defun +org-noter-set-prop-current-page (arg)
    "Set the property `NOTER_PAGE' of the current org heading to the current noter page.
The property will be removed if ran with a \\[universal-argument]."
    (interactive "P")
    (org-noter--with-selected-notes-window
     (if (equal arg '(4))
         (org-delete-property "NOTER_PAGE")
       (when-let ((vec (org-noter--get-current-view))
                  (num (and (vectorp vec)
                            (> (length vec) 1)
                            (format "%s" (aref vec 1)))))
         (message "meow: %s" num)
         (org-entry-put (point) "NOTER_PAGE" num))))))

;;; -- org-capture ------------------------------------------------------------

(leaf org-capture :elpaca nil
  :after org
  :init
  (leader-bind
    "oc" 'org-capture)

  :config
  (defun +get-org-agenda-denote-file (name)
    (let ((regex (format "^.*--%s__.*\\.org$" name)))
      (car (seq-filter
            (lambda (path)
              (string-match regex (file-name-nondirectory path)))
            org-agenda-files))))

  (setq org-capture-templates
        `(("t" "Tasks")

          ("td" "Todo with deadline" entry
           (file ,(+get-org-agenda-denote-file "agenda"))
           "* TODO %^{Task}\nDEADLINE: %^{Deadline}t\n%?\n"
           :empty-lines 1
           :immediate-finish nil)

          ("tp" "Task" entry
           (file ,(+get-org-agenda-denote-file "agenda"))
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("n" "New note (with Denote)" plain
           (file denote-last-path)
           #'denote-org-capture :no-save t :immediate-finish nil
           :kill-buffer t :jump-to-captured t))))

;;; -- more org-anki stuff ----------------------------------------------------

(defun +org-priority-to-anki ()
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

(defun +org-clone-with-fraction (days time effort)
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

;;; -- visual fill column -----------------------------------------------------

(leaf visual-fill-column
  :require t
  :hook ((org-mode-hook . +org-visual-fill))
  :init
  (defun +org-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1)))


(provide '+org)
;;; +org.el ends here



