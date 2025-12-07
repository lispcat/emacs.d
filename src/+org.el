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

;;; Org-mode

;; General config options for org-mode.

;; --

(setup org
  (:option org-directory "~/Notes/org"  ; default org directory

           ;; Startup:
           org-startup-folded 'showall ; default folding mode (def: 'showeverything)
           org-startup-indented t      ; indent with heading depth

           ;; UI
           org-src-window-setup 'current-window ; edit src blocks in the same window
           org-src-preserve-indentation t ; remove leading whitespace in src-blocks
           org-tags-column -45            ; tag indent column
           org-cycle-hide-drawer-startup t ; hide drawers

           ;; Keyboard:
           org-special-ctrl-a/e t       ; better C-a/C-e
           org-return-follows-link t    ; RET can open links

           ;; Visual:
           org-hide-emphasis-markers t  ; hide formatting chars (* / ~ = etc)
           org-ellipsis                 ; custom ellipses when folded
           " ‣"
           ;; " ›"
           ;; " …"
           ;; " ⤵"
           ;; " ▾"
           )

;;;; keys

  (defun +org-meta-ret-meta-right ()
    "Shortcut for M-RET M-<right>."
    (interactive)
    (org-meta-return)
    (org-metaright))

  (:with-map org-mode-map
    (:bind "C-M-<return>" +org-meta-ret-meta-right))

;;;; fonts

  (:when-loaded
    (defvar +org-fonts-alist
      '((org-document-title :height 1.9 :weight bold)
        (org-level-1 :height 1.7)
        (org-level-2 :height 1.4)
        (org-level-3 :height 1.15)
        (org-level-4 :height 1.1)))

    ;; (with-eval-after-load 'ef-themes
    ;;   (setq ef-themes-headings +org-fonts-alist))

    ;; (with-eval-after-load 'modus-themes
    ;;   (setq modus-themes-headings +org-fonts-alist))

    ;; (with-eval-after-load 'kaolin-themes
    ;;   (setq kaolin-themes-org-scale-headings nil))

    ;; for each FACE, if not yet set to target, set.
    (defun +org-fonts-setup (&rest _args)
      (interactive)
      (when (eq major-mode 'org-mode)
        (dolist (lst-face +org-fonts-alist)
          (-let* (((t-face . t-args) lst-face)
                  ;; form: '((t-attr (c-val t-val)) ...)
                  (t-attr-c-t-val-alist
                   (->> t-args
                        (-partition 2)
                        (-map (lambda (pair)
                                (-let* (((t-attr t-val) pair)
                                        (c-val (face-attribute t-face t-attr)))
                                  (list t-attr (list c-val t-val)))))))
                  ;; form: '(bool ...)
                  (eq-c-t-lst
                   (->> t-attr-c-t-val-alist
                        (-map (lambda (pair)
                                (-let (((t-attr (c-val t-val)) pair))
                                  (equal c-val t-val))))))
                  ;; form: '(bool ...)
                  (eq-c-t-every?
                   (-all? #'identity eq-c-t-lst)))
            ;; if all cur eq target, then ok
            (if eq-c-t-every?
                (when debug-on-error
                  (message "Log: ok: %S, %S" t-face t-attr-c-t-val-alist))
              ;; else, set to target
              (message "Log: setting: %S, %S" t-face t-attr-c-t-val-alist)
              (apply #'set-face-attribute t-face nil t-args))))))

    ;; (advice-add 'load-theme :after #'+org-fonts-setup)
    ;; (add-hook 'org-mode-hook #'+org-fonts-setup)
    )

  ;; colorize "NEXT" todo face

  (:when-loaded
    (defun +org-todo-color-override (&rest _)
      "Set org-todo-keyword-faces only if not already set by the theme."
      (setq org-todo-keyword-faces
            `(("NEXT" . ( :foreground ,(face-foreground 'font-lock-string-face nil)
                          :weight bold
                          :inhert (org-todo)
                          ))
              ("PLAN" . ( :foreground ,(face-foreground 'font-lock-function-name-face nil)
                          :weight bold
                          :inhert (org-todo)
                          )))))

    (+org-todo-color-override)
    (advice-add 'load-theme :after #'+org-todo-color-override))

  ;; fix syntax "<" ">" matching with paren

  (:when-loaded
    (add-hook 'org-mode-hook
              (lambda ()
                (modify-syntax-entry ?< ".")
                (modify-syntax-entry ?> "."))))

;;;; export

  ;; org export options

  (:option org-latex-src-block-backend 'minted
           org-latex-minted-langs
           '((python "python") (emacs-lisp "common-lisp") (cc "c++")
             (shell-script "bash")))

  )

;; --

;;; Functionality

;;;; toc-org

(-setup toc-org
  (:hook-into org-mode-hook))

;;; Integration

;;;; anki-editor

(-setup anki-editor
  (:autoload anki-editor-push-note-at-point
             anki-editor-push-notes
             anki-editor-push-new-notes)
  (:option anki-editor-latex-style 'mathjax)
  (:when-loaded
    (defun +ensure-anki-editor-mode (note)
      "Ensure `anki-editor-mode' is enabled before pushing notes."
      (unless anki-editor-mode
        (anki-editor-mode 1)))
    (advice-add #'anki-editor--push-note :before #'+ensure-anki-editor-mode)))

;;;;; functions for anki-editor

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

;;; Quality of Life

;;;; org-tempo (disabled)

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

;; (setup org-tempo
;;   (:load-after org)
;;   (:when-loaded
;;     (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
;;     (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
;;     (add-to-list 'org-structure-template-alist '("py" . "src python"))
;;     (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
;;     (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
;;     (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
;;     (add-to-list 'org-structure-template-alist '("java" . "src java"))
;;     (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
;;     (add-to-list 'org-structure-template-alist '("clang" . "src c"))))

;; --

;;;; org-auto-tangle

(-setup org-auto-tangle
  (:hook-into org-mode-hook))

;;;; image-slicing

(setup image-slicing
  (:autoload image-slicing-mode)
  (:hook-into org-mode-hook)
  (:option image-slicing-newline-trailing-text nil))

;;; Agenda (TODO)

;; TODO: script to generate subtasks for each day for an assignment

(defun +org-agenda-open-agenda-file ()
  (interactive)
  (find-file (car org-agenda-files)))

(defun +org-agenda-mark-as-done ()
  (interactive)
  (org-agenda-todo 'done))

(setup org-agenda
  (:load-after org)

  ;; binds

  (:global
   "C-c o a" #'org-agenda
   "C-c o A" #'+org-agenda-open-agenda-file)
  (:with-map org-agenda-mode-map
    (:bind ")" #'+org-agenda-mark-as-done))

  ;; options

  (:option
   ;; only DONE if TODO subtasks are DONE
   org-enforce-todo-dependencies t

   ;; todo keywords
   org-todo-keywords
   `((sequence
      "TODO(t)" "NEXT(n)" "PLAN(p)" "|" "DONE(d/!)"))

   ;; agenda files
   org-agenda-files
   `("~/Notes/denote/20250728T235116--todo__todo.org")

   ;; org tags
   org-tag-alist
   '(;; activities:
     ("@task" . ?t)
     ("@study" . ?s)
     ;; type:
     ("@ongoing" . ?o)
     ;; classes:
     ("@cs2" . ?S)
     ("@bio" . ?B)
     ("@calc2" . ?C)
     ("@phy" . ?P))

   ;; format specification for agenda view
   org-agenda-prefix-format
   `((agenda
      . ,(concat " %i %?-12t "
                 "%-10(+org-get-prop-default) "
                 "% s"))
     (todo . " %i ")
     ;; (tags . " %i %-12:c")
     (tags . " %i ")
     (search . " %c")))

  ;; helper, used in var `org-agenda-prefix-format' above
  (defun +org-get-prop-default ()
    (apply #'format "[%s] %-2s"
           (let* ((parts-done (+org-get-prop "PARTS_DONE"))
                  (parts-total (+org-get-prop "PARTS_TOTAL"))
                  (fraction (concat (or parts-done "_")
                                    "/"
                                    (or parts-total "_"))))
             (if-let* ((parts-done-num (if (stringp parts-done)
                                           (string-to-number parts-done)
                                         0))
                       (parts-total-num (when (stringp parts-total)
                                          (string-to-number parts-total)))
                       (days-left-num (+org-get-days-left-till-deadline))
                       (parts-left-num (- parts-total-num parts-done-num))
                       (days-behind (- parts-left-num days-left-num))
                       (days-behind-result
                        (cond ((> days-behind 0)
                               (format "-%s" days-behind))
                              (t
                               (format "+%s" (abs days-behind))))))
                 (list days-behind-result fraction)
               (list "  " fraction)))))

  (defun +org-get-days-left-till-deadline ()
    (when-let* ((deadline-abs-day
                 (-some->> (+org-get-prop "DEADLINE")
                   (org-time-string-to-time)
                   (time-to-days)))
                (today-abs-day
                 (-some->> org-agenda-current-date ;; in gregorian
                   (calendar-absolute-from-gregorian)))
                (days-left
                 (- deadline-abs-day today-abs-day)))
      days-left))

  (defun +org-get-prop (prop)
    (when (eq major-mode 'org-mode)
      (let ((val (org-entry-get nil prop)))
        (if (not val) nil
          (format "%s" (string-trim val)))))))

;;;; org-habit

(setup org-habit
  (:load-after org)
  (:when-loaded
    (add-to-list 'org-modules 'org-habit t)))

;;;; org-super-agenda

(-setup org-super-agenda
  (:load-after org-agenda)
  (:when-loaded
    (org-super-agenda-mode 1))

  (setq ;; spacemacs-theme-org-agenda-height nil
   ;; org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
   org-agenda-skip-scheduled-if-done t
   org-agenda-skip-deadline-if-done t
   org-agenda-include-deadlines t
   org-agenda-include-diary t
   org-agenda-block-separator nil
   ;; org-agenda-compact-blocks t
   org-agenda-start-with-log-mode t)

  ;; (org-agenda-show-future-repeats nil)
  ;; (org-agenda-start-on-weekday nil)
  ;; (org-agenda-span 'week)
  ;; (org-habit-show-habits nil)
  ;; (org-agenda-skip-deadline-if-done t)
  ;; (org-agenda-skip-scheduled-if-done t)

  ;; (defun ri/pred-test (item)
  ;;   t)

  ;; TODO: fix this so that it works even if goto next day in agenda view!!!
  ;; (defun ri/org-parts-total-deadline-predicate (item)
  ;;   "Return t if PARTS-TOTAL is < days remaining till DEADLINE."
  ;;   (let ((marker (or (get-text-property 0 'org-marker item)
  ;;                     (get-text-property 0 'org-hd-marker item))))
  ;;     (when marker
  ;;       (let* ((parts-total (org-entry-get marker "PARTS_TOTAL"))
  ;;              (parts-total-num (when (and parts-total (stringp parts-total))
  ;;                                (string-to-number parts-total)))
  ;;              (deadline (org-entry-get marker "DEADLINE"))
  ;;              (deadline-time-left
  ;;               (when deadline
  ;;                 (org-time-string-to-time deadline)))
  ;;              (deadline-days
  ;;               (when deadline-time-left
  ;;                 (time-to-days deadline-time-left)))
  ;;              (curr-agenda-day (or (and (bound-and-true-p org-agenda-current-date)
  ;;                                        (time-to-days
  ;;                                         (org-time-from-absolute
  ;;                                          org-agenda-current-date)))
  ;;                                   (org-today)))
  ;;              (days-to-deadline
  ;;               (when deadline-days
  ;;                 (- deadline-days curr-agenda-day))))

  ;;         (cond ((and parts-total-num days-to-deadline)
  ;;                (> parts-total-num (- days-to-deadline 1))))))))

  ;; (defvar ri/org-parts-- )

  (defun ri/org-parts--marker-validate (marker)
    (when (and (markerp marker)
               (buffer-live-p (marker-buffer marker)))
      (with-current-buffer (marker-buffer marker)
        (when (derived-mode-p 'org-mode)
          t))))

  (defun ri/org-parts--marker-get-deadline-time (marker)
    "Return days left till deadline."
    (when (ri/org-parts--marker-validate marker)
      (-some->> (org-entry-get marker "DEADLINE")
        (org-time-string-to-time))))

  (defun ri/org-parts--marker-get-parts-total (marker)
    "Return float if PARTS-TOTAL exists."
    (when (ri/org-parts--marker-validate marker)
      (-some->> (org-entry-get marker "PARTS_TOTAL")
        (string-to-number))))

  (defun ri/org-parts--days-and-parts-state (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (when-let*
          ((parts (ri/org-parts--marker-get-parts-total marker))
           (deadline (ri/org-parts--marker-get-deadline-time marker)) ;; in abs time
           (deadline-days (time-to-days deadline))
           (today org-agenda-current-date) ;; in gregorian
           (today-days (calendar-absolute-from-gregorian today))
           (days-left (- deadline-days today-days)))
        (let* ((days-lt-parts-p (>= parts (- days-left 1)))
               (days-n-lt-parts-p (not days-lt-parts-p)))
          (list days-lt-parts-p days-n-lt-parts-p)))))

  (defun ri/org-parts--days-lt-parts-predicate (item)
    (nth 0 (ri/org-parts--days-and-parts-state item)))

  (defun ri/org-parts--days-not-lt-parts-predicate (item)
    (nth 1 (ri/org-parts--days-and-parts-state item)))

  (defun ri/org-parts--yes-deadline-no-parts-predicate (item)
    (let ((marker (or (get-text-property 0 'org-marker item)
                      (get-text-property 0 'org-hd-marker item))))
      (and (ri/org-parts--marker-get-deadline-time marker)
           (not (ri/org-parts--marker-get-parts-total marker)))))

  (:option
   ;; add super agenda to org-agenda dispatcher
   ;; TODO: have a thing at the top that shows how many parts i'm behind with in total.
   org-agenda-custom-commands
   `(("a" "Super Agenda"
      ((agenda ""
               ((org-agenda-span 'day)
                (org-agenda-overriding-header "")
                (org-habit-show-habits-only-for-today nil)

                ;; Visible Structure:
                ;; [primary]
                ;; - Overdue [any <- deadline or schedule]
                ;; - Important [any <- priority]
                ;; - DUE TODAY [any <- deadline or schedule]
                ;; [secondary]
                ;; - Today's work [deadline & parts_total]
                ;; - invalid properites [deadline & != parts_total]
                ;; [rest]
                ;;
                (org-super-agenda-groups
                 '(;; Primary
                   (:name "Habit"
                          :order 100
                          :habit t)
                   (:name "Plan"
                          :order 1
                          :todo "PLAN")
                   (:name "Overdue"
                          :order 2
                          :deadline past :scheduled past)
                   (:name "DUE TODAY"
                          :order 3
                          :deadline today :scheduled today)
                   (:name "NEXT"
                          :order 4
                          :todo "NEXT")
                   (:name "Important"
                          :order 5
                          :priority>= "A")
                   ;; Secondary
                   (:name "TODAY'S WORK"
                          :order 6
                          :pred ri/org-parts--days-lt-parts-predicate)
                   (:name "future work"
                          :order 99
                          :discard (:pred ri/org-parts--days-not-lt-parts-predicate))
                   (:name "Unformatted (deadline, no parts)"
                          :order 10
                          :pred ri/org-parts--yes-deadline-no-parts-predicate)
                   (:name "DEBUG: rest"
                          :order 50
                          :anything t)
                   (:discard (:anything t))))))
       (tags "@ongoing"
             ((org-agenda-overriding-header "")
              (org-super-agenda-groups
               '((:name "Ongoing"
                        :anything t)))))))))

  )

;;;; org-ql (disabled)

(-setup org-ql :disabled
  (:load-after org))

;;;; misc functions for org-agenda

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

(defun +org-clone-subtasks (effort max-times)
  (interactive
   (list
    (read-number "Effort, on a scale of 1-10: ")
    (read-number "Max times (0 or less for unbound): ")))
  (save-excursion
    ;; get deadline and days till for the heading at point
    (org-back-to-heading)
    (when-let* ((deadline-ts
                 (or (org-entry-get nil "DEADLINE")
                     (user-error "No deadline-ts at point")))
                (days-till-deadline
                 (let ((days
                        (or (org-timestamp-to-now deadline-ts)
                            (user-error "Days till deadline = nil"))))
                   (if (<= max-times 0)
                       ;; unbounded
                       days
                     ;; boundud by max
                     (if (> days max-times)
                         (1+ max-times)
                       days)))))
      ;; set TODO if nil
      (unless (org-entry-get nil "TODO")
        (org-entry-put nil "TODO" "TODO"))

      ;; temporarily set top heading deadline to today
      ;; (org-entry-put nil "DEADLINE"
      ;;                (format-time-string (org-time-stamp-format)))

      ;; delete counting fraction if exists (avoid cloning to below)
      (save-excursion
        (org-end-of-line)
        (let ((org-special-ctrl-a/e t))
          (org-beginning-of-line))
        (when (looking-at "\\[[0-9]*/[0-9]*\\] ")
          (delete-region (point) (match-end 0))))

      ;; extra properties before cloning...
      (org-set-property "EFFORT" (format "%s" effort))

      ;; create subtree clones
      (org-do-demote)
      (org-clone-subtree-with-time-shift (1- days-till-deadline) "-1d")
      ;; note: promote top heading up at the end

      ;; sort deadline of all below
      (save-excursion
        (org-forward-heading-same-level 1)
        (set-mark (line-beginning-position))
        (let ((last-point (point))
              (times 0))
          (while (and (org-forward-heading-same-level 1)
                      (> (point) last-point)
                      (setq last-point (point))
                      (setq times (1+ times))))
          (setq mark-active t)
          (org-sort-entries nil ?d)))

      ;; insert counting fraction at top heading
      (save-excursion
        (org-end-of-line)
        (let ((org-special-ctrl-a/e t))
          (org-beginning-of-line))
        (unless (looking-at "\\[[0-9]*/[0-9]*\\] ")
          (insert "[/] ")))

      ;; ;; revert top heading deadline to deadline-ts
      ;; (org-entry-put nil "DEADLINE" deadline-ts)

      ;; insert fraction in each subheading
      (save-excursion
        (let ((last-point (point))
              (times 0))
          (while (and (org-forward-heading-same-level 1)
                      (> (point) last-point)
                      (setq last-point (point))
                      (setq times (1+ times)))
            (org-end-of-line)
            (let ((org-special-ctrl-a/e t))
              (org-beginning-of-line))
            (insert (format "%s/%s " times (1- days-till-deadline))))))

      ;; promote top heading at top heading
      (org-do-promote)

      ;; visibility folded
      (org-set-property "VISIBILITY" "folded")

      ;; update counting fraction
      (call-interactively #'org-update-statistics-cookies)
      )))

;;; Workflow

;;;; org-noter

(-setup org-noter
  (:load-after org)
  (:global "C-c o n" #'org-noter
           "C-c d n" #'org-noter-start-from-dired
           "C-c o p" #'+org-noter-set-prop-current-page)
  (:option org-noter-doc-split-fraction '(0.6 . 0.6))
  (:when-loaded
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
           (org-entry-put (point) "NOTER_PAGE" num)))))))

;;;; org-capture (TODO)

(setup org-capture
  (:load-after org)
  (leader-bind
    "oc" 'org-capture)

  ;; helper for `org-capture-templates', below
  (defun +get-org-agenda-denote-file (name)
    (let ((regex (format "^.*--%s__.*\\.org$" name)))
      (car (seq-filter
            (lambda (path)
              (string-match regex (file-name-nondirectory path)))
            org-agenda-files))))

  (:option org-capture-templates
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

;;;; org-download

(-setup org-download
  (:option org-download-image-dir "_images")
  (:load-after org)
  (:when-loaded
    (org-download-enable)))

;;; Prettify

;; stuff to make org prettier.

;;;; visual fill column

(-setup visual-fill-column
  (with-eval-after-load 'org
    (add-hook 'org-mode-hook
              (defun +org-visual-fill ()
                (setq visual-fill-column-width 100
                      visual-fill-column-center-text t)
                (visual-fill-column-mode 1)))))

;;;; org-bullets

;; TODO: replace with org-superstar
(-setup org-bullets
  (:hook-into org-mode-hook)
  (:option org-bullets-bullet-list
           '("◉"
             "●"
             "○"
             "■"
             "□"
             "✦"
             "✧"
             "✿")))

;;;; org-modern (disabled)

(-setup org-modern :disabled
  (:option org-modern-star nil)
  (global-org-modern-mode 1))

;;; Misc

;;;; pomodoro

(-setup org-pomodoro
  (:load-after org))

;;; end
(provide '+org)

;;; +org.el ends here
