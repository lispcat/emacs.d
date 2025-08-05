;;; my-org.el --- org-mode

;;; Commentary:

;;; Code:


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; NOTE: ensure that the newest version of org is installed right after elpaca setup
(leaf org :ensure nil
  :setq
  (org-directory . "~/Notes/org")
  (org-tags-column . -55)          ; column where tags are indented to
  (org-startup-folded . 'showall)  ; default folding mode
  (org-startup-indented . t)       ; indent headings and its body
  (org-special-ctrl-a/e . t)
  (org-src-window-setup . 'current-window) ; edit code blocks in the same window
  (org-return-follows-link . t)            ; RET can open links
  (org-hide-emphasis-markers . t) ; hide formatting chars (* / ~ = etc)
  (org-src-preserve-indentation . t) ; remove annoying leading whitespace in code blocks
  (org-fontify-whole-heading-line . t)

  :init
  (general-my-map
    "o" '(:ignore t :which-key "org"))

  :hook (org-mode-hook . indent-tabs-mode)

  :config
  (defun my/org-insert-subheading-respect-content ()
    "Insert new subheading after the current heading's body.
If in a list, inserts a new sublist after the current list."
    (interactive)
    (org-meta-return)
    (org-metaright))

  :bind (org-mode-map
         ("C-M-<return>"
          . my/org-insert-subheading-respect-content))

  :defer-config

  ;; set org font sizes
  (dolist (pair '((org-document-title :height 1.9 :weight bold)
                  (org-level-1 :height 1.7 :weight bold)
                  (org-level-2 :height 1.4 :weight bold)
                  (org-level-2 :height 1.1)
                  (org-level-3 :height 1.1)))
    (apply #'set-face-attribute (car pair) nil (cdr pair)))

  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))
  (add-to-list 'org-structure-template-alist '("gcc" . "src c"))
  (add-to-list 'org-structure-template-alist '("scm" . "src scheme"))
  (add-to-list 'org-structure-template-alist '("conf" . "src conf"))
  (add-to-list 'org-structure-template-alist '("java" . "src java"))
  (add-to-list 'org-structure-template-alist '("unix" . "src conf-unix"))
  (add-to-list 'org-structure-template-alist '("clang" . "src c"))

  )

(leaf org-download
  :after org
  :config
  (org-download-enable)
  :setq-default
  (org-download-image-dir . "_images"))

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

(leaf toc-org
  :hook org-mode-hook)

(leaf anki-editor
  :commands (anki-editor-push-note-at-point
             anki-editor-push-notes
             anki-editor-push-new-notes)
  :setq
  (anki-editor-latex-style . 'mathjax)
  :defer-config
  (defun my/ensure-anki-editor-mode (note)
    "Ensure `anki-editor-mode' is enabled before pushing notes."
    (unless anki-editor-mode
      (anki-editor-mode 1)))
  (advice-add #'anki-editor--push-note :before #'my/ensure-anki-editor-mode))

(use-package f :ensure (:wait f))
(leaf image-slicing :ensure nil
  :hook org-mode-hook
  :setq
  (image-slicing-newline-trailing-text . nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; agenda ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf org-agenda :ensure nil
  :after org
  :init
  (general-my-map
    "oa" 'org-agenda)

  :bind (org-agenda-mode-map
         (")" . 'org-agenda-todo))

  :config
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)"
                    "|"
                    "DONE(d/!)")))
  (setq org-agenda-files
        (list "~/Notes/org/Inbox.org"
              "~/Notes/org/agenda.org"))
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
                      "[%3(my/org-get-prop-effort)]    "
                      ;; "%3(my/org-get-prop-effort)  "
                      "% s"))
          (todo   . " %i ")
          (tags   . " %i %-12:c")
          ;; (search . " %i %-12:c")
          (search . " %c")
          ))

  (defun my/org-get-prop-effort ()
    (if (not (eq major-mode 'org-mode)) ""
      (let ((val (org-entry-get nil "EFFORT")))
        (if (not val) ""
          (format "%s" (string-trim val))))))

  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t))

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
                  ;; (org-agenda-start-on-weekday nil)
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

(leaf org-ql
  :after org)

(leaf org-pomodoro
  :after org)

(leaf org-noter
  :after org
  :bind (("C-c o n" . org-noter)
         ("C-c d n" . org-noter-start-from-dired))
  :setq
  (org-noter-doc-split-fraction . '(0.7 . 0.6)))

;;;;;; Org Capture ;;;;;;

(leaf org-capture :ensure nil
  :after org
  :init
  (general-my-map
    "oc" 'org-capture)

  :config
  (defun my/get-org-agenda-denote-file (name)
    (let ((regex (format "^.*--%s__.*\\.org$" name)))
      (car (seq-filter
            (lambda (path)
              (string-match regex (file-name-nondirectory path)))
            org-agenda-files))))

  (setq org-capture-templates
        `(("t" "Tasks")

          ("td" "Todo with deadline" entry
           (file ,(my/get-org-agenda-denote-file "agenda"))
           "* TODO %^{Task}\nDEADLINE: %^{Deadline}t\n%?\n"
           :empty-lines 1
           :immediate-finish nil)

          ("tp" "Task" entry
           (file ,(my/get-org-agenda-denote-file "agenda"))
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)

          ("n" "New note (with Denote)" plain
           (file denote-last-path)
           #'denote-org-capture :no-save t :immediate-finish nil
           :kill-buffer t :jump-to-captured t))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;; org-latex ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: implement one-time load after cdlatex loads, but before cdlatex is enabled
(leaf auctex
  :require t)

(leaf cdlatex
  :after auctex
  :hook (org-mode-hook . turn-on-org-cdlatex)
  :setq
  (org-preview-latex-default-process . 'dvisvgm)
  (org-latex-create-formula-image-program . 'dvisvgm)
  (org-latex-preview-ltxpng-directory . "_ltximg/")
  :config
  (defun org-try-cdlatex-tab ()
    "Check if it makes sense to execute `cdlatex-tab', and do it if yes.
It makes sense to do so if `org-cdlatex-mode' is active and if the cursor is
  - inside a LaTeX fragment, or
  - after the first word in a line, where an abbreviation expansion could
    insert a LaTeX environment."
    (when org-cdlatex-mode
      (cond
       ;; Before any word on the line: No expansion possible.
       ;; ((save-excursion (skip-chars-backward " \t") (bolp)) nil)
       ;; Just after first word on the line: Expand it.  Make sure it
       ;; cannot happen on headlines, though.
       ;; ((save-excursion
       ;;    (skip-chars-backward "a-zA-Z0-9*")
       ;;    (skip-chars-backward " \t")
       ;;    (and (bolp) (not (org-at-heading-p))))
       ;;  (cdlatex-tab) t)
       ((org-inside-LaTeX-fragment-p) (cdlatex-tab) t))))
  :init
  (defun my/org-latex-preview-buffer ()
    (interactive)
    (if (not (derived-mode-p 'org-mode))
        (message "Not in org-mode.")
      (org-latex-preview '(16))))
  (general-my-map
    "ol" 'my/org-latex-preview-buffer))

(leaf org-fragtog
  :hook org-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-org)
;;; my-org.el ends here
