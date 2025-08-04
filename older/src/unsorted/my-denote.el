
;; Sample config:
;; https://protesilaos.com/emacs/denote#h:5d16932d-4f7b-493d-8e6a-e5c396b15fd6

;; TODO: to look into!!
;; - https://baty.blog/2022/keeping-my-org-agenda-updated
;; - https://forum.systemcrafters.net/t/bring-denote-into-org-agenda-with-prettyness/779
;; https://www.reddit.com/r/emacs/comments/1er9wj4/denote_and_agenda_practical_use/

;; Note:
;; - dired: "% m" then "t" then `k' to kill and filter down results

(leaf denote
  :init
  (general-my-map
    "n" '(:ignore t :which-key "denote")
    "nn" 'denote
    "ns" 'denote-subdirectory
    ;; "nf" 'denote-open-or-create ;; moved to consult-notes

    ;; renaming
    "nr" '(:ignore t :which-key "rename file")
    "nrf" '(denote-rename-file :which-key "rename file")
    "nrt" '(denote-rename-file-title :which-key "rename title")
    "nrk" '(denote-rename-file-keywords :which-key "rename keywords")

    ;; dired
    "nd" '(:ignore t :which-key "dired")
    "ndj" '(my/denote-directory-jump :which-key "jump to denote dir")
    "ndr" '(denote-dired-rename-marked-files :which-key "marked rename")
    "ndk" '(denote-dired-rename-marked-files-add-keywords
            :which-key "marked add keywords")
    "ndK" '(denote-dired-rename-marked-files-remove-keywords
            :which-key "marked remove keywords")

    ;; links
    "nl" '(:ignore t :which-key "links")
    "nll" '(denote-find-link :which-key "find links in file")
    "nln" '(denote-link :which-key "new link")
    "nla" '(denote-add-links :which-key "add links for metanote")

    ;; backlinks
    "nb" '(:ignore t :which-key "backlinks")
    "nbb" '(denote-find-backlink :which-key "find backlinks")
    "nbl" '(denote-backlinks :which-key "list backlinks")

    ;; org-dblocks
    "no" '(:ignore t :which-key "org-dblocks")
    "nol" '(denote-org-extras-dblock-insert-links :which-key "dblock links")
    "nof" '(denote-org-extras-dblock-insert-files :which-key "dblock files")
    "nob" '(denote-org-extras-dblock-insert-backlinks :which-key "dblock backlinks")
    "noa" '(my/denote-insert-file-local-dblock-update-mode :which-key "insert file-local dblock mode")

    ;; journal
    "oj" '(denote-journal-extras-new-or-existing-entry :which-key "denote-journal")
    "nj" '(:ignore t :which-key "journal")
    "njj" '(denote-journal-extras-new-or-existing-entry :which-key "open today")
    "njo" '(denote-journal-extras-new-or-existing-entry :which-key "open today")
    "njn" '(denote-journal-extras-new-entry :which-key "new entry")
    "njl" '(denote-journal-extras-link-or-create-entry :which-key "link entry"))

  :config

  ;; variables
  (setq denote-directory (expand-file-name "~/Notes/denote"))
  (setq denote-known-keywords '("emacs" "meta"
                                "art" "hobbies" "ideas"
                                "class" "todo"
                                "calc1" "arthist"
                                "systemsoftware" "bio2"
                                "random"))
  (setq denote-prompts '(title keywords subdirectory))
  (setq denote-save-buffers t)
  (setq denote-excluded-directories-regexp
        (concat
         ;; (^|/) ... (/|$)
         "\\(^\\|/\\)" "[aA]rchived?" "\\(/\\|$\\)" "\\|"
         "\\(^\\|/\\)" "[eE]xcluded?" "\\(/\\|$\\)" "\\|"
         "\\(^\\|/\\)" "_.*"          "\\(/\\|$\\)"))

  ;; when renaming, don't prompt for modify-file-name
  (setq denote-rename-confirmations '(rewrite-front-matter))

  ;; prettify

  ;; rename buffer/mode-line
  (setq denote-rename-buffer-format "[D] %t%b  _%k")
  (denote-rename-buffer-mode 1)

  ;; dired fontify
  (add-hook 'dired-mode-hook #'denote-dired-mode)

  ;; links in text files
  (add-hook 'text-mode-hook #'denote-fontify-links-mode-maybe)

  ;; other

  (defun my/denote-directory-jump ()
    (interactive)
    (dired denote-directory))

  ;; org-capture

  (with-eval-after-load 'org-capture
    (add-to-list 'org-capture-templates
                 '("n" "New note (with Denote)" plain
                   (file denote-last-path)
                   #'denote-org-capture
                   :no-save t
                   :immediate-finish nil
                   :kill-buffer t
                   :jump-to-captured t)))

  ;; org-dblocks

  ;; (define-minor-mode org-dblock-update-minor-mode
  ;;   "A minor mode that automatically updates Org mode dynamic blocks before saving."
  ;;   :lighter " OrgDBlocks"
  ;;   :global nil
  ;;   (if (and org-dblock-update-minor-mode (eq major-mode 'org-mode))
  ;;    (add-hook 'before-save-hook #'org-update-all-dblocks nil t)
  ;;     (remove-hook 'before-save-hook #'org-update-all-dblocks t)))

  ;; (defun my/denote-insert-file-local-dblock-update-mode ()
  ;;   (interactive)
  ;;   (if (eq major-mode 'org-mode)
  ;;    (add-file-local-variable
  ;;     'eval
  ;;     '(org-dblock-update-minor-mode))
  ;;     (message "Not in an org-mode buffer")))

  ;; journal

  (require 'denote-journal-extras)
  (setq denote-journal-extras-directory
        (expand-file-name "journal" denote-directory)))


;; provides consult sources:
;; - "SPC D" for denote buffers
;; - "SPC S" for denote subdirectories
(leaf consult-denote
  :after consult-notes
  :config
  (consult-denote-mode 1))
;; TODO: write my own consult function for "SPC S".


;; Docs: https://github.com/mclear-tools/consult-notes
(leaf consult-notes
  :commands consult-notes consult-notes-search-in-all-notes
  :after org
  :bind ("M-s n" . consult-notes)
  :init
  (general-my-map
    "nf" 'consult-notes
    "ng" 'consult-notes-search-in-all-notes)
  :config
  ;; denote keywords "_" fix
  (progn
    (setq consult-notes-denote-display-keywords-indicator "_")
    (defun consult-notes-denote--display-keywords (keywords)
      (format "%18s" (if keywords
                         (concat
                          consult-notes-denote-display-keywords-indicator
                          (mapconcat 'identity keywords "_"))
                       ""))))
  ;; custom printing format
  (progn
    (defun my/consult-notes--file-dir-annotate (name dir cand)
      "Annotate file CAND with its directory DIR, size, and modification time."
      (let* ((file  (concat (file-name-as-directory dir) cand))
             (dirs  (abbreviate-file-name dir))
             (attrs (file-attributes file))
             (fsize (file-size-human-readable (file-attribute-size attrs)))
             (ftime (consult-notes--time (file-attribute-modification-time attrs))))
        (message "DEBUGGGGG: %s %s %s" file name dirs)
        (put-text-property 0 (length name)  'face 'consult-notes-name name)
        (put-text-property 0 (length dirs)  'face 'consult-notes-name dirs)
        (put-text-property 0 (length fsize) 'face 'consult-notes-size fsize)
        (put-text-property 0 (length ftime) 'face 'consult-notes-time ftime)
        (format "%7s %8s  %12s  %8s" name fsize ftime dirs)))
    (setq consult-notes-file-dir-annotate-function #'my/consult-notes--file-dir-annotate))
  ;; enable for denote after load denote
  (with-eval-after-load 'denote
    (consult-notes-denote-mode 1)))

;; docs: https://lucidmanager.org/productivity/denote-explore/
(leaf denote-explore
  :after denote
  :init
  (general-my-map
    "ne" '(:ignore t :which-key "explore")

    ;; random walks
    "new" '(:ignore t :which-key "random walks")
    "newl" '(denote-explore-random-link :which-key "random link")
    "newr" '(denote-explore-random-regex :which-key "random regex")
    "newk" '(denote-explore-random-keyword :which-key "random keyword")

    ;; janitor
    "nej" '(:ignore t :which-key "janitor")
    "nejj" '(denote-explore-sync-metadata :which-key "sync filenames from metadata")
    "nejm" '(denote-explore-sync-metadata :which-key "sync filenames from metadata")
    "nejs" '(denote-explore-sort-keywords :which-key "sort order of all keywords")
    "nejr" '(denote-explore-rename-keyword :which-key "rename keyword")
    "nej0" '(denote-explore-zero-keywords :which-key "0 keywords")
    "nej1" '(denote-explore-single-keywords :which-key "1 keywords")

    ;; visualize
    "nen" '(:ignore t :which-key "network")
    "nenn" '(denote-explore-network :which-key "network")
    "nenr" '(denote-explore-network-regenerate :which-key "network regenerate")
    "nend" '(denote-explore-degree-barchart :which-key "degree barchart")

    ;; stats
    "nes" '(:ignore t :which-key "stats")
    "nesk" '(denote-explore-barchart-keywords :which-key "barchart keywords")
    "nese" '(denote-explore-barchart-filetypes :which-key "barchart filetypes"))

  ;; :config
  ;; (setq denote-explore-network-format )
  ;; TODO: make denote-explore-network / browse-url-browser-function

  )

;; denote-menu
(leaf denote-menu
  :after denote
  :init
  (general-my-map
    "nm" 'list-denotes)
  :bind (denote-menu-mode-map
         ("c" . denote-menu-clear-filters)
         ("r" . denote-menu-filter)
         ("k" . denote-menu-filter-by-keyword)
         ("o" . denote-menu-filter-out-keyword)
         ("/ r" . denote-menu-filter)
         ("/ k" . denote-menu-filter-by-keyword)
         ("/ o" . denote-menu-filter-out-keyword)
         ("e" . denote-menu-export-to-dired))
  :config
  (setq denote-menu-title-column-width 50))

;;; set common keys
;; (general-my-map
;;   "nN" '(:ignore t :which-key "Favorites")
;;   "nNn" 'denote
;;   "")

(provide 'my-denote)
