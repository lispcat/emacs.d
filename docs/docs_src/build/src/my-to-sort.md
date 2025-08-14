# Code

```emacs-lisp
  :init (omni-quotes-mode 1)
  :bind (("M-s q m" . omni-quotes-mode)
         ("M-s q p" . omni-quotes-prev-set)
         ("M-s q n" . omni-quotes-next-set)
         ("M-s q s" . omni-quotes-shuffle-current-set)
         ("M-s q q" . omni-quotes-display-random-quote))
  :setq
  (omni-quotes-idle-interval . 60)
  (omni-quotes-fading . t)
  (omni-quotes-fading-delay . 30)
  :config
  (omni-quotes-load-simple-quote-file "~/Notes/org/quotes.txt" "personal"))

;; (defvar eldoc-doc-buffer-separator
;;   (concat (propertize "\n" 'face '(:inherit separator-line :extend t)))
;;   "String used to separate items in Eldoc documentation buffer.")


;;; Packages to install (check source code for malware for each first):

;; bind-map

;; restart-emacs

;; golden-ratio

;; ws-butler

;; flycheck-package

;; org-present

;; emacs-purpose

;; origami

;; cask

;; tao-theme

;; emacs-color-themes

;; hydra !!! so good
;; https://github.com/abo-abo/hydra/wiki/Hydras-by-Topic

;; literate programming?

;; hl-todo

;; breadcrumb?

;; git-gutter?

;; hideshow-org?

;; wgrep

;; look into different window strengths, lower C-h e 's strength

;; undo-tree

;; hyperbole

;; tree sitter

;; eglot?

(defhydra hydra-org (:color red :columns 3)
  "Org Mode Movements"
  ("n" (lambda ()
         (interactive)
         (mapc #'call-interactively '(org-fold-hide-entry
                                      outline-next-visible-heading
                                      org-fold-show-entry
                                      ))
         (recenter-top-bottom)
         (recenter-top-bottom))
   "next heading")
  ("p" (lambda ()
         (interactive)
         (mapc #'call-interactively '(org-fold-hide-entry
                                      outline-previous-visible-heading
                                      org-fold-show-entry
                                      ))
         (recenter-top-bottom)
         (recenter-top-bottom)
         (recenter-top-bottom))
   "prev heading")
  ("N" (lambda ()
         (interactive)
         (mapc #'call-interactively '(org-fold-hide-entry
                                      org-forward-heading-same-level
                                      org-fold-show-entry))
         (recenter-top-bottom)
         (recenter-top-bottom))
   "next heading at same level")
  ("P" (lambda ()
         (interactive)
         (mapc #'call-interactively '(org-fold-hide-entry
                                      org-backward-heading-same-level
                                      org-fold-show-entry))
         (recenter-top-bottom)
         (recenter-top-bottom)
         (recenter-top-bottom))
   "prev heading at same level")
  ("u" outline-up-heading "up heading")
  ("g" org-goto "goto" :exit t))

;; https://www.reddit.com/r/emacs/comments/ioenk2/ical_import_in_emacs_calendar/

(require 'diary-lib)

(setq +calendars
      (with-temp-buffer
        (insert-file-contents "~/Private/elisp/calendar-urls.el")
        (read (current-buffer))))

(defun +ical-pull-all ()
  (interactive)
  (find-file diary-file)
  (erase-buffer)
  (message "Cleared diary file")
  (mapcar (lambda (url)
            (let ((tmpfile (url-file-local-copy url)))
              (message "Importing ")
              (icalendar-import-file tmpfile diary-file)
              (kill-buffer (car (last (split-string tmpfile "/"))))))
          +calendars))

;; no cover
;; (use-package listen)

;; emms extract metadata?
;; https://www.reddit.com/r/emacs/comments/981khz/emacs_music_player_with_emms/

;; TODO:
(leaf emms
  :config
  (emms-all)

  (setq emms-player-list '(
                           emms-player-mpd
                           emms-player-mpv
                           ))

  ;; (require 'emms-player-mpv) ; disabled for mpd

  ;; variables

  (setq emms-source-file-default-directory "~/Music/library/")
  (setq emms-player-mpd-music-directory "~/Music/library/")

  ;; emms-player-mpv-parameters '("--no-audio-display=no"); broken
  (setq emms-browser-covers #'emms-browser-cache-thumbnail-async)
  ;; sort by natural order
  (setq emms-playlist-sort-function #'emms-playlist-sort-by-natural-order)
  ;; make streams update metadata
  (setq emms-player-mpv-update-metadata t)
  ;; playlist format use m3u
  (setq emms-source-playlist-default-format 'm3u)
  ;; show format
  (setq emms-show-format "NP: %s")
  ;; ;; sort album by natural order
  ;; ;; (setq  emms-browser-album-sort-function #'emms-playlist-sort-by-natural-order)
  ;; this actually sorts by natural order upon adding
  (add-hook 'emms-playlist-source-inserted-hook
            #'emms-playlist-sort-by-natural-order)

  ;; backends


  ;; get info from mpd
  ;; (add-to-list 'emms-info-functions 'emms-info-mpd)
  ;; ? show current song when next song starts?
  ;; (add-hook 'emms-player-started-hook #'emms-show)
  ;; connect to mpd
  ;; (setq emms-player-mpd-server-name "localhost")
  ;; (setq emms-player-mpd-server-port "6600")
  ;; (setq emms-player-mpd-music-directory "\~/Music/library")
  ;; (emms-player-mpd-connect)

  ;; persistent playlists
  ;; (require 'emms-history)
  (emms-history-load)

  ;; display
  (emms-mode-line-mode 0)

  ;; enable playerctl pausing

  ;; DISABLE LATER when using mpd-mpris service
  ;; (require 'emms-mpris)
  ;; (emms-mpris-enable) ;; (will make emacs hog mpris media playing active)

  ;; (setq emms-player-list '(emms-player-mpd))
  (add-to-list 'emms-info-functions 'emms-info-mpd)
  (add-to-list 'emms-player-list 'emms-player-mpd)

  ;; browser

  ;; TODO: add this function to emms-info-functions (hard to implement?)
  ;; (instead make my own function that runs ffprobe and gets info? might be better)
  (defun +emms-show-album-cover-in-emacs ()
    (interactive)
    (if-let ((track (emms-playlist-current-selected-track))
             (song-path (emms-track-get track 'name))
             (cover-path "/tmp/emms-album-cover.jpg")) ;; is jpg fine?
        (if (not (file-exists-p song-path))
            (message "Error: cannot find path to currently playing song")
          (when (file-exists-p cover-path)
            (delete-file cover-path))
          (let ((exit-code
                 (shell-command
                  (message "extracting: %s"
                           (format "ffmpeg -i %s -an -vcodec copy %s -y"
                                   (shell-quote-argument song-path)
                                   (shell-quote-argument cover-path))))))
            (cond ((/= exit-code 0)
                   (message "Error: ffmpeg cover extraction failed with code %s"
                            exit-code))
                  ((file-exists-p cover-path)
                   (with-current-buffer (get-buffer-create "*Album Cover*")
                     (erase-buffer)
                     (insert-image (create-image cover-path))
                     (pop-to-buffer (current-buffer))))
                  (t
                   (message "Error: ffmpeg cover at cover-path not found.")))))
      (message "No song currently playing")))

  ;; Hook to display album cover in Emacs when the track changes
  ;; (add-hook 'emms-player-started-hook 'emms-show-album-cover-in-emacs)

  ;;;; Personal functions for features: ;;;;

  ;; Edit a playlist:
  ;; - steps:
  ;;   - create a new playlist buffer named "%s_real".
  ;;   - make modifications as needed.
  ;;   - command to write and delete buffer.
  ;; - create a function to reload the playlist from file, as well as write.
  ;; - indicator to show whether the playlist has been modified or not?
  ;;
  ;; Add a song to a playlist:
  ;; - steps:
  ;;   - create a new playlist buffer named "%s_real".
  ;;   - place the song at the bottom of the playlist.
  ;;   - make modifications as needed.
  ;;   - command to write and delete buffer.
  ;;
  ;;
  ;; workflow:
  ;; - idea: if playing a playlist (any) and i want to add a song from it to a specific playlist (regardless of if it's loaded or not), the process is to load the playlist from the file in a new buffer, make the change, save, then close. And for convenience, if that playlist i added the song in is loaded and i wanna see those changes be updated, run a function to reload the playlist from its source file.
  ;;
  ;; Ideas:
  ;; - playlist editing mode?
  ;; - edit one playlist at a time? bc need to preserve the source playlist file somewhere
  ;; - does a playlist file regenerate from the file when opened?
  ;; - command: `emms-playlist-editor-open-playlist'
  ;;   - emms-metaplaylist-mode-new-buffer (to create new buffer with buffer-name
  ;;   -
  ;;
  ;;
  ;; Implement:
  ;; - function: add a playlist file to a new playlist buffer ("%s_EDITING")

  ;;   (require 'cl-lib)

  ;;   (defvar emms-playlist-editor--buffer-name "EDITING"
  ;;     "The buffer name for editing.")

  ;;   (defvar emms-playlist-editor--current-path nil
  ;;     "The filepath to the current \"EDITING\" file.
  ;; Used in `emms-playlist-edit-open-playlist'.")

  ;;   (defun emms-playlist-editor-open-playlist ()
  ;;     (interactive)
  ;;     (let* ((buffer-name emms-playlist-editor--buffer-name)
  ;;         (buffer-real (get-buffer buffer-name)))
  ;;       ;; handle case if buffer already exists
  ;;       (when buffer-real
  ;;      (switch-to-buffer buffer-real)
  ;;      (if (yes-or-no-p (format "Buffer \"%s\" already exists. Delete and contiune?"
  ;;                               buffer-name))
  ;;          (kill-buffer buffer-name) ;; and continue...
  ;;        (message "aborting...")
  ;;        (return)))
  ;;       (let ((buf (get-buffer-create buffer-name)))
  ;;      ;; init new "EDITING" buffer as playlist buffer
  ;;      (with-current-buffer buf
  ;;        (emms-playlist-mode)
  ;;        (setq emms-playlist-buffer-p t))
  ;;      ;; update metaplaylist
  ;;      (emms-metaplaylist-mode-go)
  ;;      (emms-metaplaylist-mode-update)
  ;;      ;; go to new buffer
  ;;      (switch-to-buffer
  ;;       (emms-playlist-set-playlist-buffer buf))
  ;;      ;; select playlist file
  ;;      (let ((file (read-file-name "Playlist file: "
  ;;                                  emms-source-file-default-directory
  ;;                                  emms-source-file-default-directory
  ;;                                  t)))
  ;;        ;; add files
  ;;        (emms-add-playlist file)
  ;;        (setq emms-playlist-editor--current-path file)
  ;;        ))))

  ;;   (defun emms-playlist-editor-save-playlist ()
  ;;     (interactive)
  ;;     (let* ((buffer-name emms-playlist-editor--buffer-name)
  ;;         (buffer-real (get-buffer buffer-name))
  ;;         (path emms-playlist-editor--current-path))
  ;;       (if (not buffer-real)
  ;;        (message "Buffer \"%s\" doesn't exist, exiting..." buffer-name)
  ;;      (switch-to-buffer
  ;;       (emms-playlist-set-playlist-buffer buffer-real))
  ;;      ;; save to file
  ;;      (let ((format
  ;;             (emms-source-playlist-read-format)))
  ;;        (emms-playlist-save format path))


  ;;      )))

  ;; PLAYLISTS buffer, where i keep playlist files, autoload all

;;;;;;;;; YKW, fuck it, im just gonna tag everything in info-note (WORKS!)
  ;; filter by note with emms-playlist-limit-to-info-note
  ;; e.g. :nice:hardcore:

  ;; (emms-browser-add-category "note" 'info-note)
  (defun emms-browser-search-by-note ()
    (interactive)
    (emms-browser-search '(info-note)))

  ;;; As for playlists, i'll still be making it for, well, when i wanna make playlists,
  ;; but i wont need to rely on those special custom functions. i can suffice with just:
  ;; - `emms-add-playlist-file' (add playlist file) [maybe i should automate creating a PLAYLISTS buffer]
  ;; - `emms-playlist-mode-load-playlist' (expand playlist file in new playlist buffer)
  ;; - C-x C-s or `emms-playlist-save' (save playlist to file)
  ;; - `rename-buffer' (rename buffer to liking)
  ;;
  ;; TODO: bind the above to keybinds


  ;;; Holy shit writing my emacs config modules in a declarative org file is actually pretty realistic and doable!?!
  ;; It'll make everything so much nicer... documentation as well...

  (defvar emms-playlistedit-orig-path nil
    "A local var for playlist buffers with the path to its playlist file.")

  ;; emms-playlistedit-open : given a path to the playlist file, adds the playlist file to the "PLAYLISTS" buffer, load playlist in a new generic playlist buffer, with a buffer-local variable for orig path set (or maybe the playlist file?),
  ;; - simplify by adding the playlist file to a "PLAYLISTS" buffer, then loading it from there?
  (defun emms-playlistedit-playlist-file-edit ()
    "Given a loaded playlist file at point, load in a new playlist buffer for editing.
It's essentially the same as `emms-playlist-mode-load-playlist' but it also sets
a buffer-local variable `emms-playlistedit-orig-path'."
    (interactive)
    ;; load the playlist at point
    ;; (below is a copy of `emms-playlist-mode-load-playlist' (we want to use the `name' variable later)).
    (let* ((track (emms-playlist-track-at))
           (name (emms-track-get track 'name)))
      (emms-playlist-select (point))
      (run-hooks 'emms-player-stopped-hook)
      (switch-to-buffer
       (emms-playlist-set-playlist-buffer (emms-playlist-new)))
      (emms-add-playlist name)
      ;; let the buffer-local variable to be `name' and also rename.
      (let ((buf emms-playlist-buffer))
        (with-current-buffer buf
          (setq-local emms-playlistedit-orig-path name)
          (rename-buffer (concat (buffer-name)
                                 " : "
                                 name))))))

  ;; (defun emms-playlistedit-create-playlist-buffer (buffer-name)
  ;;     "Creates a new playlist buffer BUFFER-NAME.
  ;; Basically the same as `emms-metaplaylist-mode-new-buffer' but without switching
  ;; to the metaplaylist view."
  ;;     (interactive "sBuffer Name: ")
  ;;     (if (get-buffer buffer-name)
  ;;      (error "Buffer must not exist.")
  ;;       (let ((buf (get-buffer-create buffer-name)))
  ;;      (with-current-buffer buf
  ;;        (emms-playlist-mode)
  ;;        (setq emms-playlist-buffer-p t)))
  ;;       (emms-metaplaylist-mode-go)
  ;;       (emms-metaplaylist-mode-update)))

  ;; (defun emms-playlistedit-open-playlist-file ()
  ;;     "Creates a new playlist buffer from a playlist-file, saving the original path.
  ;; The original path is saved in a buffer-local variable."
  ;;     )

  ;; emms-playlistedit-goto-playlist-buffer : goes to the "PLAYLISTS" buffer. If not exist, create new then go to.

  ;;


  ;; - load playlist contents in a new playlist buffer
  ;;   - use a buffer-local variable for the origin path
  ;; - make changes
  ;; - emms-playlist-diff-and-save
  ;;   - if the buffer-local variable is nil, then just do emms-playlist-save as usual
  ;;   - if the buffer-local variable is set, then:
  ;;     - load the original playlist in "TMP-%s", and diff compare new and old playlists. (error if path to playlist invalid).
  ;;     - proceed?
  ;;       - if yes, overwrite playlist file with new changes, then delete "TMP-%s".
  ;;       - if no, delete "TMP-%s" and cancel.



  ;; maybe switch to mpv (mpd is too jank) (usempvScripts.mpris)


  :init
  (general-my-map
    "e" '(:ignore t :which-key "emms")
    "e e" 'emms
    "e k" 'emms-playlist-current-kill

    ;; goto
    "e p" 'emms-playlist-mode-go
    "e m" 'emms-metaplaylist-mode-go

    ;; browse
    "e B" 'emms-smart-browse
    "e b" '(:ignore t :which-key "browse")
    "e b b" 'emms-browser
    "e b a" 'emms-browse-by-album
    "e b A" 'emms-browse-by-artist

    ;; control
    "e c" '(:ignore t :which-key "control")
    "e c P" 'emms-pause
    "e c n" 'emms-next
    "e c p" 'emms-previous
    "e c s" 'emms-seek-to

    ;; info
    "e i" '(:ignore t :which-key "info")
    "e i i" 'emms-show
    "e i a" 'emms-show-all
    "e i m" 'emms-player-mpd-show

    ;; sort
    "e S" '(:ignore t :which-key "sort")
    "e S n" 'emms-playlist-sort-by-natural-order
    "e S r" 'emms-playlist-sort-by-random
    "e S o" 'emms-playlist-sort-by-info-note))

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
    "ndj" '(+denote-directory-jump :which-key "jump to denote dir")
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
    "noa" '(+denote-insert-file-local-dblock-update-mode :which-key "insert file-local dblock mode")
    )

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

  (defun +denote-directory-jump ()
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

  ;; (defun +denote-insert-file-local-dblock-update-mode ()
  ;;   (interactive)
  ;;   (if (eq major-mode 'org-mode)
  ;;    (add-file-local-variable
  ;;     'eval
  ;;     '(org-dblock-update-minor-mode))
  ;;     (message "Not in an org-mode buffer")))

  ;; journal

  ;; (require 'denote-journal-extras)
  ;; (setq denote-journal-extras-directory
  ;;       (expand-file-name "journal" denote-directory))
  )

(leaf denote-journal
  :config
  (general-my-map
    ;; journal

    "nj" '(:ignore t :which-key "journal")
    "njN" 'denote-journal-new-entry
    "njc" 'denote-journal-link-or-create-entry
    "njn" 'denote-journal-new-or-existing-entry))

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
    (defun +consult-notes--file-dir-annotate (name dir cand)
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
    (setq consult-notes-file-dir-annotate-function #'+consult-notes--file-dir-annotate))
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

(provide 'my-to-sort)
```



---

*Last updated: { git_revision_date_localized }*
