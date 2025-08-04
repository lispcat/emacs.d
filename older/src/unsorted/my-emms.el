

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
                           ;; emms-player-mpv
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
  (defun my/emms-show-album-cover-in-emacs ()
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


(provide 'my-emms)
