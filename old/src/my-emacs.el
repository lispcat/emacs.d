;;; my-emacs.el --- adding onto built-in emacs stuff

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; general ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Actuates Meta key by default
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Shorten yes/no prompts to y/n
(defalias 'yes-or-no-p 'y-or-n-p)

;; By default, Emacs thinks two spaces after a period is a sentence.
;; This changes that to just one space.
(setq sentence-end-double-space nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; isearch ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf isearch :ensure nil
  :bind
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; buffers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; revert buffer when its file is changed on the filesystem
(leaf autorevert :ensure nil
  :require t
  :diminish autorevert-mode
  :init
  (global-auto-revert-mode 1)
  :setq
  (global-auto-revert-non-file-buffers . t)
  (auto-revert-use-notify . nil)
  (auto-revert-interval . 5))

(+leader-bind
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(my/last-selected-buffer :which-key "last-buffer")
  "bb" 'switch-to-buffer
  "bs" 'save-buffer)

(defalias 'my/last-selected-buffer 'mode-line-other-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; history ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remember recent files
(leaf recentf :ensure nil
  :hook emacs-startup-hook)

;; go to previous location in file when reopening
(leaf saveplace :ensure nil
  :init
  (save-place-mode 1))

;; persist minibuffer history over restarts
(leaf savehist :ensure nil
  :init
  (savehist-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; windows ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf ace-window
  :setq
  (aw-keys . '(?a ?o ?e ?u ?h ?t ?n ?s))
  (aw-scope . 'frame)
  (aw-background . nil)
  ;; (aw-dispatch-always . t)
  :bind
  ("M-o" . ace-window)
  :init
  (+leader-bind
    "w" '(:ignore t :which-key "window")
    "wd" 'delete-window
    "w+" 'balance-windows
    "wa" 'balance-windows-area
    ;; split window
    "wv" 'split-window-horizontally
    "ws" 'split-window-vertically
    ;; select window directionally
    "wp" '(windmove-up    :which-key "select up")
    "wn" '(windmove-down  :which-key "select down")
    "wf" '(windmove-right :which-key "select right")
    "wb" '(windmove-left  :which-key "select left")
    ;; misc
    "wm" 'switch-to-minibuffer))

;; (leaf eyebrowse
;;   :init
;;   (eyebrowse-mode 1))

;; (leaf bufler
;;   :bind
;;   ("C-x C-b" . bufler-list)
;;   :init
;;   (bufler-workspace-mode 1))

;; (leaf activities
;;   :disabled t
;;   :init
;;   (activities-mode)
;;   ;; (activities-tabs-mode)
;;   ;; prevent edebug default bindings from interfering
;;   ;; (setq edebug-inhibit-emacs-lisp-mode-bindings t)

;;   (defvar activities-mode-map
;;     (let ((map (make-sparse-keymap)))
;;       (define-key map (kbd "n") #'activities-new)
;;       (define-key map (kbd "d") #'activities-define)
;;       (define-key map (kbd "a") #'activities-resume)
;;       (define-key map (kbd "s") #'activities-suspend)
;;       (define-key map (kbd "k") #'activities-kill)
;;       (define-key map (kbd "RET") #'activities-switch)
;;       (define-key map (kbd "b")   #'activities-switch-buffer)
;;       (define-key map (kbd "g")   #'activities-revert)
;;       (define-key map (kbd "l")   #'activities-list)
;;       ;; set up autoloads
;;       (let ((cmds (mapcar #'cdr (cdr map))))
;;         (dolist (c cmds)
;;           (unless (fboundp c)
;;             (autoload c "activities" nil t))))
;;       ;; return map
;;       map))
;;   ;; bind map
;;   (global-set-key (kbd "C-x C-a") activities-mode-map)
;;   (global-set-key (kbd "C-c .") activities-mode-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dired ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/open-my-emacs-src-dir ()
  "Open Dired in `my/emacs-src-dir'."
  (interactive)
  (dired my/emacs-src-dir))

(leaf dired :ensure nil
  :setq
  (dired-listing-switches . "-Ahl --group-directories-first -X")
  (dired-auto-revert-buffer . t)        ; auto update file changes
  :bind (dired-mode-map
         ("h" . dired-up-directory)
         ("s" . dired-find-file)
         ("r" . dired-sort-toggle-or-edit))
  :init
  (+leader-bind
    "d" '(:ignore t :which-key "dired")
    "dd" 'find-file
    "dj" 'dired-jump
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fp" 'my/open-my-emacs-src-dir)
  :config
  ;; hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; use trash if trash executable is found
  (when (executable-find "trash")
    (setq delete-by-moving-to-trash t)))

(leaf dired-launch
  :after dired
  :config
  (dired-launch-enable)
  :setq
  (dired-launch-extensions-map
   . '(("pptx" ("libreoffice"))
       ("docx" ("libreoffice"))
       ("odt"  ("libreoffice"))
       ("html" ("librewolf")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helpful ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf helpful
  :setq
  (counsel-describe-function-function . #'helpful-callable)
  (counsel-describe-variable-function . #'helpful-variable)
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ("C-h h" . helpful-at-point)
  ("C-h H" . view-hello-file)          ; command originally at "C-h h"
  ("C-h M" . which-key-show-major-mode)
  ("C-h E" . describe-keymap))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; sudoedit ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudoedit
(leaf auto-sudoedit
  :commands auto-sudoedit-sudoedit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; profiler ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my/profiler-report ()
  "Profiler stop and report."
  (interactive)
  (profiler-stop)
  (profiler-report))

(+leader-bind
  "D" '(:ignore t :which-key "debug")
  "Ds" 'profiler-start
  "Dr" 'my/profiler-report)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-emacs)
;;; my-emacs.el ends here
