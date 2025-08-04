;; [[file:../README.org::*misc][misc:1]]
(setq user-full-name "lispcat")
(setq user-mail-address "187922791+lispcat@users.noreply.github.com")
;; misc:1 ends here

;; [[file:../README.org::*buffers][buffers:1]]
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

(general-my-map
  "k" 'kill-current-buffer
  "b" '(:ignore t :which-key "buffer")
  "bk" 'kill-current-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer
  "bo" '(my/last-selected-buffer :which-key "last-buffer")
  "bb" 'switch-to-buffer
  "bs" 'save-buffer)

(defalias 'my/last-selected-buffer 'mode-line-other-buffer)
;; buffers:1 ends here

;; [[file:../README.org::*history][history:1]]
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
;; history:1 ends here

;; [[file:../README.org::*window management][window management:1]]
(leaf ace-window
  :setq
  (aw-keys . '(?a ?o ?e ?u ?h ?t ?n ?s))
  (aw-scope . 'frame)
  (aw-background . nil)
  ;; (aw-dispatch-always . t)
  :bind
  ("M-o" . ace-window)
  :init
  (general-my-map
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

(defhydra hydra-window ()
  "
Movement^^        ^Split^         ^Switch^		^Resize^
----------------------------------------------------------------
_h_ ←       	_v_ertical    	_b_uffer		_q_ X←
_j_ ↓        	_x_ horizontal	_f_ind files	_w_ X↓
_k_ ↑        	_z_ undo      	_a_ce 1		_e_ X↑
_l_ →        	_Z_ reset      	_s_wap		_r_ X→
_F_ollow		_D_lt Other   	_S_ave		max_i_mize
_SPC_ cancel	_o_nly this   	_d_elete
"
  ("h" windmove-left )
  ("j" windmove-down )
  ("k" windmove-up )
  ("l" windmove-right )
  ("q" hydra-move-splitter-left)
  ("w" hydra-move-splitter-down)
  ("e" hydra-move-splitter-up)
  ("r" hydra-move-splitter-right)
  ("b" helm-mini)
  ("f" helm-find-files)
  ("F" follow-mode)
  ("a" (lambda ()
         (interactive)
         (ace-window 1)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   )
  ("x" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   )
  ("s" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("S" save-buffer)
  ("d" delete-window)
  ("D" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body))
   )
  ("o" delete-other-windows)
  ("i" ace-maximize-window)
  ("z" (progn
         (winner-undo)
         (setq this-command 'winner-undo))
   )
  ("Z" winner-redo)
  ("SPC" nil)
  )
;; window management:1 ends here

;; [[file:../README.org::*dired][dired:1]]
(defun my/open-emacs-config-file ()
  "Open emacs config file."
  (interactive)
  (find-file my/emacs-config-file))

(defun my/open-agenda-file ()
  "Open agenda file."
  (interactive)
  (find-file "~/Notes/org/agenda.org"))

(leaf dired :ensure nil
  :setq
  (dired-listing-switches . "-Ahl --group-directories-first -X")
  (dired-auto-revert-buffer . t)        ; auto update file changes
  :bind (dired-mode-map
         ("h" . dired-up-directory)
         ("s" . dired-find-file)
         ("r" . dired-sort-toggle-or-edit))
  :init
  (general-my-map
    "d" '(:ignore t :which-key "dired")
    "dd" 'find-file
    "dj" 'dired-jump
    "f" '(:ignore t :which-key "files")
    "ff" 'find-file
    "fp" 'my/open-emacs-config-file
    "fa" 'my/open-agenda-file)
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
;; dired:1 ends here

;; [[file:../README.org::*helpful][helpful:1]]
(leaf helpful
  :commands helpful--bookmark-jump
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
;; helpful:1 ends here

;; [[file:../README.org::*sudoedit][sudoedit:1]]
;; sudoedit
(leaf auto-sudoedit
  :commands auto-sudoedit-sudoedit)
;; sudoedit:1 ends here

;; [[file:../README.org::*profiler][profiler:1]]
(defun my/profiler-report ()
  "Profiler stop and report."
  (interactive)
  (profiler-stop)
  (profiler-report))

(general-my-map
  "D" '(:ignore t :which-key "debug")
  "Ds" 'profiler-start
  "Dr" 'my/profiler-report)
;; profiler:1 ends here

;; [[file:../README.org::*end][end:1]]
(provide 'base)
;; end:1 ends here
