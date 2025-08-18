;;; +base.el ---                                   -*- lexical-binding: t; -*-

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

;; Set up mostly built-in Emacs functionality.

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Buffers                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- autorevert -------------------------------------------------------------

;; If a file is open as a buffer and its contents are changed on the disk, to
;; update that buffer, you need to run `revert-buffer'.

;; `autorevert-mode' automatically runs `revert-buffer' when it receives a
;; filesystem notification (if `auto-revert-use-notify' is non-nil), or at every
;; `auto-revert-interval'.

;; --

(leaf emacs :elpaca nil
  :init
  (global-auto-revert-mode 1)
  :custom
  ;; less verbose (don't print "Reverting buffer" in *Messages*)
  (auto-revert-verbose . nil)
  ;; work on non-file buffers (i.e. dired)
  (global-auto-revert-non-file-buffers . t)
  ;; manual check interval
  (auto-revert-interval . 10)
  ;; respond to Filesystem Notifications by the OS (instant updates)
  (auto-revert-use-notify . t))

;; --

;;; -- SPC-b binds ------------------------------------------------------------

(defalias '+last-selected-buffer 'mode-line-other-buffer)

(leader-bind
  "k" '(kill-current-buffer     :wk "kill-current")

  "b" '(:ignore t               :wk "Buffer")
  "bk" '(kill-current-buffer    :wk "kill-current")
  "bn" '(next-buffer            :wk "next")
  "bp" '(previous-buffer        :wk "prev")
  "bo" '(+last-selected-buffer  :wk "last-buffer")
  "bb" '(switch-to-buffer       :wk "switch-buffer")
  "bs" '(save-buffer            :wk "save-buffer"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   History                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- recentf ----------------------------------------------------------------

;; `recentf-mode' tracks recently opened files that you can search through and
;; open with `recentf'.

;; --

(leaf emacs :elpaca nil
  :hook
  (emacs-startup-hook . recentf-mode))

;; --

;;; -- saveplace --------------------------------------------------------------

;; `save-place-mode' saves your cursor location in a file, so that when
;; re-opening a file, your cursor jumps to your last location.
;; Persists over restarts.

;; --

(leaf emacs :elpaca nil
  :init
  (save-place-mode 1))

;; --

;;; -- savehist ---------------------------------------------------------------

;; `savehist-mode' persists minibuffer history over restarts.

;; --

(leaf emacs :elpaca nil
  :init
  (savehist-mode 1))

;; --

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   Windows                                  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- ace-window -------------------------------------------------------------

;; Efficiently switch between Emacs windows.
;;
;; repo: https://github.com/abo-abo/ace-window

;; --

(leaf ace-window
  :custom
  (aw-keys . '(?a ?o ?e ?u ?h ?t ?n ?s))
  (aw-scope . 'frame)
  (aw-background . nil)
  ;; (aw-dispatch-always . t)
  :bind
  ("M-o" . ace-window)
  :init
  (leader-bind
    "w" '(:ignore t :wk "window")
    "wd" 'delete-window
    "w+" 'balance-windows
    "wa" 'balance-windows-area
    ;; split window
    "wv" 'split-window-horizontally
    "ws" 'split-window-vertically
    ;; select window directionally
    "wp" '(windmove-up    :wk "select up")
    "wn" '(windmove-down  :wk "select down")
    "wf" '(windmove-right :wk "select right")
    "wb" '(windmove-left  :wk "select left")
    ;; misc
    "wm" 'switch-to-minibuffer))

;; --

;;; -- hydra: window ----------------------------------------------------------

;; A hydra for quick window switching
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Files                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +open-emacs-config-file ()
  "Open emacs config file."
  (interactive)
  (find-file +emacs-config-file))

(defun +open-agenda-file ()
  "Open agenda file."
  (interactive)
  (when-let* ((f (car org-agenda-files)))
    (find-file f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Dired                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Emacs' built-in file manager.

;; --

(leaf dired :elpaca nil
  :custom
  (dired-listing-switches . "-Ahl --group-directories-first -X")
  (dired-auto-revert-buffer . t)        ; auto update file changes
  :bind (dired-mode-map
         ("h" . dired-up-directory)
         ("s" . dired-find-file)
         ("r" . dired-sort-toggle-or-edit))
  :init
  (leader-bind
    "d" '(:ignore t :wk "dired")
    "dd" 'find-file
    "dj" 'dired-jump
    "f" '(:ignore t :wk "files")
    "ff" 'find-file
    "fp" '+open-emacs-config-file
    "fa" '+open-agenda-file)
  :config
  ;; hide details by default
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  ;; use trash if trash executable is found
  (when (executable-find "trash")
    (setq delete-by-moving-to-trash t)))

;; --

;;; -- dired-launch -----------------------------------------------------------

;; Easily launch external applications from dired.
;;
;; repo: https://codeberg.org/thomp/dired-launch

;; --

(leaf dired-launch
  :after dired
  :config
  (dired-launch-enable)
  :custom
  (dired-launch-extensions-map
   . '(("pptx" ("libreoffice"))
       ("docx" ("libreoffice"))
       ("odt"  ("libreoffice"))
       ("html" ("librewolf")))))

;; --

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    Misc                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; -- helpful ----------------------------------------------------------------

;; Improve Emacs' help buffer with much more contextual info.
;;
;; repo: https://github.com/Wilfred/helpful

;; --

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
  ("C-h H" . view-hello-file)           ; command originally at "C-h h"
  ("C-h M" . which-key-show-major-mode)
  ("C-h E" . describe-keymap))

;; --

;;; -- auto-sudoedit ----------------------------------------------------------

;; Automatically re-open files with sudo when needed.

;; --

(leaf auto-sudoedit
  :commands auto-sudoedit-sudoedit)

;; --

;;; -- profiling --------------------------------------------------------------

(defun +profiler-report ()
  "Profiler stop and report."
  (interactive)
  (profiler-stop)
  (profiler-report))

(leader-bind
  "D" '(:ignore t :wk "debug")
  "Ds" 'profiler-start
  "Dr" '+profiler-report)

;;; -- user info --------------------------------------------------------------

(setq user-full-name "lispcat")
(setq user-mail-address "187922791+lispcat@users.noreply.github.com")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     end                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide '+base)
;;; +base.el ends here

