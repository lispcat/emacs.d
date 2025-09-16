;;; init.el --- the main init file                   -*- lexical-binding: t; -*-

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

;; The main init file.
;; This is automatically ran at startup after early-init.el.

;;; Code:

;;; set variables

;; Set various vars for sane defaults.

;; --

;; run .el instead of .elc if newer
(setq load-prefer-newer t)

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;; --

;;; no-littering

(progn ;; no-littering

  ;; manually add
  (add-to-list 'load-path
               (file-name-concat +emacs-submodules-dir
                                 "no-littering"))

  (require 'no-littering)
  ;; variables
  (setq auto-save-default nil)          ; don't autosave all file buffers
  (setq backup-by-copying t)            ; safer backups
  (setq undo-tree-auto-save-history nil)
  ;; Dont litter project folders with backup files
  (let ((backup-dir (no-littering-expand-var-file-name "backup/")))
    (make-directory backup-dir t)
    (setq backup-directory-alist
          `(("\\`/tmp/" . nil)
            ("\\`/dev/shm/" . nil)
            ("." . ,backup-dir))))
  ;; Tidy up auto-save files
  (let ((auto-save-dir (no-littering-expand-var-file-name "auto-save/")))
    (make-directory auto-save-dir t)
    (setq auto-save-file-name-transforms
          `(("\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
             ,(concat (file-name-as-directory temporary-file-directory) "\\2") t)
            ("\\`/tmp\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
            ("\\`/dev/shm\\([^/]*/\\)*\\(.*\\)\\'" "\\2")
            ("." ,auto-save-dir t)))))

;;; Load path (TODO)

(add-to-list 'load-path +emacs-init-dir)

;;; Package manager

(defcustom +package-manager 'elpaca
  "The package manager to use."
  :options '(elpaca straight))

(defmacro +pkg-install (&rest body)
  (pcase +package-manager
    ('elpaca `(elpaca ,@body))
    ('straight `(straight-use-package ,@body))
    (_ (user-error "+pkg-install: invalid value: %S" +pkg-install))))

;;;; elpaca

(when (eq +package-manager 'elpaca)
  (require '+elpaca))

;;;; straight.el

(when (eq +package-manager 'straight)
  (require '+straight))

;;; leaf (TODO: remove all)

(+pkg-install leaf)
(when (eq +package-manager 'elpaca) (elpaca-wait))

(+pkg-install leaf-keywords)
(when (eq +package-manager 'elpaca) (elpaca-wait))
;; custom keywords
(leaf-keywords-init)
(setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
(setq leaf-defaults (append '(:elpaca t) leaf-system-defaults))
(when (eq +package-manager 'elpaca) (elpaca-wait))

;; hack: fix org version mismatch
;; (elpaca org)

;; finish all queues now to prevent async issues later
;; (when (eq +package-manager 'elpaca) (elpaca-wait))

;;; setup.el

(+pkg-install setup)
(when (eq +package-manager 'elpaca) (elpaca-wait))
(require 'setup)
(when (eq +package-manager 'elpaca) (elpaca-wait))
(require '+setup)

(when (eq +package-manager 'elpaca) (elpaca-wait))

;;; necessary packages

;; fix issues with missing programs from shell
(-setup exec-path-from-shell
  ;; (setq exec-path-from-shell-debug t)
  (when (or (memq window-system '(pgkt x))
            (daemonp))
    (exec-path-from-shell-initialize)))

;; quickly set up keybinds
(-setup general
  (general-create-definer leader-bind :prefix "C-c"))

;; hide modes from the modeline
(-setup diminish
  (:require-self))

;; display keystroke options
(-setup which-key
  (:diminish)
  (:option* idle-delay 0.3)
  (which-key-mode 1))

;; lingering key menus for repeated keypresses
(-setup hydra
  (defmacro +defhydra-repeat (fn keys)
    "Create a repeatable function FN from a list of KEYS."
    (let* ((fn-hydra (intern (concat (symbol-name fn) "-hydra"))))
      `(defhydra ,fn-hydra ()
         ,@(mapcar (lambda (k)
                     (list k fn))
                   keys)))))

;; functional programming library
;; https://github.com/magnars/dash.el
(-setup dash
  (:require-self)
  ;; (require 'dash)

  ;; FIXME
  (defun -debug (label)
    "Debugging helper function for dash.el."
    (lambda (m)
      (message "%s: %S" label m)
      m))

  ;; FIXME
  (defmacro -tap (value form)
    "Evaluate FORM with VALUE as its argument, then return VALUE unchanged.
This is the non-anaphoric version - VALUE is passed as an argument to FORM."
    `(let ((val ,value))
       ,form
       val))

  ;; FIXME
  (defmacro --tap (value &rest body)
    "Evaluate BODY with VALUE bound to `it`, then return VALUE unchanged.
  This is the anaphoric version - VALUE is available as `it` in BODY."
    `(let ((it ,value))
       ,@body
       it))

  ;; FIXME
  (defmacro --tap (value &rest body)
    "Evaluate BODY with VALUE bound to `it`, then return VALUE unchanged.
  This is the anaphoric version - VALUE is available as `it` in BODY."
    (declare (debug (form body)) (indent 1))
    (let ((val (make-symbol "value")))
      `(let ((,val ,value))
         (let ((it ,val))
           (ignore it)
           ,@body)
         ,val))))

;; (leaf dash :elpaca-wait t
;;   :require t
;;   :config
;;   (defun -debug (label)
;;     "Debugging helper function for dash.el."
;;     (lambda (m)
;;       (message "%s: %S" label m)
;;       m))

;;   (defmacro -tap (value form)
;;     "Evaluate FORM with VALUE as its argument, then return VALUE unchanged.
;; This is the non-anaphoric version - VALUE is passed as an argument to FORM."
;;     `(let ((val ,value))
;;        ,form
;;        val))

;;   (defmacro --tap (value &rest body)
;;       "Evaluate BODY with VALUE bound to `it`, then return VALUE unchanged.
;;   This is the anaphoric version - VALUE is available as `it` in BODY."
;;       `(let ((it ,value))
;;          ,@body
;;          it))

;;   (defmacro --tap (value &rest body)
;;       "Evaluate BODY with VALUE bound to `it`, then return VALUE unchanged.
;;   This is the anaphoric version - VALUE is available as `it` in BODY."
;;       (declare (debug (form body)) (indent 1))
;;       (let ((val (make-symbol "value")))
;;         `(let ((,val ,value))
;;            (let ((it ,val))
;;              (ignore it)
;;              ,@body)
;;            ,val)))
;;   )


;; files/dirs library
;; https://github.com/rejeep/f.el
(-setup f)

;; ;; string manipulation library
;; ;; https://github.com/magnars/s.el
(-setup s)

;; finish all queues now to prevent async issues later
(when (eq +package-manager 'elpaca) (elpaca-wait))

;;;; adding to the load-path

;;
;; `load-path':
;;
;; - A variable; a list of paths.
;;   - Paths to search for when loading an elisp file (like with `require').
;; - Typically, for every elisp package, you add its root dir to this list so
;;   that its main .el file is visible, and thus loadable.
;; - To make adding paths to this variable easier, we define the following
;;   function.

;; --
(defun +add-to-load-path-recursively (path depth &optional exclude-self)
  "Add PATH and its recursive subdirs to `load-path'.

DEPTH specifies how deeply to recurse. 0 for just PATH, 1 for PATH and its
subdirs, n>1 for till depth n, and -1 for infinite depth.

If EXCLUDE-SELF is non-nil, exclude PATH, and include only its recursive
subdirs.

If PATH or a subdir contains a =.nosearch= file, it's excluded.

This function returns a list of paths that were added to (or already exist in)
`load-path'."
  (cl-labels
      (;; return t if .nosearch file exists within dir
       (nosearch-subfile-p (d)
         (file-exists-p (expand-file-name ".nosearch" d)))

       ;; collect recursive subdirs
       (collect-fn (current-dir current-depth)
         (when (and (integerp current-depth)
                    ;; base case (unless inf depth)
                    (/= current-depth 0)
                    (file-directory-p current-dir))
           (let ((subdirs
                  (->> (directory-files current-dir t
                                        directory-files-no-dot-files-regexp)
                       (-filter #'file-directory-p)
                       (-remove #'nosearch-subfile-p))))
             ;; collect [ subdirs + (recurse subdirs) ]
             (->> subdirs
                  (-mapcat (lambda (sub)
                             (collect-fn sub (1- current-depth))))
                  (append subdirs))))))
    (let* ((collected (collect-fn path depth))
           (result (if (or exclude-self (nosearch-subfile-p path))
                       collected
                     (cons path collected))))

      ;; add result to `load-path'
      (--each result (add-to-list 'load-path it))
      result)))

(+add-to-load-path-recursively +emacs-src-dir -1)
(+add-to-load-path-recursively +emacs-submodules-dir 1)

;; --

;;;; loading functions

;;
;; `require'
;;
;; - A function that searches for and loads a corresponding elisp file from the
;;  `load-path'.
;;   - Any files loaded with `require' will be saved to the `features' var,
;;     so it can keep track of which files were loaded, prevent duplicate loads,
;;     and manage dependencies.
;;     - A side effect is that re-running `require' with the same arg will do
;;       nothing.
;; - By default, if it throws an error, it terminates Emacs initialization. To
;;   prevent this, we write a wrapper `+require' that catches any errors and
;;   converts them into warnings.
;;

;;
;; `load'
;;
;; - A function similar to `require', but with some key differences:
;;   - Can take any arbitrary file path.
;;   - Allows dupulicate loads.
;;   - Does not add to the `features' var.
;; - We make a wrapper `+load' for this function as well.
;;

;;
;; `+require-or-load'
;;
;; - Runs `+require' if not yet loaded, and `+load' if already loaded.
;;

;; --

(defmacro +require (feature &optional filename noerror)
  "A wrapper around `require' to warn instead of error."
  `(progn
     (condition-case-unless-debug err
         (require ,feature ,filename ,noerror)
       (error
        (display-warning 'require
                         (format "Failed to require: %s" err)
                         :error)))))

(defmacro +load (file &optional noerror nomessage nosuffix must-suffix)
  "A wrapper around `load' to warn instead of error."
  `(progn
     (condition-case-unless-debug err
         (load ,file ,noerror ,nomessage ,nosuffix ,must-suffix)
       (error
        (display-warning 'load
                         (format "Failed to load: %s" err)
                         :error)))))

(defmacro +require-or-load (feature)
  "If FEATURE is loaded, run `+load', Else, run `+require'."
  `(+benchmark-progn ,feature
     (unless (symbolp ,feature)
       (error "Expected symbol, got: %S" ,feature))
     (if (featurep ,feature)
         (+load (symbol-name ,feature))
       (+require ,feature))))

;; --

;;; startup hooks

;;
;; `emacs-startup-hook'
;;
;; - evals after emacs-startup.
;;

;;
;; `elpaca-after-init-hook'
;;
;; - evals after elpaca finishes installing all packages.
;; - essentially `after-init-hook' but elpaca-compatible.
;;


;; --

;; print init time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

;; increase gc freq
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 10000 10000))))

;; load saved customizations file
;; TODO: load this sooner? prevent freezing due to dir-locals by org-agenda
(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when (file-exists-p custom-file)
              (load custom-file))))

;; --

;;; quality of life

(setup emacs
  ;; A hook that runs after enabling a theme
  (defvar +after-enable-theme-hook nil)

  (defun +run-after-enable-theme-hook (&rest _args)
    (run-hooks '+after-enable-theme-hook))

  (advice-add 'enable-theme :after #'+run-after-enable-theme-hook))

;;; load _src.el

;; The rest of the configuration is loaded from `./src/_src.el`.

;; --

;; load ./src/src.el
(+require-or-load '_src)

;; --

;;; end

(provide 'init)
;;; init.el ends here
