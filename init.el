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

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    vars                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; run .el instead of .elc if newer
(setq load-prefer-newer t)

;; silence compiler warnings
(setq native-comp-async-report-warnings-errors nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  load-path                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-path-and-subdirs-to-load-path (path &optional recursively?)
  "Add PATH and all its subdirs to the `load-path'."
  (when (setq path (expand-file-name path))
    (add-to-list 'load-path path)
    (if recursively?
        (let ((default-directory path))
          (normal-top-level-add-subdirs-to-load-path))
      (dolist (subdir (directory-files path t directory-files-no-dot-files-regexp t))
        (when (file-directory-p subdir)
          (add-to-list 'load-path subdir))))))

(add-path-and-subdirs-to-load-path +emacs-src-dir t)
(add-path-and-subdirs-to-load-path +emacs-submodules-dir)

(defun +load-all (target-dir &optional parent-path)
  "Load all files in TARGET-DIR.
PARENT-PATH defaults to `+emacs-src-dir'."
  (let* ((dir (file-name-concat (or parent-path +emacs-src-dir)
                                target-dir))
         (files (directory-files-recursively dir "^[^_].*\\.el$")))
    (dolist (path files)
      (load path))))

(defun +require-all (target-dir &optional parent-path)
  "Load all files in TARGET-DIR.
PARENT-PATH defaults to `+emacs-src-dir'."
  (let* ((dir (file-name-concat (or parent-path +emacs-src-dir)
                                target-dir))
         (files (directory-files-recursively dir "^[^_].*\\.el$")))
    (dolist (path files)
      (require (intern
                (file-name-sans-extension
                 (file-name-nondirectory path)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                no-littering                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(progn ;; no-littering

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   elpaca                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; elpaca 0.11
(progn
  (defvar elpaca-installer-version 0.11)
  (defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
  (defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
  (defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
  (defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                                :ref nil :depth 1 :inherit ignore
                                :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                                :build (:not elpaca--activate-package)))
  (let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
         (build (expand-file-name "elpaca/" elpaca-builds-directory))
         (order (cdr elpaca-order))
         (default-directory repo))
    (add-to-list 'load-path (if (file-exists-p build) build repo))
    (unless (file-exists-p repo)
      (make-directory repo t)
      (when (<= emacs-major-version 28) (require 'subr-x))
      (condition-case-unless-debug err
          (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                    ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                    ,@(when-let* ((depth (plist-get order :depth)))
                                                        (list (format "--depth=%d" depth) "--no-single-branch"))
                                                    ,(plist-get order :repo) ,repo))))
                    ((zerop (call-process "git" nil buffer t "checkout"
                                          (or (plist-get order :ref) "--"))))
                    (emacs (concat invocation-directory invocation-name))
                    ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                          "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                    ((require 'elpaca))
                    ((elpaca-generate-autoloads "elpaca" repo)))
              (progn (message "%s" (buffer-string)) (kill-buffer buffer))
            (error "%s" (with-current-buffer buffer (buffer-string))))
        ((error) (warn "%s" err) (delete-directory repo 'recursive))))
    (unless (require 'elpaca-autoloads nil t)
      (require 'elpaca)
      (elpaca-generate-autoloads "elpaca" repo)
      (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
  (add-hook 'after-init-hook #'elpaca-process-queues)
  (elpaca `(,@elpaca-order)))


;; install use-package
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t))

;; hack: exclude all externally installed packages from elpaca.
(progn
  (require 'elpaca)
  (require 'cl-lib)
  (eval-when-compile (require 'subr-x)) ;; is this ok?

  (defun +elpaca-get-external-pkgs ()
    "Based on `package-load-all-descriptors'."
    (let ((pkg-dir-lst nil)
          (res nil))
      (dolist (dir (cons package-user-dir package-directory-list))
        (when (file-directory-p dir)
          (dolist (pkg-dir (directory-files dir t "\\`[^.]"))
            (when (file-directory-p pkg-dir)
              (push pkg-dir pkg-dir-lst)))))
      (dolist (pkg-dir pkg-dir-lst)
        (let ((pkg-file (expand-file-name (package--description-file pkg-dir)
                                          pkg-dir))
              (signed-file (concat pkg-dir ".signed")))
          (when (file-exists-p pkg-file)
            (with-temp-buffer
              (insert-file-contents pkg-file)
              (goto-char (point-min))
              (let ((pkg-text (read (current-buffer))))
                (if (not (eq 'define-package (car-safe pkg-text)))
                    (error "Package %s doesn't have \"define-package\"" pkg-file)
                  (let ((name (cadr pkg-text)))
                    (when name
                      (cl-pushnew (intern name) res)))))))))
      res))

  (dolist (pkg (+elpaca-get-external-pkgs))
    (push pkg elpaca-ignored-dependencies)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    leaf                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(elpaca leaf
  :wait                       ; deferred by default. demand with :leaf-defer nil
  )

(elpaca leaf-keywords
  (leaf-keywords-init)
  (setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
  (setq leaf-defaults (append '(:ensure t) leaf-system-defaults))
  :wait)

;; hack: fix org version mismatch
(elpaca org)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                startup hooks                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 10000 10000))))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when (file-exists-p custom-file)
              (load custom-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             necessary packages                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package general :ensure (:wait t)
  :demand t
  :config
  (general-create-definer general-my-map
    :prefix "C-c"))

(use-package diminish :ensure (:wait t)
  :demand t)

(use-package which-key :ensure (:wait t)
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package hydra :ensure (:wait t)
  :demand t)

;;; -- hello ${2:$(make-string (max 0 (- fill-column (length (concat ";;; -- " yas-text " ")))) ?-)}



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                   import                                   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; load ./src/src.el
(require 'src)

;;; end
(provide 'init)
;;; init.el ends here
