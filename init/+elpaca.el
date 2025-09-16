;;; +elpaca.el --- elpaca init                       -*- lexical-binding: t; -*-

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

;; Elpaca init and configuration.

;;; Code:

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

;; Hack: exclude all externally installed packages from elpaca.
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

(provide '+elpaca)
;;; +elpaca.el ends here
