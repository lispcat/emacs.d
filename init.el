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

;;; Vars

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

;;; elpaca

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

;;; leaf

(elpaca leaf
  ;; add my own keywords
  (eval-after-load 'leaf
    (lambda nil
      (setq leaf-keywords
            (append
             leaf-keywords
             `(:elpaca-wait `(,@leaf--body :wait))))))
  :wait)

(elpaca leaf-keywords
  ;; custom keywords
  (leaf-keywords-init)
  (setq leaf-alias-keyword-alist '((:ensure . :elpaca)))
  (setq leaf-defaults (append '(:elpaca t) leaf-system-defaults))
  :wait)

;; hack: fix org version mismatch
;; (elpaca org)

;; finish all queues now to prevent async issues later
(elpaca-wait)

;;; setup.el

(elpaca setup
  (require 'setup)
  (elpaca-wait)

;;;; easier macro defining

  (defmacro defsetup (name signature &rest body)
    "Shorthand for `setup-define'.
NAME is the name of the local macro.  SIGNATURE is used as the
argument list for FN.  If BODY starts with a string, use this as
the value for :documentation.  Any following keywords are passed
as OPTS to `setup-define'."
    (declare (debug defun))
    (let (opts)
      (when (stringp (car body))
        (setq opts (nconc (list :documentation (pop body))
                          opts)))
      (while (keywordp (car body))
        (let* ((prop (pop body))
               (val `',(pop body)))
          (setq opts (nconc (list prop val) opts))))
      `(setup-define ,name
         (cl-function (lambda ,signature ,@body))
         ,@opts)))

;;;; macro for only setting user options

  (defmacro setc (&rest args)
    "Customize user options using ARGS like `setq'."
    (declare (debug setq))
    `(setup (:option ,@args)))

;;;; :autoload
  (setup-define :autoload
    (lambda (func)
      (let ((fn (if (memq (car-safe func) '(quote function))
                    (cadr func)
                  func)))
        `(unless (fboundp (quote ,fn))
           (autoload (function ,fn) ,(symbol-name (setup-get 'feature)) nil t))))
    :documentation "Autoload COMMAND if not already bound."
    :repeatable t
    :signature '(FUNC ...))

;;;; :quit

  (setup-define :quit
    #'setup-quit
    :documentation "Unconditionally abort the evaluation of the current body.")

;;;; :with-local-quit

  (setup-define :with-local-quit
    (lambda (&rest body)
      `(catch ',(setup-get 'quit)
         ,@body))
    :documentation "Prevent any reason to abort from leaving beyond BODY."
    :debug '(setup))

;;;; :load-after

  (setup-define :load-after
    (lambda (&rest features)
      (let ((body `(require ',(setup-get 'feature))))
        (dolist (feature (nreverse features))
          (setq body `(with-eval-after-load ',feature ,body)))
        body))
    :documentation "Load the current feature after FEATURES.")

;;;; :unhook

  (setup-define :unhook
    (lambda (func)
      `(remove-hook (quote ,(setup-get 'hook)) ,func))
    :documentation "Remove FUNC from the current hook."
    :repeatable t
    :ensure '(func)
    :signature '(FUNC ...))

;;;; :local-unhook

  (setup-define :local-unhook
    (lambda (hook &rest functions)
      `(add-hook
        (quote ,(setup-get 'hook))
        (lambda ()
          ,@(mapcar
             (lambda (arg)
               (let ((fn (cond ((eq (car-safe arg) 'function) arg)
                               ((eq (car-safe arg) 'quote)    `(function ,(cadr arg)))
                               ((symbolp arg)                 `(function ,arg))
                               (t                             arg))))
                 `(remove-hook (quote ,hook) ,fn t)))
             functions))))
    :documentation "Remove FUNCTION from HOOK only in the current hook."
    :debug '(&rest sexp)
    :repeatable nil)

;;;; :advice

  (setup-define :advice
    (lambda (symbol where function)
      `(advice-add ',symbol ,where ,function))
    :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
    :after-loaded t
    :debug '(sexp sexp function-form)
    :ensure '(nil nil func)
    :repeatable t)

;;;; :advice-def

  (setup-define :advice-def
    (lambda (symbol where arglist &rest body)
      (let ((name (gensym "setup-advice-")))
        `(progn
           (defun ,name ,arglist ,@body)
           (advice-add ',symbol ,where #',name))))
    :documentation "Add a piece of advice on a function.
See `advice-add' for more details."
    :after-loaded t
    :debug '(sexp sexp function-form)
    :indent 3)

;;;; :load-from

  (setup-define :load-from
    (lambda (path)
      `(let ((path* (expand-file-name ,path)))
         (if (file-exists-p path*)
             (add-to-list 'load-path path*)
           ,(setup-quit))))
    :documentation "Add PATH to load path.
This macro can be used as NAME, and it will replace itself with
the nondirectory part of PATH.
If PATH does not exist, abort the evaluation."
    :shorthand (lambda (args)
                 (intern
                  (file-name-nondirectory
                   (directory-file-name (cadr args))))))

;;;; :file-match (auto-mode-alist)

  (setup-define :file-match
    (lambda (glob)
      `(add-to-list 'auto-mode-alist (cons ,(wildcard-to-regexp pat) ',(setup-get 'mode))))
    :documentation "Associate the current mode with files that match GLOB."
    :debug '(form)
    :repeatable t)

;;;; :option* (shorthand)

  (setup-define :option*
    (lambda (name val)
      `(customize-set-variable
        ',(intern (format "%s-%s" (setup-get 'feature) name))
        ,val
        ,(format "Set for %s's setup block" (setup-get 'feature))))
    :documentation "Set the option NAME to VAL.
NAME is not the name of the option itself, but of the option with
the feature prefix."
    :debug '(sexp form)
    :repeatable t)

;;;; :doc

  (defvar setup--doc-alist nil "AList of docs for setup blocks.")

  (setup-define :doc
    (lambda (string)
      `(add-to-list 'setup--doc-alist '(,(setup-get 'feature) . ,string)))
    :documentation "Allow documentation using STRING.
This will be ignored at expansion."
    :signature '(STRING ...)
    :repeatable nil)

;;;; :pkg (elpaca)

  (defun setup-wrap-to-install-package (body _name)
    "Wrap BODY in an `elpaca' block if necessary.
The body is wrapped in an `elpaca' block if `setup-attributes'
contains an alist with the key `elpaca'."
    (if (assq 'elpaca setup-attributes)
        `(elpaca ,(cdr (assq 'elpaca setup-attributes)) ,@(macroexp-unprogn body))
      body))
  ;; Add the wrapper function
  (add-to-list 'setup-modifier-list #'setup-wrap-to-install-package)
  (setup-define :pkg
    (lambda (order &rest recipe)
      (push (cond
             ((eq order t) `(elpaca . ,(setup-get 'feature)))
             ((eq order nil) '(elpaca . nil))
             (`(elpaca . (,order ,@recipe))))
            setup-attributes)
      ;; If the macro wouldn't return nil, it would try to insert the result of
      ;; `push' which is the new value of the modified list. As this value usually
      ;; cannot be evaluated, it is better to return nil which the byte compiler
      ;; would optimize away anyway.
      nil)
    :documentation "Install ORDER with `elpaca'.
The ORDER can be used to deduce the feature context."
    :shorthand #'cadr)

;;;; -setup (elpaca shortcut)

  (defmacro -setup (order &rest body)
    "Execute BODY in `setup' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
    (declare (indent 1))
    (if (memq :disabled body)
        (format "%S :disabled by -setup" order)
      (let ((o order))
        (when-let ((ensure (cl-position :ensure body)))
          (setq o (if (null (nth (1+ ensure) body)) nil order)
                body (append (cl-subseq body 0 ensure)
                             (cl-subseq body (+ ensure 2)))))
        `(elpaca ,o (setup
                        ,(if-let (((memq (car-safe order) '(quote \`)))
                                  (feature (flatten-tree order)))
                             (cadr feature)
                           (elpaca--first order))
                      ,@body)))))

;;;; catch errors, throw warnings

  ;; method 1
  ;; (setq setup-modifier-list '(setup-wrap-to-demote-errors))

  ;; method 2
  (progn
    (defun my-protect-setup (expansion)
      "Wrap `setup' output with `condition-case'."
      (let ((err (gensym "setup-err")))
        `(condition-case ,err
             ,expansion
           (error
            (display-warning 'setup (concat "Problem in config: "
                                            (error-message-string ,err)
                                            ": \n"
                                            (with-output-to-string
                                              (pp (quote ,expansion))))
                             :error)))))
    (advice-add 'setup :filter-return #'my-protect-setup))

;;;; custom require macro

  (setq setup-macros (assq-delete-all ':require setup-macros))

  ;; (setup-define :require
;;     (lambda (&optional feature)
;;       (let ((pkg-feature (or feature
;;                              (setup-get 'feature)
;;                              (error "No feature specified and no context available"))))
;;         `(unless (require ',pkg-feature nil t)
;;            ,(setup-quit))))
;;     :documentation "Try to require FEATURE, or stop evaluating body.
;; If FEATURE is The first FEATURE can be used to deduce the feature context.")

  (setup-define :require-self
    (lambda (&optional feature)
      (let ((pkg-feature (or feature
                             (setup-get 'feature)
                             (error "No feature specified and no context available"))))
        `(unless (require ',pkg-feature nil t)
           ,(setup-quit))))
    :documentation "Try to require FEATURE, or stop evaluating body.
If FEATURE is The first FEATURE can be used to deduce the feature context.")

;;;; custom option macro

  ;; (setup-define :option-l
;;     (lambda (lst)
;;       `(progn
;;          ,@(mapcar
;;             (lambda (e)
;;               (custom-load-symbol ',name)
;;               (funcall (or (get ',name 'custom-set) #'set-default)
;;                        ',name ,val)))))
;;     :documentation "Try to require FEATURE, or stop evaluating body.
;; If FEATURE is The first FEATURE can be used to deduce the feature context.")


;;;; end of setup macros

  )

(elpaca-wait)

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
(elpaca-wait)

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
  `(progn
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

;;; Quality of Life

(setup emacs
  ;; A hook that runs after enabling a theme
  (defvar +after-enable-theme-hook nil)

  (defun +run-after-enable-theme-hook (&rest _args)
    (run-hooks '+after-enable-theme-hook))

  (advice-add 'enable-theme :after #'+run-after-enable-theme-hook))

;;; import _src.el

;; The rest of the configuration is loaded from `./src/_src.el`.

;; --

;; load ./src/src.el
(+require-or-load '_src)

;; --

;;; end

(provide 'init)
;;; init.el ends here
