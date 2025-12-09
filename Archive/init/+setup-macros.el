;;; +setup-macros.el --- setup for setup                    -*- lexical-binding: t; -*-

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

;; Setup macros and such.

;;; Code:

(require 'setup)

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

;;;; :global

(setup-define :global
  (lambda (key command)
    `(global-set-key ,key ,command))
  :documentation "Globally bind KEY to COMMAND."
  :debug '(form sexp)
  :ensure '(kbd func)
  :repeatable t)

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

;;;; :diminish

(setup-define :diminish
  (lambda (&optional mode &rest rest)
    (let* ((mode (or mode (setup-get 'mode)))
           (mode (if (string-match-p "-mode\\'" (symbol-name mode))
                     mode
                   (intern (format "%s-mode" mode)))))
      `(diminish ',mode ,rest)))
  :documentation "Hide the mode-line lighter of the current mode.
Alternatively, MODE can be specified manually, and override the
current mode."
  :after-loaded t)

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

;;;; :pkg

(pcase +package-manager

;;;;; elpaca

  ('elpaca nil ;; Hack: added nil to deal with auto indentation

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
                        ,@body))))))

;;;;; straight.el

  ('straight

   (setup-define :pkg
     (lambda (recipe)
       `(unless (straight-use-package ',recipe)
          ,(setup-quit)))
     :documentation
     "Install RECIPE with `straight-use-package'.
This macro can be used as HEAD, and will replace itself with the
first RECIPE's package."
     :repeatable t
     :shorthand (lambda (sexp)
                  (let ((recipe (cadr sexp)))
                    (if (consp recipe)
                        (car recipe)
                      recipe))))

   (defmacro -setup (recipe &rest body)
     (declare (indent 1))
     (if (memq :disabled body)
         (format "%S :disabled by -setup" recipe)
       `(setup (:pkg ,recipe)
          ,@body)))))

;;;; -setup (elpaca shortcut)

;; (defmacro -setup (order &rest body)
;;   "Execute BODY in `setup' declaration after ORDER is finished.
;; If the :disabled keyword is present in body, the package is completely ignored.
;; This happens regardless of the value associated with :disabled.
;; The expansion is a string indicating the package has been disabled."
;;   (declare (indent 1))
;;   (if (memq :disabled body)
;;       (format "%S :disabled by -setup" order)
;;     (let ((o order))
;;       (when-let ((ensure (cl-position :ensure body)))
;;         (setq o (if (null (nth (1+ ensure) body)) nil order)
;;               body (append (cl-subseq body 0 ensure)
;;                            (cl-subseq body (+ ensure 2)))))
;;       `(elpaca ,o (setup
;;                       ,(if-let (((memq (car-safe order) '(quote \`)))
;;                                 (feature (flatten-tree order)))
;;                            (cadr feature)
;;                          (elpaca--first order))
;;                     ,@body)))))

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

;;;; benchmark all

(defun +benchmark-log (fmt &rest args)
  "Log a message into the *My Log* buffer."
  (with-current-buffer (get-buffer-create "*Benchmark Log*")
    (goto-char (point-max))
    (insert (apply #'format fmt args) "\n")))

(defmacro +benchmark-progn (name &rest body)
  (declare (debug t) (indent defun))
  (let ((value (make-symbol "value"))
	(start (make-symbol "start"))
	(gcs (make-symbol "gcs"))
	(gc (make-symbol "gc")))
    `(let ((,gc gc-elapsed)
	   (,gcs gcs-done)
           (,start (current-time))
           (,value (progn
                     ,@body)))
       (+benchmark-log "Benchmark [%fs%s] : %s"
                       (float-time (time-since ,start))
                       (if (> (- gcs-done ,gcs) 0)
                           (format " (%fs in %d GCs)"
	                           (- gc-elapsed ,gc)
	                           (- gcs-done ,gcs))
                         "")
                       ,name)
       ,value)))

(defun +setup-benchmark-wrap (orig-fun &rest args)
  (+benchmark-progn (nth 0 args)
    (apply orig-fun args)))

(advice-add 'setup :around #'+setup-benchmark-wrap)

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


;;;; custom :hook-into-all macro

(setup-define :hook-into-all
  (lambda (mode-lst)
    `(dolist (hook ,mode-lst)
       (add-hook hook #',(setup-get 'func))))
  :documentation "For each mode hook in MODE-LST, add current function."
  :repeatable nil)

;;; end

(provide '+setup-macros)
;;; +setup-macros.el ends here
