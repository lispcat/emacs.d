;;; init.el --- my init

;;; Commentary:
;;
;; organized as: main.el

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; load-path ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl-lib)

(defun add-subdirs-to-load-path (path &optional recursively?)
  "Add PATH and all its subdirs to the `load-path'."
  (when (set 'path (expand-file-name path))
    (add-to-list 'load-path path)
    (if recursively?
        (let ((default-directory path))
          (normal-top-level-add-subdirs-to-load-path))
      (dolist (subdir (directory-files path t directory-files-no-dot-files-regexp t))
        (when (file-directory-p subdir)
          (add-to-list 'load-path subdir))))))

(add-subdirs-to-load-path my/emacs-src-dir t)
(add-subdirs-to-load-path my/emacs-submodules-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; custom-file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (when (file-exists-p custom-file)
              (load custom-file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'main)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(message "Emacs initialized!")

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
