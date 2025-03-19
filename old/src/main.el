;;; main.el --- main

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun +load-all (target-dir &optional parent-path)
  "Load all files in TARGET-DIR.
PARENT-PATH defaults to `my/emacs-src-dir'."
  (let* ((dir (file-name-concat (or parent-path my/emacs-src-dir)
                                target-dir))
         (files (directory-files-recursively dir "^[^_].*\\.el$")))
    (dolist (path files)
      (load path))))

(defun +require-all (target-dir &optional parent-path)
  "Load all files in TARGET-DIR.
PARENT-PATH defaults to `my/emacs-src-dir'."
  (let* ((dir (file-name-concat (or parent-path my/emacs-src-dir)
                                target-dir))
         (files (directory-files-recursively dir "^[^_].*\\.el$")))
    (dolist (path files)
      (require (intern
                (file-name-sans-extension
                 (file-name-nondirectory path)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; my-init ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-init-no-littering)
(require 'my-init-elpaca)
(require 'my-init-elpaca-exclude-external-pkgs)
(require 'my-init-post-init)
(require 'my-init-requirements)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'my-completion)
(require 'my-emacs)
(require 'my-keyboard)
(require 'my-ide)
(require 'my-org)
(require 'my-org-extras)
(require 'my-persp)
(require 'my-ui)
(require 'my-programs)
(require 'my-misc)

(+load-all "unsorted")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tmp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf emacs :elpaca nil
  :after doom-themes kaolin-themes ef-themes
  :config
  (my/set-random-theme))

;; (load-theme 'doom-one t)

;; (use-package dashboard
;;   :demand t
;;   :config
;;   (add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
;;   (add-hook 'elpaca-after-init-hook #'dashboard-initialize)
;;   (dashboard-setup-startup-hook)
;;   (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
;;   (setq dashboard-center-content t))

;; (defun a ()
;;   (lambda () (message "loadedd after-make-frame-functions")))
;; (add-to-list 'after-make-frame-functions 'a)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; explore... ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; kind-icon
;; corfu-terminal
;; corfu-candidate-overlay
;; nerd-icons-corfu
;; epub reader (use-package calibredb)

;;;;;;;;;;;;;;;;;;;;;;;;;;;; end of main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(provide 'main)

;;; main.el ends here
