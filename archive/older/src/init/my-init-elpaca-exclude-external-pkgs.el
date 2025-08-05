;;; my-init-elpaca-exclude-external-pkgs.el --- exclude external packages for elpaca

;;; Commentary:
;; Exclude all externally installed packages from elpaca.

;;; Code:

(require 'elpaca)
(require 'cl-lib)
(eval-when-compile (require 'subr-x)) ;; is this ok?

(defun my/elpaca-get-external-pkgs ()
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

(dolist (pkg (my/elpaca-get-external-pkgs))
  (push pkg elpaca-ignored-dependencies))


(provide 'my-init-elpaca-exclude-external-pkgs)

;;; my-init-elpaca-exclude-external-pkgs.el ends here
