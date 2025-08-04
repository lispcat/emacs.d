;;; my-init-post-init.el --- stuff relevant to post-init

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;; garbage collection ;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s seconds with %d garbage collections."
                     (emacs-init-time "%.2f")
                     gcs-done)))

(add-hook 'elpaca-after-init-hook
          (lambda ()
            (setq gc-cons-threshold (* 10000 10000))))

(provide 'my-init-post-init)

;;; my-init-post-init.el ends here
