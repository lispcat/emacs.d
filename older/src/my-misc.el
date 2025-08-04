;;; my-misc.el --- misc configs

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf server :ensure nil
  :doc "Autostarts an Emacs server. Connect to it using emacsclient."
  :require t
  :bind
  ("C-c q" . delete-frame)
  ("C-c Q" . save-buffers-kill-emacs)
  :config
  (defun my/start-server-if-not-running ()
    "Start the Emacs server if not running."
    (unless (or (processp server-process)
                (server-running-p))
      (server-start)
      (message "Emacsclient Server started!")))
  :hook
  (after-init-hook . my/start-server-if-not-running))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; htmlize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf htmlize)

(leaf simple-httpd)

(leaf impatient-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-misc)
;;; my-misc.el ends here
