(leaf server :ensure nil
  :doc "Autostarts an Emacs server. Connect to it using emacsclient."
  :require t
  :bind
  ("C-c q" . delete-frame)
  ("C-c Q" . save-buffers-kill-emacs)
  :config
  (defun +start-server-if-not-running ()
    "Start the Emacs server if not running."
    (unless (or (processp server-process)
                (server-running-p))
      (server-start)
      (message "Emacsclient Server started!")))
  ;; :hook
  ;; (emacs-startup-hook . +start-server-if-not-running)
  )

(leaf htmlize)

(leaf simple-httpd)

(leaf impatient-mode)

(provide '+misc)
