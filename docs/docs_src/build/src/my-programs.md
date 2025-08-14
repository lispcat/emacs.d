# Code

```emacs-lisp
  :setq
  (eat-term-name . "xterm-256color")
  (eat-kill-buffer-on-exit . t)
  :defer-config
  (setq eat-shell (concat (or explicit-shell-file-name
                              (getenv "ESHELL")
                              shell-file-name)
                          " -c tmux"))
  :bind
  ("C-c a a" . eat)
  (eat-semi-char-mode-map
   ("M-o" . ace-window)))

(leaf eshell :ensure nil
  :bind
  ("C-c a e" . eshell))

(leaf magit
  :preface (elpaca transient) ; HACK: magit needs newer version
  :setq
  (magit-display-buffer-function . #'magit-display-buffer-same-window-except-diff-v1)
  :bind
  ("C-c v" . magit))

(leaf pdf-tools
  :config
  (pdf-loader-install)) ; On demand loading, leads to faster startup time

(leaf elfeed
  :defer-config
  ;; set `elfeed-feeds' to all files in `+elfeed-feeds-dir'.
  (defvar +elfeed-feeds-dir "~/feeds")
  (defun +elfeed-feeds-update-var ()
    (interactive)
    (setq elfeed-feeds
          (mapcar (lambda (s) (concat "file:" s))
                  (directory-files +elfeed-feeds-dir t
                                   directory-files-no-dot-files-regexp))))
  ;; run `+elfeed-feeds-update-var' before running `elfeed-update'
  (advice-add #'elfeed-update :before #'+elfeed-feeds-update-var))

(provide 'my-programs)
```



---

*Last updated: { git_revision_date_localized }*
