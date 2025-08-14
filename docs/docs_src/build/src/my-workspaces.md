NOTE: modify #'persp-save-state-to-file arg (keep-others-in-non-parametric-file 'yes)

maybe have each persp have its own save file, and when autosaving, save each persp?
maybe have a function to delete a persp from the main autosave file?
- prompt available perspectives from main autosave file, after selection, delete each from file.

```emacs-lisp
  :bind-keymap
  ("C-c w w" . persp-key-map)
  ("C-c ." . persp-key-map)
  ("C-c (" . persp-key-map)
  :bind (persp-key-map
         ("." . my-persp-load-name-from-latest)
         ("D" . my-persp-delete-name-from-latest))
  :setq
  (wg-morph-on . nil)
  (persp-autokill-buffer-on-remove . 'kill-weak)
  ;; (persp-auto-resume-time . 0.1)
  (persp-auto-resume-time . -1)
  (persp-auto-save-opt . 2)
  ;; prevent issue with persp-special-last-buffer
  :hook
  (elpaca-after-init-hook . (lambda () (persp-mode 1)))
  ;; :init
  ;; (setq persp-is-ibc-as-f-supported nil)
  ;; (persp-mode)
  ;; (message "persp-mode enabled?")
  ;; (with-eval-after-load 'persp-mode
  ;;   (message "persp-mode enabled!!!"))
  ;; (add-to-list 'find-file-hook (lambda () (message "WOWWWW WHYYYY")))
  :commands
  persp-consult-source ;; defined below
  :config
  ;; dont save persp-nil to file
  (set-persp-parameter 'dont-save-to-file t nil)
  ;; consult-buffer integration
  (defvar persp-consult-source
    (list :name     "Persp Buffers"
          :narrow   ?
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items
          (lambda ()
            (let ((current-persp (get-current-persp)))
              (consult--buffer-query
               :sort 'visibility
               :predicate (lambda (buf)
                            (and current-persp
                                 (persp-contain-buffer-p buf)))
               :as 'buffer-name)))))
  (defvar persp-rest-consult-source
    (list :name     "Other Buffers"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items
          (lambda ()
            (let ((current-persp (get-current-persp)))
              (consult--buffer-query
               :sort 'visibility
               :predicate (lambda (buf)
                            (if current-persp
                                (not (persp-contain-buffer-p buf))
                              t))
               :as 'buffer-name)))))
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-rest-consult-source)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  ;; load from file
  (cl-defun my-persp-load-name-from-latest (&optional (fname persp-auto-save-fname)
                                                      (phash *persp-hash*)
                                                      name savelist)
    "Load and switch to a perspective via name from the latest backup file."
    (interactive)
    (unless savelist
      (setq savelist (persp-savelist-from-savefile fname)))
    (when savelist
      (let* ((available-names (persp-list-persp-names-in-file fname savelist))
             (loaded-names (persp-names-current-frame-fast-ordered))
             (unloaded-names (seq-remove (lambda (p) (member p loaded-names)) available-names)))
        (when unloaded-names
          (setq name
                (persp-read-persp
                 "to load" nil nil t t nil unloaded-names t 'push)))))
    (when name
      (let ((names-regexp (regexp-opt (list name))))
        (persp-load-state-from-file fname phash names-regexp t savelist))
      ;; switch to new loaded persp
      (persp-frame-switch name)))

  ;; don't overwrite backup file with current; merge.
  (advice-add 'persp-save-state-to-file :around
              (lambda (orig-fun &rest args)
                ;; We need to modify the fourth optional parameter
                ;; Default arguments structure:
                ;; (fname phash respect-persp-file-parameter keep-others-in-non-parametric-file)
                (let ((fname (or (nth 0 args) persp-auto-save-fname))
                      (phash (or (nth 1 args) *persp-hash*))
                      (respect-param (or (nth 2 args) persp-auto-save-persps-to-their-file))
                      ;; Always set the fourth parameter to 'yes regardless of what was passed
                      (keep-others 'yes))
                  ;; Call the original function with modified arguments
                  (funcall orig-fun fname phash respect-param keep-others))))


  ;; delete persp from file
  (defun my-persp-delete-name-from-latest ()
    (interactive)
    (let* ((fname persp-auto-save-fname)
           (savelist (persp-savelist-from-savefile fname))
           (available-names (persp-list-persp-names-in-file fname savelist))
           (names (persp-read-persp
                   "to delete" 'reverse nil t nil nil available-names t 'push))
           (filtered-savelist (cl-remove-if
                               (lambda (expr)
                                 (and (listp expr)
                                      (eq (car expr) 'def-persp)
                                      (seq-contains-p names (cadr expr))))
                               savelist)))
      (if (y-or-n-p (format "Delete %s?" names))
          (persp-savelist-to-file filtered-savelist fname))))
  )

;; enable persp-mode-project-bridge mode

;; (when nil
;;   (with-eval-after-load "persp-mode"
;;     (defvar persp-mode-projectile-bridge-before-switch-selected-window-buffer nil)

;;     ;; (setq persp-add-buffer-on-find-file 'if-not-autopersp)

;;     (persp-def-auto-persp
;;      "projectile"
;;      :parameters '((dont-save-to-file . t)
;;                    (persp-mode-projectile-bridge . t))
;;      :hooks '(projectile-before-switch-project-hook
;;               projectile-after-switch-project-hook
;;               projectile-find-file-hook
;;               find-file-hook)
;;      :dyn-env '((after-switch-to-buffer-adv-suspend t))
;;      :switch 'frame
;;      :predicate
;;      #'(lambda (buffer &optional state)
;;          (if (eq 'projectile-before-switch-project-hook
;;                  (alist-get 'hook state))
;;              state
;;            (and
;;             projectile-mode
;;             (buffer-live-p buffer)
;;             (buffer-file-name buffer)
;;             ;; (not git-commit-mode)
;;             (projectile-project-p)
;;             (or state t))))
;;      :get-name
;;      #'(lambda (state)
;;          (if (eq 'projectile-before-switch-project-hook
;;                  (alist-get 'hook state))
;;              state
;;            (push (cons 'persp-name
;;                        (concat "[p] "
;;                                (with-current-buffer (alist-get 'buffer state)
;;                                  (projectile-project-name))))
;;                  state)
;;            state))
;;      :on-match
;;      #'(lambda (state)
;;          (let ((hook (alist-get 'hook state))
;;                (persp (alist-get 'persp state))
;;                (buffer (alist-get 'buffer state)))
;;            (pcase hook
;;              (projectile-before-switch-project-hook
;;               (let ((win (if (minibuffer-window-active-p (selected-window))
;;                              (minibuffer-selected-window)
;;                            (selected-window))))
;;                 (when (window-live-p win)
;;                   (setq persp-mode-projectile-bridge-before-switch-selected-window-buffer
;;                         (window-buffer win)))))

;;              (projectile-after-switch-project-hook
;;               (when (buffer-live-p
;;                      persp-mode-projectile-bridge-before-switch-selected-window-buffer)
;;                 (let ((win (selected-window)))
;;                   (unless (eq (window-buffer win)
;;                               persp-mode-projectile-bridge-before-switch-selected-window-buffer)
;;                     (set-window-buffer
;;                      win persp-mode-projectile-bridge-before-switch-selected-window-buffer)))))

;;              (find-file-hook
;;               (setcdr (assq :switch state) nil)))
;;            (if (pcase hook
;;                  (projectile-before-switch-project-hook nil)
;;                  (t t))
;;                (persp--auto-persp-default-on-match state)
;;              (setcdr (assq :after-match state) nil)))
;;          state)
;;      :after-match
;;      #'(lambda (state)
;;          (when (eq 'find-file-hook (alist-get 'hook state))
;;            (run-at-time 0.5 nil
;;                         #'(lambda (buf persp)
;;                             (when (and (eq persp (get-current-persp))
;;                                        (not (eq buf (window-buffer (selected-window)))))
;;                               ;; (switch-to-buffer buf)
;;                               (persp-add-buffer buf persp t nil)))
;;                         (alist-get 'buffer state)
;;                         (get-current-persp)))
;;          (persp--auto-persp-default-after-match state)))

;;     ;; (add-hook 'persp-after-load-state-functions
;;     ;;           #'(lambda (&rest args) (persp-auto-persps-pickup-buffers)) t)
;;     ))

;; Shows groups for all perspectives. But can't show same buffer in multiple groups.

;; (with-eval-after-load "ibuffer"

;;   (require 'ibuf-ext)

;;   (define-ibuffer-filter persp
;;       "Toggle current view to buffers of current perspective."
;;     (:description "persp-mode"
;;                   :reader (persp-prompt nil nil (safe-persp-name (get-frame-persp)) t))
;;     (find buf (safe-persp-buffers (persp-get-by-name qualifier))))

;;   (defun persp-add-ibuffer-group ()
;;     (let ((perspslist (mapcar #'(lambda (pn)
;;                                   (list pn (cons 'persp pn)))
;;                               (nconc
;;                                (cl-delete persp-nil-name
;;                                           (persp-names-current-frame-fast-ordered)
;;                                           :test 'string=)
;;                                (list persp-nil-name)))))
;;       (setq ibuffer-saved-filter-groups
;;             (cl-delete "persp-mode" ibuffer-saved-filter-groups
;;                        :test 'string= :key 'car))
;;       (push
;;        (cons "persp-mode" perspslist)
;;        ibuffer-saved-filter-groups)))

;;   (defun persp-ibuffer-visit-buffer ()
;;     (interactive)
;;     (let ((buf (ibuffer-current-buffer t))
;;           (persp-name (get-text-property
;;                        (line-beginning-position) 'ibuffer-filter-group)))
;;       (persp-switch persp-name)
;;       (switch-to-buffer buf)))

;;   (define-key ibuffer-mode-map (kbd "RET") 'persp-ibuffer-visit-buffer)

;;   (add-hook 'ibuffer-mode-hook
;;             #'(lambda ()
;;                 (persp-add-ibuffer-group)
;;                 (ibuffer-switch-to-saved-filter-groups "persp-mode"))))

;; (leaf perspective
;;   :init
;;   (persp-mode)
;;   ;; :custom
;;   ;; `(persp-mode-prefix-key . ,(kbd "C-c ."))
;;   :bind
;;   ("C-x C-b" . persp-list-buffers) ; or use a nicer switcher, see below
;;   ("C-c ." . perspective-map)
;;   (perspective-map
;;    ("S" . persp-state-save)
;;    ("M-s" . persp-state-save)
;;    ("C-s" . nil))
;;   :hook
;;   (kill-emacs-hook . persp-state-save)
;;   :config
;;   ;; default backup file
;;   (setq persp-state-default-file
;;         (file-name-concat persp-save-dir "persp-auto-save"))
;;   ;; prev/next buffers
;;   (setq switch-to-prev-buffer-skip
;;         (lambda (win buff bury-or-kill)
;;           (not (persp-is-current-buffer buff))))
;;   ;; consult-buffer
;;   (with-eval-after-load 'consult
;;     (setq +persp-consult-source
;;           '(:name "Perspective"
;;                   :narrow 115           ; ?s
;;                   :category buffer
;;                   :state consult--buffer-state
;;                   :history buffer-name-history
;;                   :default t
;;                   :items #[0 "\300\301\302\303\304\305\306&\207"
;;                              [consult--buffer-query
;;                               :sort visibility
;;                               :predicate (lambda (buf)
;;                                            (persp-is-current-buffer buf t))
;;                               :as buffer-name]
;;                              7]))
;;     (consult-customize consult--source-buffer :hidden t :default nil)
;;     (add-to-list 'consult-buffer-sources +persp-consult-source)))

(provide 'my-workspaces)
```



---

*Last updated: { git_revision_date_localized }*
