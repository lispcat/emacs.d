;; NOTE: modify #'persp-save-state-to-file arg (keep-others-in-non-parametric-file 'yes)

;; maybe have each persp have its own save file, and when autosaving, save each persp?
;; maybe have a function to delete a persp from the main autosave file?
;; - prompt available perspectives from main autosave file, after selection,
;; delete each from file.


;;; Activities

(leaf activities
  :custom
  ;; only show tab bar if more than 3 activities open
  (tab-bar-show . 3)
  :init
  (activities-mode)
  (activities-tabs-mode)
  ;; prevent edebug default bindings from interfering
  ;; (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  ;; create map
  (defun +activities-define-existing ()
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'activities-define)))

  (defvar activities-mode-map
    (let ((map (make-sparse-keymap)))
      ;; Define keys in bulk
      (dolist
          (binding
           '(;; create new activity name.
             ("N" . activities-new)
             ;; define new activity's default with current frame state.
             ;; (prefix) define pre-existing activity' default with current frame state.
             ("d" . +activities-define-existing)
             ;; resume suspended activity
             ;; (prefix) resume activity with default state.
             ("." . +activities-resume-custom)
             ("s" . +activities-resume-custom)
             ;; save and close activity.
             ;; ("k" . activities-suspend)
             ;; reset to default state and close activity.
             ("c" . activities-kill)
             ;; switch to an opened activity
             ;; ("s" . activities-switch)
             ;; permanently delete activity
             ("k" . activities-discard)
             ;; switch to a buffer in the current activity.
             ("b" . activities-switch-buffer)
             ;; revert activity to default state.
             ("g" . activities-revert)
             ;; list activities in vtable buffer
             ("l" . activities-list)
             ;; rename activity
             ("R" . activities-rename)
             ;; next tab
             ("n" . tab-next)
             ;; previous tab
             ("p" . tab-previous)))
        (define-key map (kbd (car binding)) (cdr binding)))
      ;; set up autoloads
      (let ((cmds (mapcar #'cdr (cdr map))))
        (dolist (c cmds)
          (unless (fboundp c)
            (autoload c "activities" nil t))))
      ;; return map
      map))

  ;; bind map
  (global-set-key (kbd "C-c .") activities-mode-map)
  (global-set-key (kbd "C-c x") activities-mode-map)

;;;; consult integration

  (defun activities-local-buffer-p (buffer)
    "Returns non-nil if BUFFER is present in `activities-current'."
    (when (activities-current)
      (memq buffer
            (activities-tabs--tab-parameter
             'activities-buffer-list
             (activities-tabs--tab (activities-current))))))

  (defvar activities-consult-source
    `(:name "Activity"
            :narrow   ?a
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :default  t
            :enabled  ,#'activities-current
            :items ,(lambda () (consult--buffer-query
                           :predicate #'activities-local-buffer-p
                           :sort 'visibility
                           :as #'buffer-name)))
    "Activities local buffers candidate source for `consult-buffer'.")

  (defvar activities-rest-consult-source
    `(:name "Other"
            :narrow   ?o
            :category buffer
            :face     consult-buffer
            :history  buffer-name-history
            :state    ,#'consult--buffer-state
            :enabled  ,#'activities-current
            :items ,(lambda () (consult--buffer-query
                           :predicate (lambda (buf)
                                        (not (activities-local-buffer-p buf)))
                           :as #'buffer-name)))
    "Other buffers candidate source for `consult-buffer'.")

  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden nil :default nil
                       :name "Buffers" :narrow ?b
                       :enabled (lambda () (not (activities-current))))
    (add-to-list 'consult-buffer-sources activities-rest-consult-source)
    (add-to-list 'consult-buffer-sources activities-consult-source))

;;;;; fix: consult-buffer previews

  (with-eval-after-load 'consult
    (defun +consult-buffer--frame-buffers-around (orig-fn &rest args)
      (let ((_log t)
            (result nil))
        (if-let* ((_predicates (and activities-mode
                                    activities-tabs-mode
                                    tab-bar-mode
                                    (activities-current)))
                  (current-tab (tab-bar--current-tab))
                  (tab-index (tab-bar--current-tab-index))
                  (init-tab-buffers (alist-get 'activities-buffer-list current-tab)))
            ;; if-let then form
            (unwind-protect
                (setq result (apply orig-fn args))
              ;; re-fetch current tab after consult-buffer
              (let* ((updated-tab (tab-bar--current-tab))
                     (post-tab-buffers
                      (seq-filter #'buffer-live-p
                                  (alist-get 'activities-buffer-list updated-tab)))
                     (current-buffer (current-buffer))
                     ;; remove new temp buffers from post-tab-buffers
                     (filtered-new
                      (seq-filter (lambda (buf)
                                    (and buf
                                         (or (member buf init-tab-buffers)
                                             (eq buf current-buffer)
                                             (equal (buffer-name buf)
                                                    " *Minibuf-1*"))))
                                  post-tab-buffers)))
                ;; debugging
                (when _log
                  (message "LOG: activities: ignore consult-buffer previews: %S"
                           (mapcar #'buffer-name
                                   (seq-remove (lambda (x) (member x filtered-new))
                                               post-tab-buffers))))
                ;; update the tab's activities-buffer-list
                (let ((tabs (funcall tab-bar-tabs-function)))
                  (setf (alist-get 'activities-buffer-list (nth tab-index tabs))
                        filtered-new))

                result))
          ;; if-let else form
          (apply orig-fn args))))

    (advice-add #'consult-buffer :around #'+consult-buffer--frame-buffers-around))

;;;;; feat: frame restore (disabled)

  ;; ;; helper variable
  ;; (defvar +activities--last-name nil)

  ;; ;; save activities before deleting frame
  ;; (add-to-list 'delete-frame-functions
  ;;              (lambda (frame)
  ;;                (setq +activities--last-name (activities-current))
  ;;                (activities-save-all)))
  ;; ;; save activities before killing emacs
  ;; (add-hook 'kill-emacs-hook #'activities-save-all)

  ;; ;; resume last activity when creating frame
  ;; (add-hook 'after-make-frame-functions
  ;;           (lambda (frame)
  ;;             (when +activities--last-name
  ;;               (with-selected-frame frame
  ;;                 (activities-resume +activities--last-name)))))

;;;;; feat: resume-custom

  (cl-defun +activities-resume-custom (activity &key resetp)
    "Wrapper around `activities-resume'.

When prompted, it excludes activities that are active in other frames.
After evaluating, it suspends all non-current activities."
    (interactive
     (list (activities-completing-read
            :activities (+activities--exclude-other-frames)
            :prompt "Resume activity" :default nil)
           :resetp current-prefix-arg))
    (when-let* ((current (activities-current)))
      (activities-suspend (activities-current)))
    (let ((result (apply #'activities-resume
                         activity
                         (when resetp (list :resetp resetp)))))
      ;; after apply
      (+activities--suspend-not-cur)
      result))

  ;; helper function
  (defun +activities--exclude-other-frames ()
    "Return loadable activities, excluding already active in other frames."
    (let* ((log nil)
           ;; activities loadable from file
           (activities-loadable (-map #'car-safe activities-activities))
           ;; activities active in any frame except current
           (activities-active-in-other
            (->>
             (frame-list)
             (--remove (equal it (selected-frame)))
             (-mapcat
              (lambda (frame)
                (with-selected-frame frame
                  (let ((active-names
                         (->>
                          activities-activities
                          (-filter (-compose #'activities-activity-active-p
                                             #'cdr))
                          (-map #'car-safe))))
                    (prog1 active-names
                      (when log
                        (message "LOG: active-names: %S" active-names))
                      (unless (or (<= 1 (length active-names))
                                  (not active-names))
                        (warn "Expected no more than 1 activity in frame %s: %s"
                              frame active-names)))))))
             (-non-nil)
             (-uniq))))
      ;; from loadable, remove active-in-other
      (when log
        (message "LOG: activities-loadable: %S" activities-loadable)
        (message "LOG: activities-active-in-other: %S" activities-active-in-other))
      (->> activities-loadable
           (--remove (-contains? activities-active-in-other
                                 it))
           (--map (cons it (activities-named it))))))

  ;; helper function
  (cl-defun +activities--suspend-not-cur (&rest _args)
    "Suspend all non-current activities."
    (when-let*
        ((fn-get-active-lst
          (lambda () (->> activities-activities
                     (-filter (-compose #'activities-activity-active-p #'cdr))
                     (-map #'car-safe))))
         (active-lst (funcall fn-get-active-lst))
         (current (activities-activity-name (activities-current)))
         (not-current-lst
          (->> active-lst (--remove (equal current it)))))
      ;; suspend all non-current activities
      (--each not-current-lst
        (activities-suspend (activities-named it)))
      ;; assert expected length and current-value
      (setq active-lst (funcall fn-get-active-lst))
      (unless (<= 1 (length active-lst))
        (warn "expected 1 or 0 length for active-lst: %s"
              active-lst)
        (when (and (= 1 (length active-lst))
                   (equal current (car active-lst)))
          (warn "expected 1 elem of %s, got %S"
                current (car active-lst))))))

;;;;; misc

  )

;;; Persp-mode

(leaf persp-mode :disabled t
  :bind-keymap
  ("C-c w w" . persp-key-map)
  ("C-c ." . persp-key-map)
  ("C-c (" . persp-key-map)
  :bind (persp-key-map
         ("." . my-persp-load-name-from-latest)
         ("D" . my-persp-delete-name-from-latest))
  :setq
  ;; animation
  (wg-morph-on . nil)
  ;; ?
  (persp-autokill-buffer-on-remove . 'kill-weak)
  ;; dont autoresume at startup
  (persp-auto-resume-time . -1)
  ;; save on shutdown
  (persp-auto-save-opt . 2)

  :hook
  (elpaca-after-init-hook . (lambda () (persp-mode 1)))

  :commands
  persp-consult-source

  :config
  ;; dont save persp-nil to file
  (set-persp-parameter 'dont-save-to-file t nil)
  ;; consult-buffer integration
  (defvar persp-consult-source
    (list :name     "Persp Buffers"
          :narrow   ?.
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


  ;; helper functions
  (defun my/persp--get-names-from-savelist (&optional fname savelist)
    (let* ((available (persp-list-persp-names-in-file fname savelist))
           (loaded (persp-names-current-frame-fast-ordered))
           (unloaded (seq-remove (lambda (p) (member p loaded)) available)))
      (list available loaded unloaded)))

  (defun my/persp--prompt-for-persp-name (lst)
    (when lst
      (persp-read-persp
       "to load" nil nil t t nil lst t 'push)))

  ;; TODO: relocate
  (defmacro +message-if-debug (format-string &rest args)
    (when debug-on-error
      `(message ,format-string ,@args)))

  ;; load from file
  (cl-defun my-persp-load-name-from-latest
      (&optional (fname persp-auto-save-fname)
                 (phash *persp-hash*)
                 (savelist (persp-savelist-from-savefile fname))
                 name)
    "Load and switch to a perspective via name from the latest backup file."
    (interactive)
    ;; prompt for name from available
    (when savelist
      (cl-destructuring-bind (available loaded unloaded)
          (my/persp--get-names-from-savelist fname savelist)
        (when unloaded
          (unless name
            (setq name (my/persp--prompt-for-persp-name unloaded)))
          (+message-if-debug "DEBUG: > %s\n> %s\n> %s\n> %s"
                             fname
                             phash
                             (regexp-opt (list name))
                             savelist)
          (persp-load-state-from-file fname phash
                                      (regexp-opt (list name))
                                      t savelist)
          ;; (persp-frame-switch name)
          )))

    ;; (when name
    ;;   (let ((names-regexp (regexp-opt (list name))))
    ;;     (persp-load-state-from-file fname phash names-regexp t savelist))
    ;;   ;; switch to new loaded persp
    ;;   (persp-frame-switch name))
    )

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

;;; Perspective

(leaf perspective :disabled t
  :init
  (persp-mode)
  :custom
  `(persp-mode-prefix-key . ,(kbd "C-c ."))
  :bind
  ("C-c ." . perspective-map)
  (perspective-map
   ("S" . persp-state-save)
   ("M-s" . persp-state-save)
   ("C-s" . nil))
  :hook
  (kill-emacs-hook . persp-state-save)
  :config
  ;; default backup file
  (setq persp-state-default-file
        (file-name-concat persp-save-dir "persp-auto-save"))
  ;; prev/next buffers
  (setq switch-to-prev-buffer-skip
        (lambda (win buff bury-or-kill)
          (not (persp-is-current-buffer buff))))
  ;; consult-buffer
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden nil :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source))

  ;; save activities before deleting frame
  (add-to-list 'delete-frame-functions
               (lambda (frame)
                 (persp-save-state)))
  ;; save activities before killing emacs
  (add-hook 'kill-emacs-hook #'persp-save-state)
  )

(provide '+workspaces)
