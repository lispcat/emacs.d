

;;; Code:

;; https://github.com/Bad-ptr/persp-mode.el

(leaf persp-mode
  :bind-keymap
  ("C-c w w" . persp-key-map)
  ("C-c ." . persp-key-map)
  ("C-c (" . persp-key-map)
  :setq
  (wg-morph-on . nil)
  (persp-autokill-buffer-on-remove . 'kill-weak)
  (persp-auto-resume-time . 1)
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
    (list :name     "Perspective"
          :narrow   ?s
          :category 'buffer
          :state    #'consult--buffer-state
          :history  'buffer-name-history
          :default  t
          :items
          (lambda ()
            (let ((curr-persp (get-current-persp)))
              (consult--buffer-query
               :sort 'visibility
               :predicate (lambda (buf)
                            (if curr-persp
                                (persp-contain-buffer-p buf)
                              t))
               :as 'buffer-name)))))
  (with-eval-after-load 'consult
    (consult-customize consult--source-buffer :hidden t :default nil)
    (add-to-list 'consult-buffer-sources persp-consult-source)))

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
;;     (setq my/persp-consult-source
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
;;     (add-to-list 'consult-buffer-sources my/persp-consult-source)))


(provide 'my-persp)
