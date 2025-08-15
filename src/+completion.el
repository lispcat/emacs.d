;; ? : corfu, kind-icon, wgrep?, consult-dir, cape
;; ^ more at ~/code/cloned/daviwil-dots/.emacs.d/modules/dw-interface.el
;; TODO: vim keybinds for vertico completion shit (work on later) (also daviwil)
;;
;; a framework for minibuffer completion
;; (https://github.com/minad/vertico)

(leaf vertico
  :init
  (vertico-mode 1)
  ;; :setq
  ;; (vertico-scroll-margin . 0) ; Different scroll margin
  ;; (vertico-count . 20) ; Show more candidates
  ;; (vertico-resize . t) ; Grow and shrink the Vertico minibuffer
  ;; (vertico-cycle . t) ; Enable cycling for `vertico-next/previous'
  )

;; A few more useful configurations...
(leaf emacs :ensure nil
  :init
  ;; Support opening new minibuffers from inside existing minibuffers.
  (setq enable-recursive-minibuffers t)
  ;;
  ;; Emacs 28 and newer: hide commands in M-x that do not work in the current mode.
  ;; (setq read-extended-command-predicate #'command-completion-default-include-p)
  ;;
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
  ;;
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(leaf consult
  :bind (;; generic binds
         ("C-s" . consult-line)

         ;; C-c bindings in `mode-specific-map'
         ("C-c M-x" . consult-mode-command)
         ;; ("C-c )" . consult-kmacro)

         ;; C-x bindings in `ctl-x-map'
         ("C-x M-:" . consult-complex-command) ;; repeat-complex-command
         ("C-x b" . consult-buffer)            ;; switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame) ;; switch-to-buffer-other-frame
         ("C-x t b" . consult-buffer-other-tab) ;; switch-to-buffer-other-tab
         ("C-x r b" . consult-bookmark)         ;; bookmark-jump
         ("C-x p b" . consult-project-buffer) ;; project-switch-to-buffer
         ("C-x p C-b" . consult-project-buffer) ;; project-switch-to-buffer

         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-store)
         ;; ("C-M-#" . consult-register)
         ("C-M-#" . consult-register-load)

         ;; Other custom bindings
         ("M-y" . consult-yank-pop) ;; yank-pop
         ([remap Info-search] . consult-info)

         ;; M-g bindings in `goto-map'
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)   ;; goto-line
         ("M-g M-g" . consult-goto-line) ;; goto-line
         ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ("M-g O" . consult-org-heading)

         ;; M-s bindings in `search-map'
         ("M-s d" . consult-find) ;; Alternative: consult-fd
         ("M-s c" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ("M-s M" . consult-man)        ; T for terminal
         ("M-s I" . consult-info)

         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         (isearch-mode-map
          ("M-e" . consult-isearch-history)   ;; isearch-edit-string
          ("M-s e" . consult-isearch-history) ;; isearch-edit-string
          ("M-s l" . consult-line) ;; Needed by: consult-line to detect isearch
          ("M-s L" . consult-line-multi)) ;; Needed by: consult-line to detect isearch

         ;; Minibuffer history
         (minibuffer-local-map
          ("M-s" . consult-history) ;; next-matching-history-element
          ("M-r" . consult-history)) ;; previous-matching-history-element
         )
  :init
  (leader-key
    "s" search-map
    "Tt" 'consult-theme
    "bb" 'consult-buffer
    "fr" 'consult-recent-file
    "fm" 'consult-bookmark))

;; used to go to a file in a bookmarked dir n stuff (one ex)
(leaf consult-dir
  :init
  (leader-key
    "fd" 'consult-dir)
  :bind (("C-x C-d" . consult-dir)      ; default?
         (vertico-map
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file)))
  ;; :custom
  ;; (consult-dir-project-list-function nil)
  )

;; TODO: do i even need to do this here?
;; - oh wait i do since the other module might overwrite...
;; - but the issue is that it never gets set if those modules
;; are never loaded...
;; - maybe in the other module files, only set those functions
;; if another bind isnt already there?
;; - is it possible to do eval-after-load 'thing OR after init?
;; and throw away the other autoload once one succeeds?

;; (defmacro mi/eval-now-and-after-load (feature &rest body)
;;   "Eval BODY, then if FEATURE is not loaded, eval BODY again after FEATURE loaded."
;;   (declare (indent defun))
;;   (let ((f (cadr feature)))
;;     `(progn
;;        ;; always eval now
;;        ,@body
;;        ;; if feature not loaded, eval again after load feature
;;        ,(unless (featurep f)
;;           `(eval-after-load ',f
;;              (lambda () ,@body))))))

(leaf embark
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ;; ("C-h B" . embark-bindings)
   )
  :init
  ;; use embark for showing command prefix help
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(leaf embark-consult
  :after embark consult
  :hook (embark-collect-mode-hook . consult-preview-at-point-mode))

(leaf orderless
  :require t
  :setq
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers . '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator . #'orderless-escapable-split-on-space)
  (completion-styles . '(orderless basic))
  (completion-category-defaults . nil)
  (completion-category-overrides . '((file (styles partial-completion)))))

(leaf marginalia
  :init
  (marginalia-mode 1)
  :bind ((minibuffer-local-map
          ("M-A" . marginalia-cycle))
         (completion-list-mode-map
          ("M-A" . marginalia-cycle))))

;; TODO: disable most backends by default add a bunch per mode (org should only have a few
(leaf company
  ;; :disabled t
  :require t
  :bind
  (company-active-map
   ("<return>" . nil)
   ("C-n" . nil)
   ("C-p" . nil)
   ("C-s" . company-filter-candidates))

  :config
  (company-tng-configure-default)
  (global-company-mode 1)

  (defun +company-return-default-or-complete ()
    (interactive)
    ;; number if selected, nil if not
    (if company-selection
        (company-complete-selection)
      (company-abort)
      (execute-kbd-macro (kbd "<return>"))))
  (define-key company-tng-map (kbd "<return>") #'+company-return-default-or-complete)

  (setq company-backends
        '(company-dabbrev company-files)) ; the default, overrides below
  (setq company-transformers nil)
  (setq lsp-completion-provider :none)
  (setq company-idle-delay 0.1)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-dabbrev-downcase nil)
  (setq company-search-regexp-function 'company-search-words-in-any-order-regexp)

  ;; org-mode-specific backends

  (add-hook 'prog-mode-hook
            (lambda ()
              (setq-local company-backends
                          '((company-yasnippet :with company-capf)
                            company-dabbrev-code
                            company-files))
              (setq-local company-transformers '(company-sort-by-backend-importance))))

  (eval-after-load 'org
    '(add-hook 'org-mode-hook
               (lambda ()
                 (setq-local company-backends
                             '((company-dabbrev :with company-files))))))
  (eval-after-load 'latex
    '(add-hook 'LaTeX-mode-hook
               (lambda ()
                 (setq-local company-backends'nil))))

  ;; separator for orderless completion:

  (defvar +company-separator "&")

  (defun +company-insert-separator ()
    "Insert `+company-separator' during company completion."
    (interactive)
    (when (company-manual-begin)
      (insert +company-separator)))

  (define-key company-active-map (kbd "M-SPC") #'+company-insert-separator)

  (setq orderless-component-separator "[ &]")
  )

(leaf company-quickhelp
  :after company
  :bind ("C-c l h c" . company-quickhelp-mode)
  :setq
  (company-quickhelp-delay . 1)
  :config
  (company-quickhelp-mode 1))

;; TODO: this is set up for eglot only, not lsp-mode

;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot

(leaf yasnippet :ensure yasnippet-snippets
  :commands yas-reload-all
  :hook (prog-mode-hook . yas-minor-mode)
  :bind
  (yas-keymap
   ("RET" . yas-next-field-or-maybe-expand))
  :config
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "no-search/snippets"
                                 +emacs-src-dir))
  (yas-reload-all))

(global-set-key [remap dabbrev-expand] 'hippie-expand)
(add-to-list 'hippie-expand-try-functions-list #'yas-hippie-try-expand t)

(leaf isearch :ensure nil
  :bind
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

;;; CULPRIT OF HANGING, DISABLED.

;; (leaf corfu
;;   :require t
;;   :setq
;;   (corfu-cycle . t)        ;; Enable cycling through candidates
;;   (corfu-auto . t)         ;; Enable auto completion
;;   (corfu-auto-prefix . 1)  ;; Complete after typing 2 characters
;;   (corfu-auto-delay . 0.1) ;; Wait time before showing completions
;;   (corfu-preview-current . 'insert) ;; Preview first candidate
;;   (corfu-preselect . 'prompt)       ;; Preselect the prompt
;;   (corfu-on-exact-match . nil) ;; Don't auto-complete exact matches

;;   ;; Hide commands in M-x which do not apply to the current mode.  Corfu
;;   ;; commands are hidden, since they are not used via M-x. This setting is
;;   ;; useful beyond Corfu.
;;   (read-extended-command-predicate . #'command-completion-default-include-p)

;;   :bind (corfu-map
;;          ("TAB" . corfu-next)
;;          ([tab] . corfu-next)
;;          ("S-TAB" . corfu-previous)
;;          ([backtab] . corfu-previous)
;;          ("RET" . nil)
;;          ("C-n" . nil)
;;          ("C-p" . nil)
;;          ("C-RET" . corfu-insert))
;;   :init
;;   (global-corfu-mode))

;; (leaf cape
;;   ;; :disabled t
;;   :require t
;;   ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
;;   ;; Press C-c p ? to for help.
;;   :bind ("M-+" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
;;   ;; Alternatively bind Cape commands individually.
;;   ;; :bind (("C-c p d" . cape-dabbrev)
;;   ;;        ("C-c p h" . cape-history)
;;   ;;        ("C-c p f" . cape-file)
;;   ;;        ...)
;;   :init
;;   ;; Add to the global default value of `completion-at-point-functions' which is
;;   ;; used by `completion-at-point'.  The order of the functions matters, the
;;   ;; first function returning a result wins.  Note that the list of buffer-local
;;   ;; completion functions takes precedence over the global list.

;;   (add-hook 'completion-at-point-functions #'cape-dabbrev) ; current buffers
;;   (add-hook 'completion-at-point-functions #'cape-file)    ; file name
;;   ;; (add-hook 'completion-at-point-functions (cape-company-to-capf 'company-yasnippet))    ; file name
;;   ;; (add-hook 'completion-at-point-functions #'cape-elisp-block) ; code block (THE CULPRIT!!!!!)

;;   )

;; (leaf yasnippet-capf
;;   :after cape
;;   :config
;;   (defun +capfs-add-yasnippet ()
;;     "Add yasnippet-capf to the front of completion-at-point-functions."
;;     ;; (add-to-list 'completion-at-point-functions #'yasnippet-capf)
;;     (setq-local completion-at-point-functions
;;                 (cons #'yasnippet-capf
;;                       completion-at-point-functions))
;;     )
;;   :hook (prog-mode-hook . +capfs-add-yasnippet))

;; Configure Tempel
;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "<")

;;   :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
;;          ("M-*" . tempel-insert))

;;   :init

;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-insert
;;                       completion-at-point-functions)))

;;   (add-hook 'conf-mode-hook 'tempel-setup-capf)
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf)

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   ;; (global-tempel-abbrev-mode)
;;   )

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
;; (use-package tempel-collection)

;; (leaf abbrev :ensure nil
;;   :bind (("C-c c a" . add-global-abbrev)
;;          ("C-c c -" . inverse-add-global-abbrev)
;;          ("C-c c e" . edit-abbrevs)))

(provide '+completion)
