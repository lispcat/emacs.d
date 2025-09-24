;;; +completion.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2025  lispcat

;; Author: lispcat <187922791+lispcat@users.noreply.github.com>
;; Keywords: local

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains configuration for Emacs' completion.

;; It uses the Vertico completion framework.

;;; Code:

;; TODO: wgrep ?

;;; Vertico

(-setup vertico
  (:option vertico-scroll-margin 3     ; Different scroll margin (def 2)
           vertico-count 10            ; Show more candidates (def 10)
           vertico-resize 'grow-only   ; Grow/shrink minibuffer (def 'grow-only)
           vertico-cycle t)      ; cycle for `vertico-next/previous' (def nil)
  (:option
   ;; adds a menu in the minibuffer to switch display modes
   context-menu-mode t

   ;; Support opening new minibuffers from inside existing minibuffers.
   enable-recursive-minibuffers t

   ;; hide commands in M-x that do not work in the current mode.
   read-extended-command-predicate #'command-completion-default-include-p

   ;; do not allow the cursor in the minibuffer prompt
   minibuffer-prompt-properties '(read-only t cursor-intangible t
                                            face minibuffer-prompt))

  ;; enable
  (vertico-mode 1)

  (:global "C-M-c" #'completion-at-point)

  ;; prefix TAB completion
  (keymap-set vertico-map "<backtab>" #'minibuffer-complete)

  ;; Prompt indicator for `completing-read-multiple'.
  (when (< emacs-major-version 31)
    (advice-add #'completing-read-multiple :filter-args
                (lambda (args)
                  (cons (format "[CRM%s] %s"
                                (string-replace "[ \t]*" "" crm-separator)
                                (car args))
                        (cdr args)))))

;;;; vertico - multiform mode

  ;; enable multiform
  (vertico-multiform-mode)

  ;; per command; form: ((cmd config..) ..)
  (setq vertico-multiform-commands
        `((consult-imenu buffer)
          (consult-outline buffer)))

  ;; per completion category
  (setq vertico-multiform-categories
        `((reverse (vertico-resize . 'grow-only)))))

;;; Orderless

(-setup orderless
  (:option completion-styles '(orderless basic)
           completion-category-defaults nil
           completion-category-overrides '((file (styles partial-completion)))
           ;; TODO: Configure a custom style dispatcher (see the Consult wiki)
           ;; orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
           ;; orderless-component-separator #'orderless-escapable-split-on-space
           ))

;;; Marginalia

(-setup marginalia
  (marginalia-mode 1)
  (:with-map minibuffer-local-map
    (:bind "M-A" marginalia-cycle))
  (:with-map completion-list-mode-map
    (:bind "M-A" marginalia-cycle)))

;;; Consult

;; Search and navigation commands

;; ---

(-setup consult

  ;; Keybinds:

  (:global
   ;; recent files
   "C-c f r" consult-recent-file        ; recent file

   ;; buffers
   "C-c b b" consult-buffer             ; switch-to-buffer
   "C-x 4 b" consult-buffer-other-window
   "C-x 5 b" consult-buffer-other-frame

   ;; editing
   "M-y" consult-yank-pop               ; fallback to after yank
   "M-S-y" consult-yank-from-kill-ring
   "<f5>" consult-kmacro

   ;; register (C-u for window layout!)
   "M-#" consult-register-store         ; store
   "C-M-#" consult-register-load        ; load

   ;; navigation
   "C-c s a" consult-org-agenda         ; agenda heading

   "C-c s l" consult-imenu              ; list var/func
   "C-c s L" consult-imenu-multi

   ;; search
   "C-s" consult-line
   "C-S-s" consult-line-multi
   "C-c s k" consult-keep-lines  ;; keep
   "C-c s K" consult-focus-lines ;; focus (reset: C-u) (! invert)

   ;; grep (in files)
   "C-c s r" consult-ripgrep

   ;; find (filename)
   "C-c s f" consult-fd
   "C-c s F" consult-locate

   ;; histories
   "C-c s x" consult-complex-command    ; past keychord

   ;; themes
   "C-c T t" consult-theme

   ;; list minor modes
   "C-c s m" consult-minor-mode-menu
   "C-c s M-x" consult-mode-command     ; M-x only for current mode
   "C-c M-x" consult-mode-command

   ;; Info
   "C-c s I" consult-info
   [remap Info-search] consult-info

   ;; Man pages
   ;; "C-c s M" consult-man ; replaced with tldr
   )

  ;; outline navigation

  (defun +consult-outline-or-org-heading ()
    (interactive)
    (if (derived-mode-p 'org-mode)
        (call-interactively #'consult-org-heading)
      (call-interactively #'consult-outline)))

  (:global "C-c o g" +consult-outline-or-org-heading)
  ;; "C-c ; l" consult-outline            ; outline heading
  ;; "C-c o l" consult-org-heading        ; org heading


  ;; project.el integration
  ;; moved to project.el
  ;; (with-eval-after-load 'project
  ;;   (:with-map project-prefix-map
  ;;     (:bind "b" consult-project-buffer
  ;;            "s" consult-ripgrep
  ;;            "S" project-shell)))

  ;; isearch integration
  (:with-map isearch-mode-map
    (:bind "M-r" consult-isearch-history))

  ;; lsp-mode
  (:with-feature lsp-mode
    (:with-map lsp-mode-map
      (:bind "C-c l e" consult-compile-error)))

  ;; minibuffer history
  (:with-map minibuffer-local-map
    (:bind "M-r" consult-history))

  ;; Settings:

  (:when-loaded
    ;; key for narrowing
    (setq consult-narrow-key "<")

    ;; improve register preview
    (advice-add #'register-preview :override #'consult-register-window)
    (setq register-preview-delay 0.5)

    ;; xref previews
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref)

    ;; info search groups
    (consult-info-define 'elisp "emacs" "elisp" "dash" "cl")
    (consult-info-define 'org "org" "org-ql" "org-super-agenda")
    (consult-info-define "magit")

    ;; delay previews
    (setq consult-preview-key
          '(:debounce 0.05 any))

    ;; delay previews - custom length
    (consult-customize
     consult-man
     consult-bookmark
     consult-xref
     :preview-key '(:debounce 0.1 any))

    (consult-customize
     consult-recent-file
     consult--source-recent-file
     consult--source-project-recent-file
     :preview-key '(:debounce 0.3 any))

    (consult-customize
     consult-ripgrep
     consult-git-grep
     consult-grep
     :preview-key '(:debounce 0.5 any))

    ;; allow these modes in previews
    (add-to-list 'consult-preview-allowed-hooks 'hl-todo-mode)
    (add-to-list 'consult-preview-allowed-hooks 'global-hl-todo-mode)
    (add-to-list 'consult-preview-allowed-hooks 'elide-head-mode)
    (add-to-list 'consult-preview-allowed-hooks 'global-org-modern-mode)

    ;; async search separator (for two-level filtering)
    (setq consult-async-split-style
          ;; 'perl
          'semicolon
          )

    ;; tweak consult-buffer
    (setq consult-buffer-sources
          (->> consult-buffer-sources
               (delete 'consult--source-bookmark)
               ;; (delete 'consult--source-recent-file)
               ;; (delete 'consult--source-project-recent-file-hidden)
               ))

    ;; prolly not needed:
    ;; projectile.el (projectile-project-root)
    ;; (autoload 'projectile-project-root "projectile")
    ;; (setq consult-project-function (lambda (_) (projectile-project-root)))

    ;; bind
    ;; TODO: create dedicated function for simply creating a which-key named prefix
    (leader-bind
      "s" '(:ignore t :which-key "search"))))

;;;; consult-dir

;; used to go to a file in a bookmarked dir n stuff (one ex)
(-setup consult-dir
  (:global "C-c d d" #'consult-dir
           "C-c f d" #'consult-dir)
  ;; (leader-bind
  ;;   "dd" 'consult-dir
  ;;   "fd" 'consult-dir)
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

;;; Embark

;; Invoke relavent commands on the thing at point.
;; https://github.com/oantolin/embark

;; ---

(-setup embark
  ;; TODO: bind dwim and act (very useful) to more convenient keys.
  (:global "C-." +embark-act-or-dwim
           "C-," embark-dwim)

  ;; use embark for showing command prefix help
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  ;; custom wrapper around embark-act to support dwim
  (defun +embark-act-or-dwim (arg)
    (interactive "P")
    (if current-prefix-arg
        (embark-dwim)
      (embark-act)))

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;;;; embark - consult integration

;; Adds consult-specific embark actions and the Occur buffer export.

;; `embark-export' ("E") is extremely useful. Given a set of candidates, you can
;; export them to a specialized buffer, unlocking more advanced and efficient
;; editing workflows.

;; --

(-setup embark-consult
  (:load-after embark consult)
  (:when-loaded
    (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode)))

;;; Company

;; TODO: disable most backends by default add a bunch per mode (org should only
;; have a few
(progn
  ;; (-setup company

  ;;   ;; keybinds

  ;;   (:with-map company-active-map
  ;;     (:bind "<return>" nil
  ;;            "C-n" nil
  ;;            "C-p" nil
  ;;            "C-s" company-filter-candidates))

  ;;   ;; config

  ;;   (:when-loaded

  ;;     (company-tng-configure-default)
  ;;     (global-company-mode 1)

  ;;     (defun +company-return-default-or-complete ()
  ;;       (interactive)
  ;;       ;; number if selected, nil if not
  ;;       (if company-selection
  ;;           (company-complete-selection)
  ;;         (company-abort)
  ;;         (execute-kbd-macro (kbd "<return>"))))
  ;;     (define-key company-tng-map (kbd "<return>") #'+company-return-default-or-complete)

  ;;     (setq company-backends
  ;;           '(company-dabbrev company-files)) ; the default, overrides below
  ;;     (setq company-transformers nil)
  ;;     (setq lsp-completion-provider :none)
  ;;     (setq company-idle-delay 0.1)
  ;;     (setq company-selection-wrap-around t)
  ;;     (setq company-minimum-prefix-length 1)
  ;;     (setq company-dabbrev-downcase nil)
  ;;     (setq company-search-regexp-function 'company-search-words-in-any-order-regexp)

  ;;     ;; org-mode-specific backends

  ;;     (add-hook 'prog-mode-hook
  ;;               (lambda ()
  ;;                 (setq-local company-backends
  ;;                             '((company-yasnippet :with company-capf)
  ;;                               company-dabbrev-code
  ;;                               company-files))
  ;;                 (setq-local company-transformers '(company-sort-by-backend-importance))))

  ;;     (eval-after-load 'org
  ;;       '(add-hook 'org-mode-hook
  ;;                  (lambda ()
  ;;                    (setq-local company-backends
  ;;                                '((company-dabbrev :with company-files))))))
  ;;     (eval-after-load 'latex
  ;;       '(add-hook 'LaTeX-mode-hook
  ;;                  (lambda ()
  ;;                    (setq-local company-backends'nil))))

  ;;     ;; separator for orderless completion:

  ;;     (defvar +company-separator "&")

  ;;     (defun +company-insert-separator ()
  ;;       "Insert `+company-separator' during company completion."
  ;;       (interactive)
  ;;       (when (company-manual-begin)
  ;;         (insert +company-separator)))

  ;;     (define-key company-active-map (kbd "M-SPC") #'+company-insert-separator)

  ;;     (setq orderless-component-separator "[ &]")
  ;;     ))
  )

;; (-setup company-quickhelp
;;   (:load-after company)
;;   (:global "C-c l h c" company-quickhelp-mode)
;;   (:option company-quickhelp-delay 1)
;;   (:when-loaded
;;     (company-quickhelp-mode 1)))


;;; Yasnippet

;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot

(-setup yasnippet-snippets)

(-setup yasnippet
  (:diminish yas-minor-mode)
  (add-hook 'prog-mode-hook #'yas-minor-mode)

  (:require-self)

  (:when-loaded

    ;; binds
    (:with-map yas-keymap
      (:bind "RET" yas-next-field-or-maybe-expand))

    ;; load snippets
    (:also-load yasnippet-snippets)

    ;; set custom snippets dirs
    (add-to-list 'yas-snippet-dirs
                 (expand-file-name "no-search/snippets"
                                   +emacs-src-dir))

    ;; reload snippets
    (with-eval-after-load 'yasnippet-snippets
      (yas-reload-all))))

(setup hippie-exp
  ;; replace dabbrev-expand with hippie-expand
  (global-set-key [remap dabbrev-expand] 'hippie-expand)

  ;; add yas-hippie-try-expand after yasnippet loads
  (:with-feature yasnippet
    (:when-loaded
      (add-to-list 'hippie-expand-try-functions-list
                   #'yas-hippie-try-expand t)))

  (:with-feature tempel
    (:when-loaded
      (add-to-list 'hippie-expand-try-functions-list
                   #'tempel-expand t))))

(setup isearch
  (:when-loaded
    (:global "C-M-s" isearch-forward
             "C-M-r" isearch-backward)))

;;; Corfu

(-setup corfu
  (:require-self)
  (:option corfu-cycle t                ; cycle
           corfu-on-exact-match nil     ; on exact match, do nothing
           corfu-auto t                 ; auto popup
           corfu-preselect 'prompt      ; always insert candidate into buffer
           corfu-auto-delay 0.15)

  (:with-map corfu-map
    (:bind "TAB" #'corfu-next
           [tab] #'corfu-next
           "S-TAB" #'corfu-previous
           [backtab] #'corfu-previous

           ;; unbind C-n and C-p
           "C-n" nil
           "C-p" nil
           [remap previous-line] nil
           [remap next-line] nil

           ;; avy-style select
           "M-;" #'corfu-quick-complete

           ;; prevent M-TAB from opening another completion
           ;; "M-TAB" nil

           ;; ;; make S-RET insert
           ;; "S-RET" #'corfu-insert

           ;; make RET do nothing
           ;; "RET" nil
           "C-<return>" nil

           ;; easier complete and expand appropriate
           ;; "C-<return>" #'corfu-complete
           ))

  ;; (:when-loaded
  ;;   (add-hook 'corfu-mode-hook))

  ;; enable
  (global-corfu-mode)

  ;; optional modes
  (corfu-history-mode)                  ; sort by recent and freq
  (corfu-popupinfo-mode)                ; show docs to the right

  ;; avy-style completion
  (:with-map corfu-map
    (:bind "M-;" #'corfu-quick-complete))
  (:with-feature corfu-quick
    (:option corfu-quick1 "aoeuidhtns"
             corfu-quick2 "aoeuidhtns"))

  (:option
   ;; cycle only if few candidates
   ;; FIX: disabled since tempel wont show previews
   completion-cycle-threshold 10
   ;; completion-cycle-threshold nil

   ;; make TAB indent or complete
   tab-always-indent 'complete

   ;; disable ispell completion function (`cape-dict' better)
   text-mode-ispell-word-completion nil

   ;; decrease time for corfu popupinfo
   corfu-popupinfo-delay '(1.5 . 0.5)))

;;;; candidate icons

;; (-setup nerd-icons-corfu
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(-setup kind-icon
  (:load-after corfu)
  (:when-loaded
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
    ;; blend background
    (:option kind-icon-blend-background t
             kind-icon-default-face 'corfu-default)
    ;; update background after theme change
    (add-hook '+after-enable-theme-hook #'kind-icon-reset-cache)))

;;;; yasnippet integration

;; Cause of freezing on lsp-mode?

;; Use cape to combine lsp-capf and yasnippet-capf

(-setup yasnippet-capf
  (:load-after yasnippet))

;;; Cape + Tempel

;; Notes: structure:
;; - defaults/fallbacks
;; - generic modes (prog, conf, text) - adds on top of defaults/fallbacks
;; - lsp - overwrites all above

;; TODO:
;; https://github.com/minad/corfu/wiki#same-key-used-for-both-the-separator-and-the-insertion
;; TODO: https://github.com/minad/corfu/wiki#tab-and-go-completion


;; ---

(-setup tempel
  (:global "M-*" #'tempel-insert)
  (:with-map tempel-map
    (:bind "C-M-RET" #'tempel-next
           "C-o"   #'tempel-next)))

(-setup tempel-collection
  (:load-after tempel))

(-setup cape
  (:require-self)
  (:also-load tempel tempel-collection)

  ;; test with #' prefix
  (:global "M-+" #'cape-prefix-map)

  ;; fixes ---

  (with-eval-after-load 'lsp-mode
    ;; FINALLY, the fix for lsp hanging????!!!!
    ;; https://github.com/emacs-lsp/lsp-mode/issues/3555#issuecomment-2830321073
    (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible))

  ;; manual template expansion ---

  (defun +template-expand ()
    (interactive)
    (cond
     ((not (null (tempel-expand)))
      (progn
        (message "Tempel snippet...")
        (call-interactively #'tempel-expand)))
     ((not (null (yas--templates-for-key-at-point)))
      (progn
        (message "Yasnippet snippet...")
        (call-interactively #'yas-expand)))
     (t
      (message "No snippets here"))))

  (:global "M-/" #'+template-expand
           "C-M-/" #'hippie-expand)

  ;; helper var ---

  ;; helper functions
  (defun +capf-prepend-local (capfs)
    "Prepend buffer-local capfs with CAPFS."
    (setq-local completion-at-point-functions
                (append capfs completion-at-point-functions)))

  (defun +capf-override-local (capfs)
    "Override buffer-local capfs with CAPFS."
    (setq-local completion-at-point-functions
                (append capfs '(t))))

  (defvar +capf-local-default nil
    "The buffer-local original value of the capf variable.")

  (cl-defmacro +capf-create-mode-setup
      (&key hook
            (func #'+capf-override-local)
            capfs
            after-load)
    (cl-check-type hook symbol)
    (cl-check-type func symbol)
    (cl-check-type capfs list)
    (let ((body
           `(progn
              ,(let ((new-defun-name
                      (intern
                       (format "+capf-%s-setup"
                               (symbol-name hook)))))
                 `(add-hook ',hook
                            (defun ,new-defun-name ()
                              (,func ,capfs)))))))
      (if after-load
          `(with-eval-after-load ',after-load
             ,body)
        body)))

  ;; tweaks ---

  (defun +capf-prefix-length-2-advice (orig-fun &rest args)
    (cape-wrap-prefix-length orig-fun 2))

  (defun +capf-prefix-length-5-advice (orig-fun &rest args)
    (cape-wrap-prefix-length orig-fun 5))

  (advice-add 'tempel-complete :around #'+capf-prefix-length-2-advice)
  (advice-add 'cape-keyword    :around #'+capf-prefix-length-2-advice)

  (advice-add 'cape-dabbrev    :around #'+capf-prefix-length-5-advice)

  ;; defining ---

  ;; global defaults (fallback always)
  (setq-default completion-at-point-functions
                (list #'cape-file
                      #'cape-dabbrev
                      ;; #'cape-history ;; annoying minibuffer history?
                      ))

  ;; derived-modes default (specific major-modes usually override)

  (dolist (hook '(prog-mode-hook conf-mode-hook text-mode-hook))
    (add-hook hook
              (defun +capf-derived-mode-setup ()
                (interactive)
                (when (local-variable-p 'completion-at-point-functions)
                  (setq-local +capf-local-default
                              (remove 't completion-at-point-functions))
                  (when debug-on-error
                    (message "LOG: capf-local-default: %s" +capf-local-default)))
                (+capf-prepend-local
                 (list (cape-capf-super #'yasnippet-capf ;; #'tempel-complete
                                        #'cape-keyword))))))

  ;; specific major-modes

  ;; elisp
  (+capf-create-mode-setup
   :hook emacs-lisp-mode-hook
   :capfs
   (list (cape-capf-super #'tempel-complete #'yasnippet-capf
                          #'cape-keyword
                          #'elisp-completion-at-point)))

  ;; eshell
  (+capf-create-mode-setup
   :hook eshell-mode-hook
   :capfs
   (list (cape-capf-super #'tempel-complete #'yasnippet-capf
                          #'cape-keyword
                          #'pcomplete-completions-at-point)))

  ;; lsp-mode
  (+capf-create-mode-setup
   :hook lsp-managed-mode-hook
   :after-load lsp-mode
   :capfs
   (list (cape-capf-super #'tempel-complete #'yasnippet-capf
                          (cape-capf-buster
                           #'lsp-completion-at-point))))

  ;; org
  (+capf-create-mode-setup
   :hook org-mode-hook
   :after-load org
   :capfs
   (list (cape-capf-super #'tempel-complete #'yasnippet-capf
                          #'cape-elisp-block
                          #'pcomplete-completions-at-point)
         #'cape-tex
         #'cape-rfc1345
         #'cape-emoji))

  ;; eglot-java-mode
  ;; (+capf-create-mode-setup
  ;;  :hook eglot-java-mode-hook
  ;;  :capfs
  ;;  (list #'eglot-completion-at-point
  ;;        ;; (cape-capf-super ;; #'tempel-complete #'yasnippet-capf
  ;;        ;;  ;; #'cape-elisp-block
  ;;        ;;  #'pcomplete-completions-at-point)
  ;;        ))
  )

;;; Use-package - lax completion for :custom (disabled)

(progn
  ;; (setup emacs
  ;;     (defun my/elisp-custom-keyword-lax-capf ()
  ;;       "Provide lax completion when in s-expr with preceding :custom keyword."
  ;;       (when (and (derived-mode-p 'emacs-lisp-mode)
  ;;                  (my/elisp-custom-keyword-lax-capf--pred))
  ;;         ;; get elisp-capf result
  ;;         (when-let ((result (elisp-completion-at-point)))
  ;;           ;; capf new
  ;;           (append (take 3 result)
  ;;                   (list :annotation-function
  ;;                         (lambda (cand)
  ;;                           (let ((sym (intern-soft cand)))
  ;;                             (cond
  ;;                              ((and sym (boundp sym)) " <var>")
  ;;                              ((and sym (fboundp sym)) " <func>")
  ;;                              ((keywordp sym) " <key>")
  ;;                              (t "")))))))))

  ;;     (defun my/elisp-custom-keyword-lax-capf--pred ()
  ;;       "Predicate for `my/elisp-custom-keyword-lax-capf'.

  ;; Checks if the point is under `use-package' or `leaf',
  ;; and that the last keyword was :custom."
  ;;       (when-let*
  ;;           ((limit
  ;;             (save-excursion
  ;;               (condition-case nil
  ;;                   ;; go up till find use-package or leaf
  ;;                   (progn
  ;;                     (while (not (looking-at-p "(\\(use-package\\|leaf\\)\\b"))
  ;;                       (backward-up-list))
  ;;                     (point))
  ;;                 ;; no matches
  ;;                 (error nil)))))
  ;;         ;; search backwards, find last keyword, if ":custom" ret t
  ;;         (save-excursion
  ;;           (when (re-search-backward " \\(:\\w+\\)" limit t)
  ;;             (string= (match-string 1) ":custom")))))

  ;;     (add-hook 'emacs-lisp-mode-hook
  ;;               (lambda ()
  ;;                 (add-hook 'completion-at-point-functions
  ;;                           #'my/elisp-custom-keyword-lax-capf nil t))))
  )

;;; end

(provide '+completion)
;;; +completion.el ends here

