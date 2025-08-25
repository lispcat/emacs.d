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

;; TODO: set C-M-c to completion-at-point (orig C-M-i).

;; TODO: try corfu + cape (follow system crafters vid)
;; - https://www.youtube.com/watch?v=f0FMo_XxujU
;; + tempel (replace org-tempo)

;; TODO: start adding some setup.el in my config, little by little.

;; ? : corfu, kind-icon, wgrep?, consult-dir, cape
;; ^ more at ~/code/cloned/daviwil-dots/.emacs.d/modules/dw-interface.el
;; TODO: vim keybinds for vertico completion shit (work on later) (also daviwil)
;;
;; a framework for minibuffer completion
;; (https://github.com/minad/vertico)

;;; Vertico

(-setup vertico
  (:option vertico-scroll-margin 3     ; Different scroll margin (def 2)
           vertico-count 10            ; Show more candidates (def 10)
           vertico-resize 'grow-only   ; Grow/shrink minibuffer (def 'grow-only)
           vertico-cycle nil)      ; cycle for `vertico-next/previous' (def nil)
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
  (:require)

  ;; Keybinds:

  (:global
   ;; recent files
   "C-c f r" consult-recent-file ; recent file

   ;; buffers
   "C-c b b" consult-buffer     ; switch-to-buffer
   "C-x 4 b" consult-buffer-other-window
   "C-x 5 b" consult-buffer-other-frame

   ;; editing
   "M-y" consult-yank-pop       ; fallback to after yank
   "M-S-y" consult-yank-from-kill-ring
   "<f5>" consult-kmacro

   ;; register (C-u for window layout!)
   "M-#" consult-register-store  ; store
   "C-M-#" consult-register-load ; load

   ;; navigation
   "C-c ; l" consult-outline     ; outline heading
   "C-c o l" consult-org-heading ; org heading
   "C-c s a" consult-org-agenda  ; agenda heading

   "C-c s l" consult-imenu      ; list var/func
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

   ;; programming
   "C-c l e" consult-compile-error

   ;; histories
   "C-c s x" consult-complex-command ; past keychord

   ;; themes
   "C-c T t" consult-theme

   ;; list minor modes
   "C-c s m" consult-minor-mode-menu
   "C-c s M-x" consult-mode-command ; M-x only for current mode
   "C-c M-x" consult-mode-command

   ;; Info
   "C-c s I" consult-info
   [remap Info-search] consult-info

   ;; Man pages
   "C-c s M" consult-man)

  ;; project.el integration
  (:with-map project-prefix-map
    (:bind "b" consult-project-buffer
           "C-b" consult-project-buffer))

  ;; isearch integration
  (:with-map isearch-mode-map
    (:bind "M-r" consult-isearch-history))


  ;; minibuffer history
  (:with-map minibuffer-local-map
    (:bind "M-r" consult-history))

  ;; Settings:

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
        '(:debounce 0.05 any)
        ;; 'any
        )

  ;; delay previews - custom length
  (consult-customize
   consult-ripgrep
   consult-git-grep
   consult-grep
   ;; consult-man
   ;; consult-bookmark
   consult-xref
   consult-recent-file
   ;; consult--source-bookmark
   consult--source-recent-file
   consult--source-project-recent-file
   ;; :preview-key 'any
   :preview-key '(:debounce 0.1 any) ;; Delay preview 0.4 sec
   )

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
  (leader-bind
    "s" '(:ignore t :which-key "search")))

;;;; consult-dir

;; used to go to a file in a bookmarked dir n stuff (one ex)
(-setup consult-dir
  (leader-bind
    "dd" 'consult-dir
    "fd" 'consult-dir))

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
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Company

;; TODO: disable most backends by default add a bunch per mode (org should only
;; have a few
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


;;; Yasnippet

;; https://stackoverflow.com/questions/72601990/how-to-show-suggestions-for-yasnippets-when-using-eglot

(leaf yasnippet :elpaca yasnippet-snippets
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

(leaf isearch :elpaca nil
  :bind
  ("C-M-s" . isearch-forward)
  ("C-M-r" . isearch-backward))

;;; Corfu

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

;;; Cape

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

;;; Tempel

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

;; (leaf abbrev :elpaca nil
;;   :bind (("C-c c a" . add-global-abbrev)
;;          ("C-c c -" . inverse-add-global-abbrev)
;;          ("C-c c e" . edit-abbrevs)))

;;; Use-package - lax completion for :custom

(leaf emacs :elpaca nil
  :config
  (defun my/elisp-custom-keyword-lax-capf ()
    "Provide lax completion when in s-expr with preceding :custom keyword."
    (when (and (derived-mode-p 'emacs-lisp-mode)
               (my/elisp-custom-keyword-lax-capf--pred))
      ;; get elisp-capf result
      (when-let ((result (elisp-completion-at-point)))
        ;; capf new
        (append (take 3 result)
                (list :annotation-function
                      (lambda (cand)
                        (let ((sym (intern-soft cand)))
                          (cond
                           ((and sym (boundp sym)) " <var>")
                           ((and sym (fboundp sym)) " <func>")
                           ((keywordp sym) " <key>")
                           (t "")))))))))

  (defun my/elisp-custom-keyword-lax-capf--pred ()
    "Predicate for `my/elisp-custom-keyword-lax-capf'.

Checks if the point is under `use-package' or `leaf',
and that the last keyword was :custom."
    (when-let*
        ((limit
          (save-excursion
            (condition-case nil
                ;; go up till find use-package or leaf
                (progn
                  (while (not (looking-at-p "(\\(use-package\\|leaf\\)\\b"))
                    (backward-up-list))
                  (point))
              ;; no matches
              (error nil)))))
      ;; search backwards, find last keyword, if ":custom" ret t
      (save-excursion
        (when (re-search-backward " \\(:\\w+\\)" limit t)
          (string= (match-string 1) ":custom")))))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'completion-at-point-functions
                        #'my/elisp-custom-keyword-lax-capf nil t))))



(provide '+completion)
;;; +completion.el ends here

