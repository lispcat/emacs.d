;;; +ide.el ---                                      -*- lexical-binding: t; -*-

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

;; Configure Emacs for IDE functionality.

;;; Code:
;;;; auto-insert-mode (newfile templates)

(auto-insert-mode)  ;;; Adds hook to find-files-hook

;;;; auto-fill-mode (break long lines)

(setup simple
  (diminish 'auto-fill-function ""))

;;;; sane defaults for indentation

(setq-default indent-tabs-mode nil)
(setq tab-always-indent t)

;;;; compilation buffer

(setup compile
  (:option compilation-scroll-output t))

;;;; flymake (built-in syntax checker)

(setup flymake
  (:option flymake-no-changes-timeout 0.3
           flymake-show-diagnostics-at-end-of-line t)
  (:with-map flymake-mode-map
    (:bind "C-c ! n" flymake-goto-next-error
           "C-c ! p" flymake-goto-prev-error
           "C-c ! l" flymake-show-diagnostics-buffer)))

;;;; flycheck (another syntax checker)

;; Emacs comes with a built-in syntax checker Flymake, but has limitations.
;; Flycheck is an "improved" version with more language support.

;; ----

(-setup flycheck
  (:diminish)
  (:option flycheck-display-errors-delay 0.8
           flycheck-idle-change-delay 0.4)
  (add-hook (+get-after-init-hook)
            #'global-flycheck-mode))

(-setup flycheck-inline
  (:load-after flycheck)
  (:with-hook flycheck-mode-hook
    (:hook #'flycheck-inline-mode)))

;;;; buttonize URLs

(setup emacs
  (add-hook (+get-after-init-hook)
            #'global-goto-address-mode))

;;;; project.el (project management)

;; (setup project)

;; (-setup ripgrep
;;   (:require-self))

;; FIXME: workaround to get completing-read regexp search?

;; NOTE: consult-ripgrep is project aware... so this already works....................................
(setup project
  ;; options
  (:option xref-search-program 'ripgrep)
  ;; global
  (global-set-key (kbd "C-c p") project-prefix-map)
  ;; project compile with comint
  (defun project-compile-interactive ()
    (declare (interactive-only compile))
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'project-compile)))
  (:with-map project-prefix-map
    (:bind "C" project-compile-interactive
           "s" consult-ripgrep
           "S" project-shell
           "b" consult-project-buffer)))

;; Note: obsolete, consult-project-buffer is the same?
;; (-setup consult-project-extra
;;   (:load-after consult)
;;   (:require-self)
;;   (:with-map project-prefix-map
;;     (:bind "F" consult-project-extra-find)))

;;;; projectile (alternative to project.el)

(-setup projectile :disabled
        (projectile-mode 1)
        (:global "C-c P" projectile-command-map)
        (:option projectile-compile-use-comint-mode t)

        ;; configure projectile-command-map
        (:with-map projectile-command-map
          (with-eval-after-load 'consult
            (:bind "s r" #'consult-ripgrep))

          (defun +lorri-init ()
            "Run `lorri init` in the current directory and show the output."
            (interactive)
            (when (y-or-n-p "Run `lorri init`? ")
              (shell-command "lorri init")))
          (:bind "c L" #'lorri-init)))

(-setup consult-projectile
  (:load-after projectile)
  (:with-map projectile-command-map
    ;; Buffers, Files, Projects
    (:bind "/" #'consult-projectile)))

;;;; Automatic parenthesis pair matching

;; for non-programming too
(leaf elec-pair :ensure nil
  :require t
  :config
  ;; disable "<" pair expansion
  (defun +disable-<-pair-expansion ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<)
                       t
                     (,electric-pair-inhibit-predicate c)))))
  (add-hook 'org-mode-hook #'+disable-<-pair-expansion)
  ;; global
  (electric-pair-mode 1))

;;;; Toggle lsp-mode vs eglot

(defvar +use-lsp-mode? nil)
(defvar +use-eglot? t)

;;;; lsp-mode

(leaf lsp-mode
  :if +use-lsp-mode?
  :commands (lsp lsp-deferred)

  :hook (lsp-mode-hook . lsp-enable-which-key-integration)

  :custom
  (lsp-keymap-prefix . "C-c l")
  (lsp-completion-enable . nil)

  ;; disable auto adding capf, manually do from cape
  (lsp-completion-provider . :none)

  :config
  (setq lsp-inlay-hint-enable t
        ;; freq of refreshing highlights, lenses, links, etc
        lsp-idle-delay 0.5
        ;; bind "C-c l" to lsp-command-map
        ;; lsp-keymap-prefix "C-c l"
        ;; problematic, lag: https://github.com/emacs-lsp/lsp-mode/issues/4113
        ;; lsp-update-inlay-hints-on-scroll nil
        )
  )

;;;;; lsp-ui

(leaf lsp-ui
  :if +use-lsp-mode?
  :after lsp-mode
  :bind
  (lsp-ui-mode-map
   ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
   ([remap xref-find-references]  . lsp-ui-peek-find-references))
  (lsp-ui-doc-frame-mode-map
   ("q" . lsp-ui-doc-hide)
   ("u" . lsp-ui-doc-unfocus-frame))
  :config
  (setq lsp-ui-doc-delay 0.5
        lsp-ui-doc-position 'top
        ;; lsp-ui-doc-alignment 'window
        lsp-ui-doc-alignment 'frame
        ;; lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor t

        lsp-ui-sideline-delay 0.2

        lsp-ui-imenu-auto-refresh-delay 1.0)

  (with-eval-after-load 'lsp-mode
    (define-key lsp-command-map (kbd "v i") #'lsp-ui-imenu)))

;;;;; lsp-booster

;; use lsp-doctor for testing
;; Steps:
;; - install emacs-lsp-booster
;; - use plist for deserialization (FOLLOW GUIDE)
(leaf emacs :ensure nil
  :if +use-lsp-mode?
  :config
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (defun lsp-booster--advice-json-parse (old-fn &rest args)
    "Try to parse bytecode instead of json."
    (or
     (when (equal (following-char) ?#)
       (let ((bytecode (read (current-buffer))))
         (when (byte-code-function-p bytecode)
           (funcall bytecode))))
     (apply old-fn args)))
  (advice-add (if (progn (require 'json)
                         (fboundp 'json-parse-buffer))
                  'json-parse-buffer
                'json-read)
              :around
              #'lsp-booster--advice-json-parse)
  (defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
    "Prepend emacs-lsp-booster command to lsp CMD."
    (let ((orig-result (funcall old-fn cmd test?)))
      (if (and (not test?) ;; for check lsp-server-present?
               (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
               lsp-use-plists
               (not (functionp 'json-rpc-connection)) ;; native json-rpc
               (executable-find "emacs-lsp-booster"))
          (progn
            (when-let ((command-from-exec-path (executable-find (car orig-result)))) ;; resolve command from exec-path (in case not found in $PATH)
              (setcar orig-result command-from-exec-path))
            (message "Using emacs-lsp-booster for %s!" orig-result)
            (cons "emacs-lsp-booster" orig-result))
        orig-result)))
  (advice-add 'lsp-resolve-final-command :around
              #'lsp-booster--advice-final-command))

;;;; Eglot

;; The Emacs LSP client.
;;
;; It's a more minimal alternative to Lsp-mode, and I find its performance to be
;; much better.

;; ----

(setup eglot
  (:only-if +use-eglot?)
  (:option eglot-report-progress nil)
  (:with-map eglot-mode-map
    (:bind "C-c l a" eglot-code-actions
           "C-c l A" eglot-code-action-quickfix ;; what's this do?
           "C-c l r" eglot-rename
           "C-c l h" eldoc
           "C-c l f" eglot-format
           "C-c l F" eglot-format-buffer
           "C-c l R" eglot-reconnect
           "C-c l n" flycheck-next-error
           "C-c l p" flycheck-previous-error
           )))

;;;;; Eglot extensions

;; Eglot does not support extensions to the LSP protocol.
;; This package adds them.
;; https://github.com/nemethf/eglot-x

;; Useful looking functions:
;; - ! eglot-x-expand-macro
;; - ! eglot-x-reload-workspace
;; - ! eglot-x-analyzer-status
;; - ? eglot-x-ask-related-tests
;; - eglot-x-rebuild-proc-macros
;; - eglot-x-run-flycheck
;; - eglot-x-view-recursive-memory-layout

;; ----

(-setup (eglot-x :host github :repo "nemethf/eglot-x")
  (:load-after eglot)
  (:when-loaded
    (eglot-x-setup)
    (:with-map eglot-mode-map
      (:bind "C-c l q s" eglot-x-analyzer-status
             "C-c l q R" eglot-x-reload-workspace
             "C-c l q e" eglot-x-expand-macro
             "C-c l q p" eglot-x-rebuild-proc-macros))))

;;;;; consult integration

;; An all-in-one consult command to view everything.

;; ((c . "Class") (f . "Function") (e . "Enum") (i . "Interface")
;;  (m . "Module") (n . "Namespace") (p . "Package")
;;  (s . "Struct") (t . "Type Parameter") (v . "Variable")
;;  (A . "Array") (B . "Boolean") (C . "Constant")
;;  (E . "Enum Member") (F . "Field") (M . "Method") (N . "Number")
;;  (O . "Object") (P . "Property") (S . "String") (o . "Other"))

(-setup consult-eglot
  (:only-if +use-eglot?)
  (:load-after consult eglot)
  (:when-loaded
    (:with-map eglot-mode-map
      (:bind "C-c l s" consult-eglot-symbols))))

;;;;; eldoc (documentation at point))

;; Pop-up documentation frame for thing at point.

;; https://github.com/casouri/eldoc-box

;; ----

(setup eldoc
  (:diminish)
  (:option eldoc-echo-area-prefer-doc-buffer t)
  (with-eval-after-load 'eglot
    (:with-map eglot-mode-map
      (:bind "C-c l ?" xref-find-references
             "C-c l ]" xref-go-forward
             "C-c l [" xref-go-back))))

(-setup eldoc-box
  (:only-if +use-eglot?)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t))

;;;;; flymake vs flycheck integration

;; Eglot only supports Flymake, the built-in syntax checker. Flycheck is a
;; better alternative, but needs some tweaking to be used with eglot.

;; Eglot uses Flymake by default. To use flycheck with Eglot, you'll need to
;; use the flycheck-eglot package.

;; -----

(-setup flycheck-eglot
  (:only-if +use-eglot?)
  (:load-after eglot flycheck)
  ;; TODO: experiment with this (does nil worsen performance?)
  ;; Is this necessary to nil for rustowl?
  (:option flycheck-eglot-exclusive nil))

(setup eglot
  (:only-if +use-eglot?)
  (:load-after flycheck-eglot)

  (defcustom +eglot-syntax-checker 'flycheck
    "The syntax checker eglot should use."
    :options '(flycheck flymake))

  (defcustom +eglot-override-syntax-checker nil
    "Specific syntax checker for a major-mode")

  (:when-loaded
    (add-hook 'eglot-managed-mode-hook
              (defun +eglot-setup-flycheck-or-flymake ()
                (pcase (or (alist-get major-mode +eglot-override-syntax-checker)
                           +eglot-syntax-checker)
                  ('flycheck
                   (flycheck-eglot-mode 1))
                  ('flymake
                   (when flycheck-mode
                     (flycheck-mode 0))))))))

;;;;; eglot-booster

;; Vastly improve the performance of eglot.
;; Homepage: [https://github.com/jdtsmith/eglot-booster].
;;
;; Note: try experimenting performance difference by running
;; M-x eglot-booster.
(-setup (eglot-booster :host github :repo "jdtsmith/eglot-booster")
  (:only-if +use-eglot?)
  (:load-after elgot)
  (:when-loaded
    (eglot-booster-mode)))

;;;; Langs

;;;;; Lisp

(defvar +lisp-mode-hooks
  '(emacs-lisp-mode-hook
    lisp-data-mode-hook
    scheme-mode-hook))

(defmacro +add-hooks (hooks-lst func)
  (declare (debug (form symbolp)))
  `(progn
     (unless (listp ,hooks-lst)
       (error "listp: %S" ,hooks-lst))
     (unless (symbolp ,func)
       (error "symbolp: %S" ,func))
     (dolist (hook ,hooks-lst)
       (add-hook hook ,func))))

;;;;;; rainbow parens

;; Highlight nested parens according to their depth.
;; ---

(-setup rainbow-delimiters
  (:hook-into-all +lisp-mode-hooks))

;; (leaf rainbow-delimiters
;;   :hook `,@+lisp-mode-hooks)

;;;;;; paredit

(-setup paredit
  (:diminish)
  (:hook-into-all +lisp-mode-hooks))

;;;;;; emacs-lisp

;; not a real feature
(setup emacs-lisp-mode
  (:hook (lambda ()
           (auto-fill-mode)
           (setq-local fill-column 80))))

;;;;;; org-style links in elisp (disabled)

(-setup orglink :disabled
  (:hook-into emacs-lisp-mode-hook))

;;;;;; elisp misc

;; (defun create-banner-comment (text &optional width)
;;   "Create a banner comment with TEXT centered between semicolons.
;; Optional WIDTH parameter determines total width (defaults to 70)."
;;   (interactive "sText: ")
;;   (let* ((width (or width 70))
;;          (text-len (length text))
;;          (semi-len (/ (- width text-len 2) 2)) ; -2 for spaces
;;          (left-semis (make-string semi-len ?\;))
;;          (right-semis (make-string
;;                        (if (cl-oddp (- width text-len))
;;                            (1+ semi-len)
;;                          semi-len)
;;                        ?\;)))
;;     (insert (format "%s %s %s\n"
;;                     left-semis
;;                     text
;;                     right-semis))))

;;;;;; tweak flycheck for elisp

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc emacs-lisp)))

;;;;;; Scheme (disabled)

;; Scheme is a family of Lisp languages, which includes Guile Scheme, a Lisp
;; used for configuring GNU Guix.

;; -----

;; (setup scheme-mode)

;; improved scheme editing
(-setup geiser :disabled
        (:match-file "\\.scm\\'")
        (:option geiser-default-implementation 'guile
                 geiser-active-implementations '(guile)
                 geiser-implementations-alist  '(((regexp "\\.scm$") guile))))

;; improved guile (dialect of scheme) editing
(-setup geiser-guile :disabled
  (:load-after geiser))

;;;;;; Clojure

(-setup clojure-mode :disabled)

;;;;; Rust

(-setup rust-mode
  (:option rust-mode-treesitter-derive t
           rust-rustfmt-switches '("--edition" "2021")))

(-setup rustic
  (:load-after rust-mode)
  (:option rustic-cargo-use-last-stored-arguments t
           rustic-format-on-save t
           rustic-rustfmt-args "--edition 2021")

  (:with-map rustic-mode-map
    (:bind "C-c C-c M-r" rustic-cargo-comint-run
           "C-c C-c l" flycheck-list-errors
           "C-c C-c A" rustic-cargo-add
           "C-c C-c R" rustic-cargo-rm
           "C-c C-c a" lsp-execute-code-action
           "C-c C-c r" lsp-rename
           "C-c C-c q" lsp-workspace-restart
           "C-c C-c Q" lsp-workspace-shutdown
           "C-c C-c s" lsp-rust-analyzer-status
           "C-c C-c h" lsp-describe-thing-at-point))

  ;; company integration
  (with-eval-after-load 'company
    (add-hook 'rust-ts-mode-hook
              (lambda ()
                (setq-local company-idle-delay 0.3
                            company-minimum-prefix-length 2)))))

;;;;;; lsp-mode version

(setup rustic
  (:only-if +use-lsp-mode?)
  (:load-after rustic lsp-mode)
  (:when-loaded
    (:option lsp-rust-analyzer-cargo-watch-command "clippy"
             lsp-rust-analyzer-display-closure-return-type-hints t ; def: lsp
             nil-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
             lsp-rust-analyzer-display-parameter-hints t ; def: nil (input param name)

             ;; maybe
             ;; lsp-rust-analyzer-display-reborrow-hints "mutable" ; def: never (&*(&*jargon))
             lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t ; def: nil (?)

             ;; experimenting
             lsp-signature-auto-activate t ; def: '(:on-trigger-char :on-server-request)
             )

    (add-hook 'lsp-mode-hook
              (lambda ()
                (setq-local lsp-idle-delay 0.5
                            lsp-ui-sideline-delay 0.3
                            lsp-eldoc-render-all nil ; def: nil (minibuffer doc popup)
                            lsp-ui-doc-enable t      ; def: t (ui-popup docs)
                            lsp-ui-doc-max-height 14 ; def: 13
                            )))))

;;;;;; eglot version

(setup rustic
  (:only-if +use-eglot?)
  (:load-after rustic eglot)
  (:when-loaded
    (:option rustic-lsp-client 'eglot)))

;; (leaf rustic :ensure nil
;;   ;; :disabled t
;;   :if use-eglot?
;;   :init
;;   (setq rustic-lsp-client 'eglot)
;;   (with-eval-after-load 'eglot
;;     (let ((rust-init-options
;;            `(
;;              :cargo       ( :buildScripts (:enable t) :features "all" )
;;              :procMacro   ( :enable t )
;;              :checkOnSave ( :command "clippy" )
;;              :inlayHints  ( :typeHints t
;;                             :parameterHints t
;;                             :closureReturnTypeHints t
;;                             :lifetimeElisionHints (:enable "skip_trivial" :useParameterNames t)
;;                             :reborrowHints "mutable"
;;                             ;; :chainingHints t
;;                             )
;;              )))
;;       (add-to-list 'eglot-server-programs
;;                    `(rustic-mode . ("rust-analyzer"
;;                                     :initializationOptions ,rust-init-options)))))
;;   ;; :config

;;   )

;;;;;; rustowl (disabled)

;; Note: not compatible with Eglot
;; - requires a second LSP server (eglot only supports one)

;; ----

;; will require tinkering with source code for eglot integration
;; https://github.com/cordx56/rustowl/issues/16
;; (-setup (rustowl :host github :repo "lispcat/rustowl-fork")
;;   (:load-after eglot rust-mode)
;;   (:when-loaded
;;     ;; (add-to-list 'eglot-server-programs
;;     ;;              '((rust-mode rust-ts-mode rustic-mode) . ("rustowl")))
;;     ))

;;;;; C

(setup cc-mode
  (:when-loaded
    (add-to-list 'c-default-style '(c-mode . "cc-mode"))
    (define-key c-mode-map (kbd "<f8>") #'project-compile-interactive))
  (:with-hook c-mode-hook
    (when +use-lsp-mode?
      (:hook lsp-deferred)
      (:hook (lambda ()
               (setq-local lsp-idle-delay 0.1
                           lsp-enable-indentation nil
                           lsp-enable-on-type-formatting nil)
               (c-set-offset 'case-label '+))))
    (when +use-eglot?
      (:hook eglot-ensure))))

;; (leaf cc-mode :ensure nil
;;   :if use-eglot?
;;   :hook ((c-mode-hook . eglot-ensure)
;;          (c-mode-hook . (lambda ()
;;                           ;; (setq-local lsp-idle-delay 0.1
;;                           ;;             lsp-enable-indentation nil
;;                           ;;             lsp-enable-on-type-formatting nil)
;;                           (c-set-offset 'case-label '+))))
;;   :config
;;   (add-to-list 'c-default-style '(c-mode . "cc-mode"))
;;   (define-key c-mode-map (kbd "<f8>") #'project-compile-interactive))

;;;;; Java

;; Java LSP support

;; https://github.com/emacs-lsp/lsp-java

;; (-setup lsp-java :disabled
;;   (:option lsp-java-format-settings-url
;;            (expand-file-name "no-search/java/eclipse-java-google-style.xml"
;;                              +emacs-src-dir)

;;            lsp-java-format-settings-profile "GoogleStyle")

;;   (:with-feature java-mode
;;     (:when-loaded
;;       ;; lsp
;;       (:hook lsp-deferred)
;;       ;; (:hook eglot-ensure)

;;       ;; auto-fill
;;       (:local-set fill-column 100)
;;       (:hook auto-fill-mode)

;;       ;; binds
;;       (:bind "<f9>" #'projectile-compile-project)))

;;   (:with-feature java-ts-mode
;;     (:when-loaded
;;       ;; lsp
;;       (:hook lsp-deferred)
;;       ;; (:hook eglot-ensure)

;;       ;; auto-fill
;;       (:hook auto-fill-mode)
;;       (:local-set fill-column 100)

;;       ; binds
;;       (:bind "<f9>" #'projectile-compile-project))))

(-setup eglot-java
  (:load-after eglot)
  (:hook-into java-mode-hook
              java-ts-mode-hook)
  (:with-map eglot-java-mode-map
    (:bind "C-c l l n" eglot-java-file-new
           "C-c l l x" eglot-java-run-main
           "C-c l l t" eglot-java-run-test
           "C-c l l N" eglot-java-project-new
           "C-c l l T" eglot-java-project-build-task
           "C-c l l R" eglot-java-project-build-refresh))
  (:when-loaded
    (setq eglot-java-user-init-opts-fn 'custom-eglot-java-init-opts)
    (defun custom-eglot-java-init-opts (server eglot-java-eclipse-jdt)
      "Custom options that will be merged with any default settings."
      `(:settings
        (:java
         (:format
          (:settings
           (:url ,(concat "file://"
                          (expand-file-name "no-search/java/eclipse-java-google-style.xml"
                                            +emacs-src-dir)))
           :enabled t))))))

  (with-eval-after-load 'eglot
    (add-to-list '+eglot-override-syntax-checker '(java-ts-mode flymake))
    (add-to-list '+eglot-override-syntax-checker '(java-mode flymake)))

  (defun my/java-mode-setup (mode)
    ;; lsp
    (add-hook mode #'eglot-ensure)
    ;; (add-hook mode #'lsp-deferred)

    ;; auto-fill
    (setq-local fill-column 100)
    (add-hook mode #'auto-fill-mode)

    ;; binds
    (define-key (symbol-value (intern (format "%s-map" mode)))
                (kbd "<f9>") #'projectile-compile-project))

  (:with-feature java-mode
    (:when-loaded
      (my/java-mode-setup 'java-mode)))

  (:with-feature java-ts-mode
    (:when-loaded
      (my/java-mode-setup 'java-ts-mode))))

;;;;; Markdown

(-setup markdown-mode
  (:match-file "\\.md\\'")
  (:with-mode gfm-mode
    (:match-file "README\\.md\\'" ))
  (:option markdown-fontify-code-blocks-natively t)
  (:when-loaded
    (defun +setup-markdown-mode ()
      ;; (visual-fill-column-mode 1)
      (display-line-numbers-mode 0))

    ;; (setq markdown-command "marked")
    (add-hook 'markdown-mode-hook #'+setup-markdown-mode)))

;;;;; Scala (disabled)

(-setup scala-mode :disabled
  (:hook-into (lambda ()
                (setq prettify-symbols-alist
                      scala-prettify-symbols-alist))))

;;;;; Zig (disabled)

(-setup zig-mode :disabled
  ;; :config
  ;; (zig-format-on-save-mode 0)
  )

;;;;; Haskell

(-setup haskell-mode
  (:match-file ".hs")
  (:with-feature haskell-cabal
    (:match-file ".cabal")))

(-setup lsp-haskell
  (add-hook 'haskell-mode-hook #'lsp-deferred)
  (add-hook 'haskell-literate-mode-hook #'lsp-deferred))

;;;;; Nix

(-setup nix-mode
  (:hook eglot-ensure))

;;;;; Yaml

(-setup yaml-mode)

;;;;; Ron (disabled)

(-setup ron-mode :disabled)

;;;;; Kerolox

(-setup emacs
  :disabled

  ;; Major-mode for .rp1 files
  (define-derived-mode kerolox-mode prog-mode "kerolox"
    "Major mode for editing kerolox (.rp1) files."
    :group 'kerolox)

  (with-eval-after-load 'lsp-mode
    ;; Register LSP server and setup LSP server
    (add-to-list 'lsp-language-id-configuration '(kerolox-mode . "kerolox"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("/home/sui/Code/cloned/saturn-v/target/release/saturn-v" "lsp"))
      :major-modes '(kerolox-mode)
      :server-id 'saturn-v-lsp)))


;;;;;; Kerolox treesit mode and LSP

  (define-derived-mode kerolox-ts-mode kerolox-mode "kerolox[ts]"
    "Tree-sitter based major mode for editing kerolox (.rp1) files."
    :group 'kerolox

    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))

      ;; create parser for this buffer
      (treesit-parser-create 'kerolox)

      (setq-local treesit-font-lock-feature-list
                  '((comment)
                    (keyword string)
                    (constant type)
                    (function variable module constructor)
                    (operator punctuation)))

      (setq-local font-lock-defaults nil)

      ;; Set up face mapping for tree-sitter query capture names to Emacs faces
      (defvar kerolox-ts-font-lock-settings
        (treesit-font-lock-rules
         :language 'kerolox
         :feature 'comment
         '((comment) @font-lock-comment-face)

         :language 'kerolox
         :feature 'constant
         '((integer) @font-lock-constant-face
           (value (symbol)) @font-lock-constant-face)

         :language 'kerolox
         :feature 'variable
         '((variable) @font-lock-variable-name-face)

         :language 'kerolox
         :feature 'module
         '((import (symbol)) @font-lock-preprocessor-face)

         :language 'kerolox
         :feature 'type
         '((type (symbol)) @font-lock-type-face)

         :language 'kerolox
         :feature 'function
         '((definition relation: (symbol)) @font-lock-function-name-face
           (atom head: (symbol)) @font-lock-function-name-face)

         :language 'kerolox
         :feature 'constructor
         '((rule relation: (symbol)) @font-lock-function-name-face)

         :language 'kerolox
         :feature 'punctuation
         '(([":-" "," "."]) @font-lock-delimiter-face
           (["(" ")"]) @font-lock-bracket-face)

         :language 'kerolox
         :feature 'operator
         '((binary_expr op: (_)) @font-lock-builtin-face
           (unary_expr op: (_)) @font-lock-builtin-face
           (cardinality kind: (_)) @font-lock-builtin-face)

         :language 'kerolox
         :feature 'keyword
         '((["constrain" "decision" "define" "import" "output" "soft"]) @font-lock-keyword-face
           (constraint_kind) @font-lock-keyword-face))
        "Font-lock settings for Kerolox.")

      ;; Set font-lock settings from the defined rules
      (setq-local treesit-font-lock-settings kerolox-ts-font-lock-settings)

      (treesit-major-mode-setup)))

  ;; Register LSP server and setup LSP server
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-language-id-configuration '(kerolox-ts-mode . "kerolox"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection '("/home/sui/Code/cloned/saturn-v/target/release/saturn-v" "lsp"))
      :major-modes '(kerolox-ts-mode)
      :server-id 'saturn-v-ts-lsp)))


;;;;;; kerolox - tree-sitter generic

  (with-eval-after-load 'treesit
    ;; Configure the language grammar source and mapping
    (when (and (fboundp 'treesit-available-p)
               (treesit-available-p))
      ;; Define grammar source
      (add-to-list 'treesit-language-source-alist
                   '(kerolox . ("https://github.com/marceline-cramer/saturn-v" nil "tree-sitter-kerolox/src")))

      ;; ;; Set up language mapping
      ;; (add-to-list 'treesit-language-remap-alist '(kerolox-ts-mode . kerolox))

      ;; Only install if not already installed
      ;; (unless (treesit-language-available-p 'kerolox)
      ;;   (treesit-install-language-grammar 'kerolox))
      (treesit-install-language-grammar 'kerolox)
      ))

  ;; Auto-start LSP when opening .rp1 files with tree-sitter mode
  (add-hook 'kerolox-ts-mode-hook #'lsp-deferred)


;;;;;; Kerolox misc

  ;; Remap regular mode to tree-sitter mode
  (setq major-mode-remap-alist
        '((kerolox-mode . kerolox-ts-mode)))


;;;;;; Kerolox - Auto-mode-alist

  ;; Associate file name pattern with major-mode
  (add-to-list 'auto-mode-alist '("\\.rp1\\'" . kerolox-ts-mode)))

;;;;; Lua-mode

(-setup lua-mode
  (:when-loaded
    (with-eval-after-load 'lsp-lua
      ;; fix issue with externally installed server
      (setq lsp-clients-lua-language-server-command
            "lua-language-server")
      ;; renoise lua api definitions
      ;; (setq lsp-lua-workspace-library "'Lua.workspace.library': {'/home/sui/Music/prod/scripts/renoise-lua/definitions': true}")
      (setq lsp-lua-workspace-library (ht ("/home/sui/Music/prod/scripts/renoise-lua/definitions" t)))
      (setq lsp-lua-runtime-plugin "/home/sui/Music/prod/scripts/renoise-lua/definitions/plugin.lua")
      )

    ;; fix pt.2
    (defun +lsp-clients-lua-language-server-test ()
      "(Improved) Test Lua language server binaries and files."
      (or (and (f-exists? lsp-clients-lua-language-server-main-location)
               (f-exists? lsp-clients-lua-language-server-bin))
          (f-exists? (car (split-string lsp-clients-lua-language-server-command)))))

    (advice-add #'lsp-clients-lua-language-server-test
                :override
                #'+lsp-clients-lua-language-server-test)))

;;;;; Typst

(-setup (typst-ts-mode :type git :host codeberg :repo "meow_king/typst-ts-mode")
  (:option typst-ts-mode-grammar-location
           (expand-file-name
            "tree-sitter/libtree-sitter-typst.so"
            user-emacs-directory))

  ;; open output pdf in other window
  (defun +typst-ts-mode-open-pdf ()
    (interactive)
    (let* ((orig-win (selected-window))
           (target-extension "pdf")
           (current-path (buffer-file-name))
           (target-path (file-name-with-extension current-path
                                                  target-extension)))
      (find-file-other-window target-path)
      (select-window orig-win)))
  (add-hook 'typst-ts-mode-hook #'+typst-ts-mode-open-pdf)
  (:option typst-ts-preview-function #'+typst-ts-mode-open-pdf)

  ;; auto compile
  (add-hook 'typst-ts-mode-hook #'typst-ts-watch-mode))

;;;;; Typescript

(-setup typescript-mode)

;;;; Tooling
;;;;; direnv

(-setup direnv
  (direnv-mode 1))

;;;;; Rainbow mode

;; Add color to hex codes in buffer.
;; --

(-setup rainbow-mode
  (:diminish)
  (:hook-into prog-mode-hook))

;;;;; TODO: Ansi-color... not sure what this is for

(with-eval-after-load 'ansi-color
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;;;;; Auto-install treesitter backends

(-setup treesit-auto
  (:option treesit-auto-install 'prompt)
  (:autoload global-treesit-auto-mode)
  (global-treesit-auto-mode))

;;;; Code formatting

;;;; Code folding

;; TODO: look into: https://github.com/tarsius/outline-minor-faces
;; is this worth it? alternative of

;;;;; Outline

;; Optimal folding: https://github.com/jamescherti/outline-indent.el
(-setup outline-indent
  (:diminish outline-minor-mode)
  (:diminish outline-indent-minor-mode)
  (:autoload outline-indent-minor-mode)
  (:option outline-indent-ellipsis " ‣")

  ;; outline-cycle
  (defun +outline-toggle (&optional univ)
    "Toggle previous heading.

If hide, fold only current heading.
If show, open only current heading.

If ran with Universal Argument, run `+outline-cycle-buffer' instead."
    (interactive "P")
    ;; go to prev heading
    (outline-back-to-heading)
    ;; universal arg
    (if current-prefix-arg
        (+outline-cycle-buffer)
      ;; toggle
      (let ((action
             (if (outline-invisible-p (pos-eol))
                 'to-show
               'to-hide)))
        (pcase action
          ('to-hide
           (outline-hide-entry))
          ('to-show
           (outline-show-entry))
          (_ (error "bug"))))))

  (+defhydra-repeat +outline-toggle
                    (";" "<backtab>"))

  (defun +outline-toggle-meta (&optional univ)
    "Toggle subtree at previous heading.

If hide, fold current and all subheadings, and show tree.
If show, open /everything/ under the heading.

If ran with Universal Argument, run `+outline-cycle-buffer' instead."
    (interactive "P")
    ;; go to prev heading
    (outline-back-to-heading)
    ;; universal arg
    (if current-prefix-arg
        (+outline-cycle-buffer)
      ;; toggle
      (let ((action
             (if (outline-invisible-p (pos-eol))
                 'to-show
               'to-hide)))
        (pcase action
          ('to-hide
           (outline-hide-subtree)
           (outline-show-branches))
          ('to-show
           (outline-show-subtree))
          (_ (error "bug"))))))

  (+defhydra-repeat +outline-toggle-meta
                    (";" "<backtab>"))

  ;; outline-cycle buffer
  (defun +outline-cycle-buffer (&optional level)
    (interactive (list (when current-prefix-arg
                         (prefix-numeric-value current-prefix-arg))))
    (let (top-level)
      (save-excursion
        (goto-char (point-min))
        (while (not (or (eq top-level 1) (eobp)))
          (when-let ((level (and (outline-on-heading-p t)
                                 (funcall outline-level))))
            (when (< level (or top-level most-positive-fixnum))
              (setq top-level (max level 1))))
          (outline-next-heading)))
      (cond
       (level
        (outline-hide-sublevels level)
        (setq outline--cycle-buffer-state 'all-heading)
        (message "All headings up to level %s" level))
       ((or (eq outline--cycle-buffer-state 'show-all)
            (eq outline--cycle-buffer-state 'top-level))
        (outline-show-all)
        (outline-hide-region-body (point-min) (point-max))
        (setq outline--cycle-buffer-state 'all-heading)
        (message "All headings"))
       (t
        (outline-show-all)
        (setq outline--cycle-buffer-state 'show-all)
        (message "Show all")))))

  (+defhydra-repeat +outline-cycle-buffer
                    (";" "<backtab>"))

  ;; special TAB, cycle if on heading
  (defun +indent-for-tab-command--outline-advice (orig-fn &rest args)
    "Advice for alternative TAB behavior if over outline heading."
    (if (and (eq major-mode 'emacs-lisp-mode)
             (save-excursion
               (beginning-of-line)
               (looking-at "^;;;+ .*$")))
        (+outline-toggle)
      (apply orig-fn args)))

  (advice-add 'indent-for-tab-command :around
              #'+indent-for-tab-command--outline-advice)

  ;; run outline-hide-body only after first focus (add to .dir-locals.el)
  ;; (defun +hide-outline-on-open (func &rest args)
  ;;   "Hide outlines when opening files via dired or projectile."
  ;;   (let ((result (apply func args)))
  ;;     ;; After the file is opened, hide outlines if conditions are met
  ;;     (when (and (buffer-file-name)
  ;;                outline-indent-minor-mode)
  ;;       (outline-hide-body))
  ;;     result))

  ;; (advice-add 'find-file :around #'+hide-outline-on-open)
  ;; (advice-add 'dired-find-file :around #'+hide-outline-on-open)
  ;; (advice-add 'projectile-find-file :around #'+hide-outline-on-open)
  ;; (advice-add 'projectile-find-file-dwim :around #'+hide-outline-on-open)

  (:with-map outline-minor-mode-map
    (:bind "<backtab>" +outline-toggle-meta))

  (:with-map emacs-lisp-mode-map
    (:bind "C-c C-n" outline-next-visible-heading
           "C-c C-p" outline-previous-visible-heading))

  (:when-loaded
    (defun +outline-faces-setup ()
      (dolist (face-config
               '((outline-1 1.9 nil)
                 (outline-2 1.6 nil)
                 (outline-3 1.3 t)
                 (outline-4 1.1 t)
                 (outline-5 1.0 t)
                 (outline-6 1.0 t)
                 (outline-7 1.0 t)
                 (outline-8 1.0 t)))
        (let ((face (nth 0 face-config))
              (height (nth 1 face-config))
              (over (nth 2 face-config)))
          (set-face-attribute face nil :height height :overline over)))
      ;; extras
      (with-eval-after-load 'org
        (set-face-attribute 'org-ellipsis nil :foreground 'unspecified)))
    ;; run now
    (+outline-faces-setup)
    ;; run after each theme load
    (add-hook '+after-enable-theme-hook #'+outline-faces-setup)

    (progn

      (defvar my-outline-map (make-sparse-keymap)
        "Keymap for outline commands.")

      (dolist (binding
               '(;; buffer
                 (";" . +outline-cycle-buffer)
                 ("s" . outline-show-all)
                 ("h" . outline-hide-body)
                 ;; subtree
                 ("t" . outline-show-subtree)
                 ("T" . outline-hide-subtree)
                 ;; other/current
                 ("O" . outline-hide-other)
                 ;; children
                 ("c" . outline-show-children)
                 ("C" . outline-hide-children)
                 ;; move
                 ("<up>" . outline-indent-move-subtree-up)
                 ("<down>" . outline-indent-move-subtree-down)
                 ("<right>" . outline-indent-shift-right)
                 ("<left>" . outline-indent-shift-left)
                 ;; navigation
                 ("p" . outline-previous-visible-heading)
                 ("n" . outline-next-visible-heading)
                 ("b" . outline-backward-same-level)
                 ("f" . outline-forward-same-level)))
        (define-key my-outline-map (kbd (car binding)) (cdr binding)))

      ;; Bind the keymap to C-c ;
      (global-set-key (kbd "C-c o o") my-outline-map)))

  ;; buffer
  (:global "C-c o ;" +outline-cycle-buffer
           "C-c o s" outline-show-all
           "C-c o h" outline-hide-body)

  (:with-hook emacs-lisp-mode-hook
    (:hook (lambda ()
             (outline-indent-minor-mode)
             ;; (setq-local make-window-start-visible t) ;; TODO: see what commenting out does
             (let ((header-comment-p "^\\(;;;+\\) .*"))
               (setq-local outline-regexp header-comment-p)
               (setq-local outline-level
                           (lambda ()
                             (if (looking-at "^\\(;;;+\\) .*")
                                 (- (match-end 1) (match-beginning 1) 2)
                               0)))
               )))))

;;;;; Outline faces

(-setup outline-minor-faces
  (:load-after outline outline-indent)
  (:with-hook outline-minor-mode-hook
    (:hook outline-minor-faces-mode))
  ;; (progn
  ;;   ;; exclude custom fontlocking for defuns
  ;;   (defun +outline-minor-faces--exclude-defuns (orig-fn arg)
  ;;     "Remove ^( patterns from the regex argument."
  ;;     (let ((filtered-regex
  ;;            (or (let ((regex "\\|^("))   ; Fixed: escaped the backslash properly
  ;;                  (and (string-search regex arg)
  ;;                       (string-replace regex "" arg))) ; Fixed: "" instead of nil
  ;;                (let ((regex "^(\\|"))                 ; Fixed: escaped properly
  ;;                  (and (string-search regex arg)
  ;;                       (replace-regexp-in-string regex "" arg))) ; Fixed: "" instead of nil
  ;;                (let ((regex "^("))
  ;;                  (and (string-search regex arg)
  ;;                       (replace-regexp-in-string regex "" arg)))))) ; Fixed: "" instead of nil
  ;;       (if filtered-regex
  ;;           (funcall orig-fn filtered-regex)
  ;;         (funcall orig-fn arg))))
  ;;   (advice-add 'outline-minor-faces--syntactic-matcher :around
  ;;               #'+outline-minor-faces--exclude-defuns))
  )

(-setup backline
  (:load-after outline outline-indent)
  (:when-loaded
    (advice-add 'outline-flag-region :after 'backline-update)))

;;;;; Elide (hide license header)

(setup elide
  (:with-hook emacs-lisp-mode-hook
    (:hook #'elide-head-mode)))

;;; Provide:

(provide '+ide)
;;; +ide.el ends here
