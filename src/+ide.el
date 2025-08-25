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

;;; templates for new files

(auto-insert-mode)  ;;; Adds hook to find-files-hook

;;; sane indentation defaults

(setq-default indent-tabs-mode nil)
(setq tab-always-indent t)

;;; tweak compilation buffer

(leaf compile :elpaca nil
  :config
  (setq compilation-scroll-output t))

;;; enable syntax checker

(leaf flycheck
  :hook prog-mode-hook)

;;; URLs buttonize

(leaf emacs :elpaca nil
  :hook goto-address-mode)

;;; project.el

;; FIXME: workaround to get completing-read regexp search?
(leaf project :elpaca nil
  :bind-keymap ("C-c P" . project-prefix-map)
  :init
  (defun project-compile-interactive ()
    (declare (interactive-only compile))
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'project-compile)))
  :config
  (setq xref-search-program 'ripgrep)
  :bind
  (project-prefix-map
   ("C" . project-compile-interactive)))

(-setup consult-project-extra
  (:load-after consult)
  (:require)
  (:with-map project-prefix-map
    (:bind "f" consult-project-extra-find)))

(leaf projectile
  :init
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-compile-use-comint-mode t))

(-setup consult-projectile
  (:load-after projectile)
  (:with-map projectile-command-map
    ;; Buffers, Files, Projects
    (:bind "/" #'consult-projectile)))

;;; lsp-mode

(leaf lsp-mode
  :commands (lsp lsp-deferred)

  :hook (lsp-mode-hook . lsp-enable-which-key-integration)

  :bind-keymap ("C-c l" . lsp-command-map)

  :config
  (setq lsp-inlay-hint-enable t
        ;; freq of refreshing highlights, lenses, links, etc
        lsp-idle-delay 0.5
        ;; bind "C-c l" to lsp-command-map
        lsp-keymap-prefix "C-c l"
        ;; problematic: https://github.com/emacs-lsp/lsp-mode/issues/4113
        lsp-update-inlay-hints-on-scroll nil))

;;;; lsp-ui

(leaf lsp-ui
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

;;;; lsp-booster
;; use lsp-doctor for testing
;; Steps:
;; - install emacs-lsp-booster
;; - use plist for deserialization (FOLLOW GUIDE)
(leaf emacs :elpaca nil
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

;;;; Automatic parenthesis pair matching

;; for non-programming too
(leaf elec-pair :elpaca nil
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

;;; Langs

;;;; Lisp

(setq +lisp-mode-hooks
      '(emacs-lisp-mode-hook
        lisp-data-mode-hook
        scheme-mode-hook))

;;;;; rainbow parens

;; Highlight nested parens according to their depth.
;; ---

(leaf rainbow-delimiters
  :hook `,@+lisp-mode-hooks)

;;;;; paredit

(leaf paredit
  :hook `,@+lisp-mode-hooks)

;;;;; emacs-lisp

(leaf emacs :elpaca nil
  :hook ((emacs-lisp-mode-hook . (lambda ()
                                   (auto-fill-mode)
                                   (setq-local fill-column 80)))))

;;;;; org-style links in elisp

(leaf orglink
  :hook emacs-lisp-mode-hook)

;;;;; elisp misc

(defun create-banner-comment (text &optional width)
  "Create a banner comment with TEXT centered between semicolons.
Optional WIDTH parameter determines total width (defaults to 70)."
  (interactive "sText: ")
  (let* ((width (or width 70))
         (text-len (length text))
         (semi-len (/ (- width text-len 2) 2)) ; -2 for spaces
         (left-semis (make-string semi-len ?\;))
         (right-semis (make-string
                       (if (cl-oddp (- width text-len))
                           (1+ semi-len)
                         semi-len)
                       ?\;)))
    (insert (format "%s %s %s\n"
                    left-semis
                    text
                    right-semis))))

;;;;; tweak flycheck for elisp

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

;;;;; Scheme

(leaf scheme-mode :elpaca nil
  :disabled t
  :mode "\\.sld\\'" "\\.scm\\'")

(leaf geiser
  :disabled t
  :mode "\\.scm\\'"
  :setq
  (geiser-default-implementation . 'guile)
  (geiser-active-implementations . '(guile))
  (geiser-implementations-alist  . '(((regexp "\\.scm$") guile))))

(leaf geiser-guile
  :disabled t
  :after geiser)

;;;;; Clojure

(leaf clojure-mode
  :disabled t) ;;;; Rust

(leaf rust-mode
  :require t
  :init
  (setq rust-mode-treesitter-derive t)
  (setq rust-rustfmt-switches '("--edition" "2021")))

(leaf rustic
  :require t
  :after rust-mode
  :config
  (setq rustic-cargo-use-last-stored-arguments t)
  (setq rustic-format-on-save t)
  (setq rustic-rustfmt-args "--edition 2021")

  ;; lsp-mode settings
  (with-eval-after-load 'lsp-mode
    (setq lsp-rust-analyzer-cargo-watch-command "clippy"
          lsp-rust-analyzer-display-closure-return-type-hints t ; def: nil
          lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
          lsp-rust-analyzer-display-parameter-hints t ; def: nil (input param name)

          ;; maybe
          ;; lsp-rust-analyzer-display-reborrow-hints "mutable" ; def: never (&*(&*jargon))
          lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names t ; def: nil (?)

          ;; experimenting
          lsp-signature-auto-activate t ; def: '(:on-trigger-char :on-server-request)
          ))

  ;; use tree-sitter for rustic-mode
  ;; (define-derived-mode rustic-mode rust-ts-mode "Rustic"
  ;;     "Major mode for Rust code.

  ;; \\{rustic-mode-map}"
  ;;     :group 'rustic
  ;;     (when (bound-and-true-p rustic-cargo-auto-add-missing-dependencies)
  ;;       (add-hook 'lsp-after-diagnostics-hook 'rustic-cargo-add-missing-dependencies-hook nil t)))

  :bind
  (rustic-mode-map
   ("C-c C-c M-r" . rustic-cargo-comint-run)
   ("C-c C-c l" . flycheck-list-errors)
   ("C-c C-c A" . rustic-cargo-add)
   ("C-c C-c R" . rustic-cargo-rm)
   ("C-c C-c a" . lsp-execute-code-action)
   ("C-c C-c r" . lsp-rename)
   ("C-c C-c q" . lsp-workspace-restart)
   ("C-c C-c Q" . lsp-workspace-shutdown)
   ("C-c C-c s" . lsp-rust-analyzer-status)
   ("C-c C-c h" . lsp-describe-thing-at-point))

  :hook
  (rust-ts-mode-hook . (lambda ()
                         ;; company settings
                         (with-eval-after-load 'company
                           (setq-local company-idle-delay 0.3
                                       company-minimum-prefix-length 2))
                         ;; lsp settings
                         (with-eval-after-load 'lsp-mode
                           (setq-local lsp-idle-delay 0.5
                                       lsp-ui-sideline-delay 0.3
                                       lsp-eldoc-render-all nil ; def: nil (minibuffer doc popup)
                                       lsp-ui-doc-enable t ; def: t (ui-popup docs)
                                       lsp-ui-doc-max-height 14 ; def: 13
                                       )))))


;; (leaf rustic :elpaca nil
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


;; rustowl
;; (straight-use-package
;;  `(rustowlsp
;;    :host github
;;    :repo "cordx56/rustowl"
;;    :files (:defaults "emacs/*")))

;;;; C

(leaf cc-mode :elpaca nil
  :hook ((c-mode-hook . lsp)
         (c-mode-hook . (lambda ()
                          (setq-local lsp-idle-delay 0.1
                                      lsp-enable-indentation nil
                                      lsp-enable-on-type-formatting nil)
                          (c-set-offset 'case-label '+))))
  :config
  (add-to-list 'c-default-style '(c-mode . "cc-mode"))
  (define-key c-mode-map (kbd "<f8>") #'project-compile-interactive))

;; (leaf cc-mode :elpaca nil
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

;;;; Java

(leaf lsp-java
  :mode "\\.java\\'"
  :config
  (add-hook 'java-mode-hook #'lsp))

;; (leaf eglot-java
;;   :hook java-mode-hook
;;   :bind
;;   (eglot-java-mode-map
;;    ("C-c l n" . eglot-java-file-new)
;;    ("C-c l x" . eglot-java-run-main)
;;    ("C-c l t" . eglot-java-run-test)
;;    ("C-c l N" . eglot-java-project-new)
;;    ("C-c l T" . eglot-java-project-build-task)
;;    ("C-c l R" . eglot-java-project-build-refresh)))

;;;; Markdown

(leaf markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :setq
  (markdown-fontify-code-blocks-natively . t)
  :config
  (defun +setup-markdown-mode ()
    ;; (visual-fill-column-mode 1)
    (display-line-numbers-mode 0))

  ;; (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'+setup-markdown-mode))

;;;; Scala

(leaf scala-mode
  :disabled t
  :interpreter "scala"
  :hook
  (lambda () (setq prettify-symbols-alist
              scala-prettify-symbols-alist)))

;;;; Zig

(leaf zig-mode
  :disabled t
  ;; :config
  ;; (zig-format-on-save-mode 0)
  )

;;;; Haskell

(leaf haskell-mode
  :mode "\\.hs\\'")

;;;; Nix

(leaf nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode-hook . lsp)))

;;;; Yaml

(leaf yaml-mode
  :mode "\\.yml\\'")

;;;; Ron

(leaf ron-mode
  :require t)

;;;; Kerolox

(leaf emacs :elpaca nil ;; kerolox!

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


;;;;; Kerolox treesit mode and LSP

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


;;;;; kerolox - tree-sitter generic

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


;;;;; Kerolox misc

  ;; Remap regular mode to tree-sitter mode
  (setq major-mode-remap-alist
        '((kerolox-mode . kerolox-ts-mode)))


;;;;; Kerolox - Auto-mode-alist

  ;; Associate file name pattern with major-mode
  (add-to-list 'auto-mode-alist '("\\.rp1\\'" . kerolox-ts-mode)))

;;;; Lua-mode

(leaf lua-mode
  :config
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
              #'+lsp-clients-lua-language-server-test))

;;;; Typst

(use-package typst-ts-mode
  :ensure (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :demand t
  :custom
  (typst-ts-mode-grammar-location (expand-file-name
                                   "tree-sitter/libtree-sitter-typst.so"
                                   user-emacs-directory)))

;;; Tooling

;;;; direnv

(leaf direnv
  :init
  (direnv-mode 1))

;;;; Rainbow mode

;; Add color to hex codes in buffer.
;; --

(leaf rainbow-mode
  :hook prog-mode-hook)

;;;; Ansi-color... not sure what this is for

(with-eval-after-load 'ansi-color
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;;;; Auto-install treesitter backends

(leaf treesit-auto
  :require t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

;;; Code formatting

;;; Code folding

;; TODO: look into: https://github.com/tarsius/outline-minor-faces
;; is this worth it? alternative of

;;;; Outline

(leaf outline-indent
  :doc "Optimal folding: https://github.com/jamescherti/outline-indent.el"
  :commands outline-indent-minor-mode
  :after org
  :custom
  (outline-indent-ellipsis . " ▼")

  :init

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

  :bind

  (outline-minor-mode-map
   ("<backtab>" . +outline-toggle-meta))

  (emacs-lisp-mode-map
   ("C-c C-n" . outline-forward-same-level)
   ("C-c C-p" . outline-backward-same-level))

  ;; buffer
  ("C-c ; ;" . +outline-cycle-buffer)
  ("C-c ; s" . outline-show-all)
  ("C-c ; h" . outline-hide-body)

  ;; subtree
  ("C-c ; t" . outline-show-subtree)
  ("C-c ; T" . outline-hide-subtree)

  ;; other/current
  ("C-c ; O" . outline-hide-other)

  ;; children
  ("C-c ; c" . outline-show-children)
  ("C-c ; C" . outline-hide-children)

  ;; move
  ("C-c ; <up>" . outline-indent-move-subtree-up)
  ("C-c ; <down>" . outline-indent-move-subtree-down)
  ("C-c ; <right>" . outline-indent-shift-right)
  ("C-c ; <left>" . outline-indent-shift-left)

  ; navigation
  ("C-c ; p" . outline-previous-visible-heading)
  ("C-c ; n" . outline-next-visible-heading)
  ("C-c ; b" . outline-backward-same-level)
  ("C-c ; f" . outline-forward-same-level)

  ;; navigation

  :hook
  ((emacs-lisp-mode-hook
    . (lambda ()
        (outline-indent-minor-mode)
        (setq-local make-window-start-visible t)
        (let ((header-comment-p "^\\(;;;+\\) .*")
              (cob-p (string-join
                      '("^;;;;+$"
                        "^;;+ +\\(.*\\) +;$")
                      "\\|"))
              (coh-p (string-join
                      '("^;;; -- \\(.*\\) -+$")))
              (def-p (string-join
                      '("^("))))
          (setq-local outline-regexp
                      (string-join
                       (list
                        ;; cob-p
                        ;; coh-p
                        ;; def-p
                        header-comment-p)
                       ;; '(
                       ;;  ;; "^;;;+ .*"    ; ;;;+ space rest       (regular)
                       ;;  "^;;+ .*"     ; ;;+ space rest (optimal?)
                       ;;  "^;;$"        ; ^;;$ (alt)
                       ;;  "^(...."      ; top-level parens
                       ;;  ;; "^;;+ .* ;$"  ; ;;+ space rest ;      (cob)
                       ;;  "^;;;;+$"     ; ;;;;+ (only, all the way) (cob)
                       ;;  )
                       "\\|"))
          (setq-local outline-level
                      (lambda ()
                        (cond
                         ;; ((looking-at cob-p)
                         ;;  1)
                         ;; ((looking-at coh-p)
                         ;;  2)
                         ((looking-at "^\\(;;;+\\) .*")
                          (- (match-end 1) (match-beginning 1) 2))
                         ((looking-at def-p)
                          1000)
                         (t 0))))

          ;; (setq-local outline-level
          ;;             (lambda ()
          ;;               (cond
          ;;                ;; ((looking-at cob-p) 1)
          ;;                ;; ((looking-at coh-p) 2)
          ;;                ;; ((looking-at cow-p) 3)
          ;;                ((looking-at def-p)
          ;;                 (- (match-end 0) (match-beginning 0))))))
          ;; (setq-local outline-level
          ;;             (lambda ()
          ;;               (cond
          ;;                ((looking-at "^;;;+")
          ;;                 (- (match-end 0) (match-beginning 0)))
          ;;                ((looking-at "^(")
          ;;                 1000)
          ;;                nil)))
          )))))

;;;; Outline faces

(leaf outline-minor-faces
  :after outline outline-indent
  :hook (outline-minor-mode-hook . outline-minor-faces-mode)
  :config
  ;; exclude custom fontlocking for
  ;; (defun +outline-minor-faces--exclude-defuns (orig-fn arg)
  ;;   "Remove ^( patterns from the regex argument."
  ;;   (let ((filtered-regex
  ;;          (or (let ((regex "\\|^("))   ; Fixed: escaped the backslash properly
  ;;                (and (string-search regex arg)
  ;;                     (string-replace regex "" arg))) ; Fixed: "" instead of nil
  ;;              (let ((regex "^(\\|"))                 ; Fixed: escaped properly
  ;;                (and (string-search regex arg)
  ;;                     (replace-regexp-in-string regex "" arg))) ; Fixed: "" instead of nil
  ;;              (let ((regex "^("))
  ;;                (and (string-search regex arg)
  ;;                     (replace-regexp-in-string regex "" arg)))))) ; Fixed: "" instead of nil
  ;;     (if filtered-regex
  ;;         (funcall orig-fn filtered-regex)
  ;;       (funcall orig-fn arg))))
  ;; (advice-add 'outline-minor-faces--syntactic-matcher :around
  ;;             #'+outline-minor-faces--exclude-defuns)
  )

(leaf backline
  :after outline outline-indent
  :config (advice-add 'outline-flag-region :after 'backline-update))

;;;; Elide (hide license header)

(setup elide
  (:with-hook emacs-lisp-mode-hook
    (:hook #'elide-head-mode)))

;;; end

(provide '+ide)
;;; +ide.el ends here
