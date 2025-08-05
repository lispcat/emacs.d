(defvar prefer-eglot-mode? nil)
(defvar prefer-lsp-mode? nil)

(auto-insert-mode)  ;;; Adds hook to find-files-hook

(setq-default indent-tabs-mode nil)
(setq tab-always-indent t)

(leaf compile :ensure nil
  :config
  (setq compilation-scroll-output t))

(leaf flycheck
  :hook prog-mode-hook)

(leaf emacs :ensure nil
  :hook goto-address-mode)

(leaf project :ensure nil
  :bind-keymap ("C-c P" . project-prefix-map)
  :init
  (defun project-compile-interactive ()
    (declare (interactive-only compile))
    (interactive)
    (let ((current-prefix-arg '(4)))
      (call-interactively #'project-compile)))
  :bind
  (project-prefix-map
   ("C" . project-compile-interactive)))

(leaf projectile
  :init
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (setq projectile-compile-use-comint-mode t))

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

;;; lsp-booster
;; use lsp-doctor for testing
;; Steps:
;; - install emacs-lsp-booster
;; - use plist for deserialization (FOLLOW GUIDE)
(leaf emacs :ensure nil
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
  (advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command))

;; for non-programming too
(leaf elec-pair :ensure nil
  :require t
  :config
  ;; disable "<" pair expansion
  (defun my/disable-<-pair-expansion ()
    (setq-local electric-pair-inhibit-predicate
                `(lambda (c)
                   (if (char-equal c ?<)
                       t
                     (,electric-pair-inhibit-predicate c)))))
  (add-hook 'org-mode-hook #'my/disable-<-pair-expansion)
  ;; global
  (electric-pair-mode 1))

(setq my/lisp-mode-hooks
      '(emacs-lisp-mode-hook
        scheme-mode-hook))

;; rainbow parens
(leaf rainbow-delimiters
  :hook `,@my/lisp-mode-hooks)

;; paredit
(leaf paredit
  :hook `,@my/lisp-mode-hooks)

(leaf emacs :ensure nil
  :hook ((emacs-lisp-mode-hook . auto-fill-mode)))

(leaf orglink
  :hook emacs-lisp-mode-hook)

;; other

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

(with-eval-after-load 'flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(leaf scheme-mode :ensure nil
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


;; rustowl
;; (straight-use-package
;;  `(rustowlsp
;;    :host github
;;    :repo "cordx56/rustowl"
;;    :files (:defaults "emacs/*")))

(leaf cc-mode :ensure nil
  :hook ((c-mode-hook . lsp)
         (c-mode-hook . (lambda ()
                          (setq-local lsp-idle-delay 0.1
                                      lsp-enable-indentation nil
                                      lsp-enable-on-type-formatting nil)
                          (c-set-offset 'case-label '+))))
  :config
  (add-to-list 'c-default-style '(c-mode . "cc-mode"))
  (define-key c-mode-map (kbd "<f8>") #'project-compile-interactive))

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

(leaf markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :setq
  (markdown-fontify-code-blocks-natively . t)
  :config
  (defun my/setup-markdown-mode ()
    ;; (visual-fill-column-mode 1)
    (display-line-numbers-mode 0))

  ;; (setq markdown-command "marked")
  (add-hook 'markdown-mode-hook #'my/setup-markdown-mode))

(leaf clojure-mode
  :disabled t)

(leaf scala-mode
  :disabled t
  :interpreter "scala"
  :hook
  (lambda () (setq prettify-symbols-alist
                   scala-prettify-symbols-alist)))

(leaf zig-mode
  :disabled t
  ;; :config
  ;; (zig-format-on-save-mode 0)
  )

(leaf haskell-mode
  :mode "\\.hs\\'")

(leaf nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode-hook . lsp)))

(leaf yaml-mode
  :mode "\\.yml\\'")

(leaf ron-mode
  :require t)

;;;; Kerolox ;;;;

(leaf emacs :ensure nil
  ;;;; Kerolox mode and LSP ;;;;

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


  ;;;; Kerolox treesit mode and LSP

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


  ;;;; Tree-sitter generic ;;;;

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


  ;;;; Misc ;;;;

  ;; Remap regular mode to tree-sitter mode
  (setq major-mode-remap-alist
        '((kerolox-mode . kerolox-ts-mode)))


  ;;;; Auto-mode-alist ;;;;

  ;; Associate file name pattern with major-mode
  (add-to-list 'auto-mode-alist '("\\.rp1\\'" . kerolox-ts-mode)))

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
  (defun my/lsp-clients-lua-language-server-test ()
    "(Improved) Test Lua language server binaries and files."
    (or (and (f-exists? lsp-clients-lua-language-server-main-location)
             (f-exists? lsp-clients-lua-language-server-bin))
        (f-exists? (car (split-string lsp-clients-lua-language-server-command)))))

  (advice-add #'lsp-clients-lua-language-server-test
              :override
              #'my/lsp-clients-lua-language-server-test))

(leaf direnv
  :init
  (direnv-mode 1))

(leaf rainbow-mode
  :hook prog-mode-hook)

(with-eval-after-load 'ansi-color
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(leaf treesit-auto
  :require t
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'my-ide)
