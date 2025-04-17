;;; my-ide.el --- ide setup

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp (generic) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default indent-tabs-mode nil)
(setq tab-always-indent t)

(leaf compile :ensure nil
  :setq
  (compilation-scroll-output . t))

(leaf flycheck
  :hook prog-mode-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf lsp-mode
  :ensure lsp-ui
  :commands (lsp lsp-deferred)

  ;; bind "C-c l" to lsp-command-map
  :setq
  (lsp-keymap-prefix . "C-c l")
  ;; (lsp-ui-doc-show-with-cursor . t) ; def: nil
  ;; (lsp-ui-doc-side . 'right) ; def: right
  ;; (lsp-ui-doc-position . 'at-point)

  :bind-keymap
  ("C-c l" . lsp-command-map)

  :hook
  (lsp-mode-hook . lsp-enable-which-key-integration)
  ;; :config
  ;; (defface lsp-flycheck-warning-unnecessary '((t))
;;     "Face which apply to side line for symbols not used.
;; Possibly erroneously redundant of lsp-flycheck-info-unnecessary-face."
;;     :group 'lsp-ui-sideline)
  )

;;; lsp-booster
;; use lsp-doctor for testing
;; Steps:
;; - install emacs-lsp-booster
;; - use plist for deserialization (FOLLOW GUIDE)
(progn
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

;; TODO: move this to corfu ?
;; if corfu is installed
;; (https://github.com/minad/corfu/wiki#configuring-corfu-for-lsp-mode)
;; (use-package lsp-mode :ensure nil
;;   :commands (lsp lsp-deferred)
;;   :after corfu
;;   :custom
;;   (lsp-completion-provider :none) ; use corfu!
;;   :init
;;   (defun my/orderless-dispatch-flex-first (_pattern index _total)
;;     (and (eq index 0) 'orderless-flex))

;;   (defun my/lsp-mode-setup-completion ()
;;     (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;           '(orderless))
;;     ;; Optionally configure the first word as flex filtered.
;;     (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
;;     ;; Optionally configure the cape-capf-buster.
;;     (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point))))

;;   :hook
;;   (lsp-completion-mode . my/lsp-mode-setup-completion))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; projectile ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf projectile
  :after
  :init
  (projectile-mode 1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; hex colors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf rainbow-mode
  :hook prog-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;;; lisp (generic) ;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;; rainbow
(leaf rainbow-delimiters) ; hooks below

;; paredit
(leaf paredit) ; hooks below

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; emacs-lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf rainbow-delimiters :ensure nil
  :hook emacs-lisp-mode-hook)

(leaf paredit :ensure nil
  :hook emacs-lisp-mode-hook)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scheme ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf rainbow-delimiters :ensure nil
  :hook scheme-mode-hook)

(leaf paredit :ensure nil
  :hook scheme-mode-hook)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; C/C++ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; java ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf lsp-java
  :disabled t
  :mode "\\.java\\'"
  :config
  (add-hook 'java-mode-hook #'lsp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; markdown ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; rust ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://robert.kra.hn/posts/rust-emacs-setup

(leaf rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :bind (rustic-mode-map
         ("C-c C-c M-r" . rustic-cargo-comint-run)
         ("C-c C-c l" . flycheck-list-errors)
         ("C-c C-c a" . lsp-execute-code-action)
         ("C-c C-c A" . rustic-cargo-add)
         ("C-c C-c r" . lsp-rename)
         ("C-c C-c R" . rustic-cargo-rm)
         ("C-c C-c q" . lsp-workspace-restart)
         ("C-c C-c Q" . lsp-workspace-shutdown)
         ("C-c C-c s" . lsp-rust-analyzer-status)
         ("C-c C-c h" . lsp-describe-thing-at-point))
  :config
  (setq rustic-cargo-use-last-stored-arguments t
        rustic-format-on-save t
        lsp-rust-analyzer-cargo-watch-command "clippy"
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-chaining-hints nil ; def: nil
        lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil ; def: nil
        lsp-rust-analyzer-display-closure-return-type-hints t ; def: nil
        lsp-rust-analyzer-display-parameter-hints nil ; def: nil
        lsp-rust-analyzer-display-reborrow-hints t) ; def: never

  (add-hook 'rust-mode-hook #'my/rust-mode-hook)

  (defun my/rust-mode-hook ()
    (with-eval-after-load 'lsp-ui
      (setq-local lsp-ui-peek-always-show t
                  lsp-ui-sideline-delay 0.4
                  lsp-ui-doc-enable nil))
    (with-eval-after-load 'lsp-mode
      (setq-local lsp-idle-delay 0.6
                  lsp-inlay-hint-enable t
                  lsp-eldoc-render-all t))
    (with-eval-after-load 'company
      (setq-local company-idle-delay 0.5
                  company-minimum-prefix-length 1))))

;; rustowl
;; (straight-use-package
;;  `(rustowlsp
;;    :host github
;;    :repo "cordx56/rustowl"
;;    :files (:defaults "emacs/*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; clojure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf clojure-mode
  :disabled t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; scala ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf scala-mode
  :disabled t
  :interpreter "scala"
  :hook
  (lambda () (setq prettify-symbols-alist
              scala-prettify-symbols-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; zig ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf zig-mode
  :disabled t
  ;; :config
  ;; (zig-format-on-save-mode 0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; haskell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf haskell-mode
  :mode "\\.hs\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nix ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf nix-mode
  :mode "\\.nix\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; yaml ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf yaml-mode
  :mode "\\.yml\\'")

;;;;;;;;;;;;;;;;;;;;;;;;;;;; direnv/envrc ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf direnv
  :init
  (direnv-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; code folding ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(leaf hideshow
  :hook
  (prog-mode-hook . hs-minor-mode)
  :config
  ;; new fold function
  (defun my/toggle-fold ()
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  ;; unset orig keymap from minor-mode
  (setf (alist-get 'hs-minor-mode minor-mode-map-alist) nil)
  ;; new custom keymap
  ;; (defvar my/hs-minor-mode-map
  ;;   (let ((map (make-sparse-keymap)))
  ;;     (define-key map (kbd "h") #'hs-hide-block)
  ;;     (define-key map (kbd "s") #'hs-show-block)
  ;;     (define-key map (kbd "a") #'hs-hide-all)
  ;;     (define-key map (kbd "r") #'hs-show-all)
  ;;     (define-key map (kbd "l") #'hs-hide-level)
  ;;     (define-key map (kbd "t") #'my/toggle-fold)
  ;;     map))
  ;; bind new keymap
  ;; (define-key global-map (kbd "C-c @") my/hs-minor-mode-map)
  ;; (with-eval-after-load 'lsp-mode
  ;;   (define-key lsp-command-map (kbd "t") my/hs-minor-mode-map))
  ;; hydra
  (defhydra hydra-folding (:color red)
    "Code folding"
    ("t" my/toggle-fold "toggle")
    ("l" hs-hide-level  "hide level")
    ("s" hs-show-block  "show block")
    ("h" hs-hide-block  "hide block")
    ("S" hs-show-all    "Show all")
    ("H" hs-hide-all    "Hide all")
    ("n" next-line      "next line")
    ("p" previous-line  "previous line")
    ("j" scroll-up-command "down")
    ("k" scroll-down-command "up")
    ("g" nil "quit")
    ("c" nil "close"))
  (general-my-map
    "@" 'hydra-folding/body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; end ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'my-ide)
;;; my-ide.el ends here
