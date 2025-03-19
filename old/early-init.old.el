;;; early-init.el --- early-init

;;; Commentary:
;;
;; Order:
;; - variables
;; - paths
;; - tweak ui
;; - transparency
;; - eln-cache dir

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq package-enable-at-startup nil)      ; dont load package.el
(setq gc-cons-threshold (* 50 1000 1000)) ; startup gc
(setq load-prefer-newer t)                ; run .el instead of .elc if newer
(setq native-comp-async-report-warnings-errors nil) ; Silence compiler warnings

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; paths ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar my/emacs-root-dir       user-emacs-directory)

(defvar my/emacs-src-dir        (file-name-concat my/emacs-root-dir "src"))
(defvar my/emacs-local-dir      (file-name-concat my/emacs-root-dir "local"))
(defvar my/emacs-submodules-dir (file-name-concat my/emacs-root-dir "submodules"))

;; set local dir to local files
(setq user-emacs-directory      my/emacs-local-dir)

;; set custom-file
(setq custom-file (file-name-concat my/emacs-local-dir "custom-vars.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tweak ui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; disable tool-bar-setup
(advice-add 'tool-bar-setup :override #'ignore)

;; UI disables
(setq tool-bar-mode nil                 ; disable tool bar
      menu-bar-mode nil                 ; disable menu bar
      scroll-bar-mode nil)              ; disable vertical scroll bar

;; UI tweaks
(setq default-frame-alist
      '((tool-bar-lines . 0)            ; disable tool bar
        (menu-bar-lines . 0)            ; disable menu bar
        (vertical-scroll-bars)          ; disable vertical scroll bar
        (drag-internal-border . t)
        ;; (internal-border-width . 13) ; box border around buffer+modeline (creates gap) (prev: 15)
        (fullscreen . maximized)        ; TODO: ???
        (left-fringe)                   ; set left fringe
        (right-fringe)                  ; set right fringe
        ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; transparency ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; transparency by default
(unless (assoc 'alpha-background default-frame-alist)
  (add-to-list 'default-frame-alist (cons 'alpha-background 100)))

;;;;;;;;;;;;;;;;;;;;;;;;;;; eln-cache dir ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; changes the eln-cache dir to be inside a subdir for cleanliness
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" my/emacs-local-dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; lsp-booster ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;;;;;;;;;;;;;;;;;;;;;;;;; end of early-init ;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
