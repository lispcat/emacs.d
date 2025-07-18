;; [[file:README.org::*variables][variables:1]]
(setq package-enable-at-startup nil)      ; dont load package.el
(setq gc-cons-threshold (* 50 1000 1000)) ; startup gc
(setq load-prefer-newer t)                ; run .el instead of .elc if newer
(setq native-comp-async-report-warnings-errors nil) ; Silence compiler warnings
;; variables:1 ends here

;; [[file:README.org::*variables][variables:2]]
(defvar my/emacs-root-dir       user-emacs-directory)

(defvar my/emacs-src-dir        (file-name-concat my/emacs-root-dir "src"))
(defvar my/emacs-local-dir      (file-name-concat my/emacs-root-dir "local"))
(defvar my/emacs-submodules-dir (file-name-concat my/emacs-root-dir "submodules"))

(defvar my/emacs-config-file    (file-name-concat my/emacs-root-dir "README.org"))

;; set local dir to local files
(setq user-emacs-directory      my/emacs-local-dir)

;; set custom-file
(setq custom-file (file-name-concat my/emacs-local-dir "custom-vars.el"))
;; variables:2 ends here

;; [[file:README.org::*UI tweaks][UI tweaks:1]]
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
;; UI tweaks:1 ends here

;; [[file:README.org::*startup transparency][startup transparency:1]]
;; transparency by default
(unless (assoc 'alpha-background default-frame-alist)
  (add-to-list 'default-frame-alist
               '(alpha-background . 100)))

;; make initial frame invisible (note: requires (make-frame-visible) after theme load)
;; (push '(visibility . nil) initial-frame-alist)

;; use color black for startup frame
;; (add-to-list 'default-frame-alist
;;              '(background-color . "#000000"))
;; startup transparency:1 ends here

;; [[file:README.org::*eln-cache dir tweaks][eln-cache dir tweaks:1]]
;; changes the eln-cache dir to be inside a subdir for cleanliness
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" my/emacs-local-dir))))
;; eln-cache dir tweaks:1 ends here

;; [[file:README.org::*lsp-booster tweaks][lsp-booster tweaks:1]]
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)
;; lsp-booster tweaks:1 ends here
