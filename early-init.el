;; [[file:Config.org::*early-init.el][early-init.el:1]]
(eval-when-compile
  (require 'cl-lib))

;; settings

(setq gc-cons-threshold (* 50 1000 1000)) ; lower gc freq
(setq package-enable-at-startup nil)      ; dont load package.el

;; dir vars

(defvar my/emacs-root-dir user-emacs-directory)

(cl-macrolet
    ((defdir (name sub)
       `(defvar ,name
          (file-name-as-directory
           (expand-file-name ,sub my/emacs-root-dir)))))
  (defdir my/emacs-init-dir "init")
  (defdir my/emacs-src-dir "src")
  (defdir my/emacs-local-dir "local")
  (defdir my/emacs-submodules-dir "submodules"))

(defvar my/emacs-config-file
  (expand-file-name "Config.org" my/emacs-root-dir))

(setq user-emacs-directory my/emacs-local-dir) ; main dir

(setq custom-file ;; set custom-file location
      (expand-file-name "custom-vars.el" my/emacs-local-dir))

;; change eln-cache dir

(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" my/emacs-local-dir))))

;; ui

(advice-add 'tool-bar-setup :override #'ignore) ; disable tool bar

(setq tool-bar-mode nil                 ; disable tool bar
      menu-bar-mode nil                 ; disable menu bar
      scroll-bar-mode nil)              ; disable vertical scroll bar

(setq default-frame-alist
      '((tool-bar-lines . 0)            ; disable tool bar
        (menu-bar-lines . 0)            ; disable menu bar
        (vertical-scroll-bars)          ; disable vertical scroll bar
        (drag-internal-border . t)
        ;; (internal-border-width . 13) ; box border around buffer+modeline (creates gap)
        (fullscreen . maximized)        ; TODO: ???
        (left-fringe)                   ; set left fringe
        (right-fringe)                  ; set right fringe
        (undecorated . t) ; fix river bar at top issue
        ))

;; transparency

(let ((value 95))
  (unless (assoc 'alpha-background default-frame-alist)
    (add-to-list 'default-frame-alist
                 `(alpha-background . ,value))))

;; misc tweaks

(setenv "LSP_USE_PLISTS" "true")        ; lsp-mode
(setq lsp-use-plists t)                 ; lsp-mode
;; early-init.el:1 ends here
