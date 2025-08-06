# early-init - 

*Author:* lispcat <187922791+lispcat@users.noreply.github.com><br>

The first file loaded at startup.

- loaded before gui creation.
  - disable gui components here.

Sets the following:
- startup variables
- dirs
- UI
  - gui
  - transparency
- eln-cache


# Code

```emacs-lisp

```

## startup vars

```emacs-lisp

;; set startup gc
(setq gc-cons-threshold (* 50 1000 1000))
;; dont load package.el
(setq package-enable-at-startup nil)

```

## dirs

```emacs-lisp

(defvar my/emacs-root-dir       user-emacs-directory)

(defvar my/emacs-src-dir        (file-name-concat my/emacs-root-dir "src"))
(defvar my/emacs-local-dir      (file-name-concat my/emacs-root-dir "local"))
(defvar my/emacs-submodules-dir (file-name-concat my/emacs-root-dir "submodules"))

(defvar my/emacs-config-file    (file-name-concat my/emacs-root-dir "init.el"))

                                        ; set dirs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set main dir to local dir
(setq user-emacs-directory my/emacs-local-dir)

;; set custom-file location
(setq custom-file (file-name-concat my/emacs-local-dir "custom-vars.el"))

```

## UI

```emacs-lisp

                                        ; gui ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

                                        ; transparency ;;;;;;;;;;;;;;;;;;;;;;;;

;; transparency by default
(unless (assoc 'alpha-background default-frame-alist)
  (add-to-list 'default-frame-alist
               '(alpha-background . 100)))

;; make initial frame invisible (note: requires (make-frame-visible) after theme load)
;; (push '(visibility . nil) initial-frame-alist)

;; use color black for startup frame
;; (add-to-list 'default-frame-alist
;;              '(background-color . "#000000"))

```

## eln-cache

```emacs-lisp

;; changes the eln-cache dir to be inside a subdir for cleanliness
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" my/emacs-local-dir))))

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

```

## end

```emacs-lisp

(provide 'early-init)
;;; early-init.el ends here

```


---

*Last updated: { git_revision_date_localized }*
