# λ early-init - 

*Author:* lispcat <187922791+lispcat@users.noreply.github.com><br>

The first file loaded at startup, automatically.

At load-time, the Emacs GUI is yet to be created, so here we disable and
modify GUI elements, as well as define path variables that will be used
throughout the rest of the config.

# > Vars

## ‣ Pivotal vars to set first:

Here we set pivotal variables, such as setting the garbage collection
frequency, disabling autoload for the default package manager (to opt in for
a different one), and etc.

```emacs-lisp
;; set startup gc
(setq gc-cons-threshold (* 50 1000 1000))

;; dont load package.el
(setq package-enable-at-startup nil)

```

## ‣ Directory Vars : defvar:

Here we define various path variables.

- root-dir: root of the Emacs config directory (usually "~/.emacs.d/").
- src-dir: modularized config files; contains most of my config.
- local-dir: the root of the Emacs config directory, as Emacs and its
  installed packages understands it to be (more on this later).
- submodules-dir: contains git submodules.
- config-file: the main config file, mostly used for accessibility purposes
  (e.g. a command to open the main config file).

```emacs-lisp
(defvar +emacs-root-dir
  (file-name-as-directory ;; add trailing slash
   (expand-file-name user-emacs-directory)))

(defvar +emacs-src-dir
  (file-name-as-directory
   (expand-file-name "src" +emacs-root-dir)))

(defvar +emacs-local-dir
  (file-name-as-directory
   (expand-file-name "local" +emacs-root-dir)))

(defvar +emacs-submodules-dir
  (file-name-as-directory
   (expand-file-name "submodules" +emacs-root-dir)))

(defvar +emacs-config-file
  (directory-file-name ;; remove trailing slash
   (expand-file-name "init.el" +emacs-root-dir)))

```

## ‣ Directory Vars : setq:

## More on the local-dir

- By default, Emacs throws temp and state files into
  the `user-emacs-directory`, or the root of the configuration directory.
  This can get pretty messy, so we set the user-emacs-directory to a
  subdirectory, the local-dir from earlier.

## The custom-file

- Any customizations saved using Emacs' "customize" interface will be added
  to the `custom-file`. This file is later loaded in init.el.

```emacs-lisp
;; set main dir to local dir
(setq user-emacs-directory +emacs-local-dir)

;; set custom-file location
(setq custom-file (expand-file-name "custom-vars.el" +emacs-local-dir))

```

## ‣ eln-cache dir - set to ./local/var/eln-cache

The eln-cache dir contains compiled .el files (.eln).
To keep the emacs-root-dir tidy, we override this path.

```emacs-lisp
;; only works on Emacs 29
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" +emacs-local-dir))))

```

# > UI

## ‣ GUI

The Emacs GUI has a lot of bloat by default, so they're disabled here (e.g.
tool-bar, menu-bar, scroll-bar).

These UI elements are typically loaded right after early-init.el loads, so
we disable them now.

```emacs-lisp
;; disable tool-bar-setup
(advice-add 'tool-bar-setup :override #'ignore)

;; UI disables
(setq tool-bar-mode nil               ; disable tool bar
      menu-bar-mode nil               ; disable menu bar
      scroll-bar-mode nil)            ; disable vertical scroll bar

;; UI tweaks
(setq default-frame-alist
      '((tool-bar-lines . 0)          ; disable tool bar
        (menu-bar-lines . 0)          ; disable menu bar
        (vertical-scroll-bars)        ; disable vertical scroll bar
        (drag-internal-border . t)
        ;; (internal-border-width . 13) ; box border around buffer+modeline (creates gap) (prev: 15)
        (fullscreen . maximized)      ; TODO: ???
        (left-fringe)                 ; set left fringe
        (right-fringe)                ; set right fringe
        ))

```

## ‣ transparency

We enable transparency by default by adding an entry to default-frame-alist,
unless an entry for it already exists.

```emacs-lisp
(let ((value 95))
  (unless (assoc 'alpha-background default-frame-alist)
    (add-to-list 'default-frame-alist
                 `(alpha-background . ,value))))

;; TODO: what is this???
;; make initial frame invisible (note: requires (make-frame-visible) after theme
;; load)
;; (push '(visibility . nil) initial-frame-alist)

```

## ‣ default background color

In some display environments, during startup, Emacs will show its window.
During this period, a theme will yet to be set. So it will show the default
theme (white, flashbang), so this entry to default-frame-alist will set this
default background color to black.
use color black for startup frame

```emacs-lisp
;; TODO: broken
;; (add-to-list 'default-frame-alist
;;              '(background-color . "#999999"))

```

# > misc

## ‣ lsp

Some tweaks for lsp-mode to increase performance.
<https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization>

```emacs-lisp
(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

```

## ‣ end

```emacs-lisp
(provide 'early-init)
;;; early-init.el ends here
```



---

*Last updated: August 14, 2025*
