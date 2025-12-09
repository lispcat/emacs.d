;;; +ui.el --- ui improvements                       -*- lexical-binding: t; -*-

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

;; UI improvements.

;;; Code:

;;;; Fontconfig

;; https://devfonts.gafi.dev/

(defun +fontconfig ()
  (set-face-attribute
   'default nil :font
   ;; "Hack"
   ;; "-Misc-TamzenForPowerline-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1"
   ;; "-UW  -Ttyp0-regular-normal-normal-*-17-*-*-*-m-*-iso8859-1"
   ;; "-UW-Ttyp0-bold-normal-normal-*-17-*-*-*-c-90-iso8859-1"
   ;; "-UW  -Ttyp0-regular-normal-normal-*-16-*-*-*-m-*-iso8859-1"
   ;; "-UW  -Ttyp0-regular-italic-normal-*-16-*-*-*-m-*-iso10646-1"
   ;; "-AW-Greybeard 16px-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1"
   ;; "Fira Code"
   ;; "Maple Mono"
   ;; "Jetbrains Mono"
   ;; "Iosevka"
   ;; "Iosevka-11"
   ;; "Iosevka-11"
   ;; "Iosevka Custom"
   "Iosevka Custom-11"
   ;; "Iosevka NFM-11"
   ;; "Iosevka NFP-11"
   ;; "Iosevka Extended"
   ;; "Aporetic Sans Mono"
   ;; "Aporetic Sans Mono-11"
   ;; "Aporetic Serif Mono"
   ;; "Aporetic Serif Mono-11"
   ;; "Rec Mono Casual"
   ;; "Rec Mono Duotone"
   ;; "Rec Mono Linear"
   ;; "Rec Mono Semicasual"
   ;; "Recursive"
   ;; "Recursive Mn Csl St"
   ;; "Recursive Mn Lnr St"
   )
  (set-face-attribute
   'variable-pitch nil :font
   ;; "Aporetic Sans-11"
   ;; "Aporetic Serif-11"
   ;; "Iosevka-11"
   ;; "Iosevka NFP-13"
   ;; "Iosevka Custom"
   "Iosevka Custom-11"
   ;; "Recursive Sn Csl St"
   ;; "Recursive Sn Lnr St"
   ))

(+fontconfig)

;; Hack: fix bitmap fonts on emacsclient frames
(add-hook 'server-after-make-frame-hook #'+fontconfig)

;; enable variable-pitch-mode by default in org-mode
(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'variable-pitch-mode))

;;;; All the icons

;; all the icons
(-setup all-the-icons
  (:when-loaded
    ;; Use 'prepend for the NS and Mac ports or Emacs will crash.
    (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
    (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append)))

;;;; Ligatures

(-setup ligature
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (let ((all-ligatures
         '(;; line arrows
           "-<<" "-<" "-<-" "<--" "<---" "<<-" "<-" "->" "->>" "-->" "--->"
           "->-" ">-" ">>-"
           ;; double arrows
           "=<<" "=<" "=<=" "<==" "<===" "<<=" "<=" "=>" "=>>" "==>" "===>"
           "=>=" ">=" ">>="
           ;; two-way arrows
           "<->" "<-->" "<--->" "<---->" "<=>" "<==>" "<===>" "<====>" "::"
           ":::" "__"
           ;; slashes
           "<~~" "</" "</>" "/>" "~~>" "==" "!=" "/=" "~=" "<>" "===" "!=="
           "!===" "=/=" "=!="
           ;; carrots
           "<:" ":=" "*=" "*+" "<*" "<*>" "*>" "<|" "<|>" "|>" "<." "<.>" ".>"
           "+*" "=*" "=:" ":>"
           ;; wrap
           "(*" "*)" "/*" "*/" "[|" "|]" "{|" "|}" "++" "+++" "\\/" "/\\" "|-"
           "-|" "<!--" "<!---")))
    (ligature-set-ligatures 'prog-mode all-ligatures)
    (ligature-set-ligatures 'org-mode all-ligatures))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  (global-ligature-mode t))

;;;; Themes

;;;;; Function: sets a random theme.

(defun +set-random-theme ()
  "Set a random theme."
  (interactive)
  (let* ((available-themes (custom-available-themes))
         (current-theme (car custom-enabled-themes))
         (themes-except-current (remove current-theme available-themes))
         (chosen-theme (nth (random (length themes-except-current))
                            themes-except-current)))
    ;; disable all enabled themes
    (mapc #'disable-theme custom-enabled-themes)
    ;; enable randomly chosen theme
    (if (custom-theme-p chosen-theme)
        (enable-theme chosen-theme)
      (load-theme chosen-theme :no-confirm))
    ;; mesg
    (message "Enabled theme: %s" chosen-theme)))

;; Install themes

;; fav themes:
;; - ef-owl
;; - ef-dream

(leaf emacs :ensure nil
  :preface
  (leaf kaolin-themes
    :require t)
  (leaf ef-themes
    :require t)
  (leaf doom-themes
    :require t
    :setq
    (doom-themes-enable-bold   . t)    ; if nil, bold is universally disabled
    (doom-themes-enable-italic . t)    ; if nil, italics is universally disabled
    )

  :leaf-defer nil
  :bind
  ("C-c T t" . consult-theme)
  ("C-c T r" . +set-random-theme))

(leaf emacs :ensure nil
  :after doom-themes kaolin-themes ef-themes
  :config
  (+set-random-theme))

;;;; Transparency

(defvar +transparency-value 100)

(defun +native-transparency-supported? ()
  "Whether native-transparency is supported on this version of Emacs."
  (if (version<= "29" emacs-version)
      t
    (message "Native transparency is not supported.")
    nil))

(defun +toggle-transparency ()
  "Toggle transparency with `+transparency-value'."
  (interactive)
  (when (+native-transparency-supported?)
    (let ((alpha (frame-parameter nil 'alpha-background)))
      (set-frame-parameter
       nil 'alpha-background
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
                100)
           +transparency-value
         100)))))

(defun +set-transparency (value)
  "Set the transparency of the frame window to VALUE."
  (interactive "nTransparency Value 0 - 100 opaque: ")
  (when (+native-transparency-supported?)
    (set-frame-parameter (selected-frame) 'alpha-background value)))

;;;; Line numbers

;; list of programming modes to disable line-numbers on
(defvar +display-line-numbers-exclude '())

;; enable line-numbers on programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (memq major-mode +display-line-numbers-exclude)
              (display-line-numbers-mode 1))))

(setq display-line-numbers-type 'relative)

;;;; Visual line mode

(global-visual-line-mode 1)
(diminish 'visual-line-mode) ; hide "Wrap" in mode-line

;;;; show whitespace

(setup whitespace
  (:diminish whitespace-mode)
  (:option whitespace-trailing 'whitespace-hspace)

  (defvar +base-whitespace-style '(face trailing tabs missing-newline-at-eof))
  (defun +prog-mode-whitespace ()
    (setq whitespace-style (append +base-whitespace-style '(tab-mark)))
    (whitespace-mode 1))
  (defun +org-mode-whitespace ()
    (setq whitespace-style (append +base-whitespace-style '()))
    (whitespace-mode 1))

  (add-hook 'prog-mode-hook #'+prog-mode-whitespace)
  (add-hook 'org-mode-hook #'+org-mode-whitespace)
  (add-hook 'text-mode-hook #'+org-mode-whitespace))

;;;; Solaire mode

;; (leaf solaire-mode
;;   :config
;;   (defun real-buffer-p ()
;;     (or (solaire-mode-real-buffer-p)
;;         (equal (buffer-name) "*dashboard*")))
;;   (setq solaire-mode-real-buffer-fn #'real-buffer-p)

;;   (solaire-global-mode +1))

;;;; Spacious padding

(-setup spacious-padding
  (:require-self)
  (:option spacious-padding-widths
           (plist-put spacious-padding-widths :header-line-width 0)
           spacious-padding-widths
           (plist-put spacious-padding-widths :mode-line-width 0))
  (spacious-padding-mode 1))

;;;; Mode-line

(add-to-list 'load-path
             (file-name-concat +emacs-submodules-dir
                               "cat-line"))

;; show column # on modeline
(column-number-mode 1)

;;;;; cat-line (mode-line framework)

(setup cat-line
  (:require-self)
  (cat-line-mode 1))

;;;;; custom segments

;; helper segments:

(defun +mode-line-half-space ()
  (propertize " "
              'display '((space :relative-width 0.5))))

(defun +mode-line-tall-space ()
  (let ((top (propertize " " 'display `(raise 0.2)))
        (bot (propertize " " 'display `(raise ,(- 0.25)))))
    (propertize (concat top " " bot))))

(defvar +mode-line-empty
  `(((+mode-line-tall-space))))

(defface +mode-line-doc-face-no-slant
  '((t font-lock-doc-face (:slant normal)))
  "Basically `font-lock-doc-face' but with no slant.")

;; segments:

(defun +mode-line-segment-persp-name ()
  (propertize (format-mode-line persp-lighter)
              'face 'shadow))

(defun +mode-line-segment-pdf-page ()
  (when (eq major-mode 'pdf-view-mode)
    (let ((page-current (image-mode-window-get 'page))
          (page-total (pdf-cache-number-of-pages)))
      (propertize (format "%d/%d " page-current page-total)
                  'face 'shadow))))

(defun +mode-line-segment-cursor-position ()
  (propertize (format-mode-line "%l:%c")
              'face 'shadow))

(defun +mode-line-segment-scroll ()
  (or (+mode-line-segment-pdf-page)
      (propertize (format-mode-line "%p%")
                  'face 'shadow)))

(defun +mode-line-get-project-root ()
  (when-let*
      ((path
        (or
         ;; project?
         (and (fboundp 'project-current)
              (when-let* ((project (project-current)))
                (expand-file-name
                 (if (fboundp 'project-root)
                     (project-root project)
                   (car (with-no-warnings
                          (project-roots project)))))))
         ;; projectile?
         (and (bound-and-true-p projectile-mode)
              (projectile-project-root)))))
    ;; if project path, then abbreviate it
    (abbreviate-file-name path)))

(defun +mode-line-segment-buffer-name ()
  (if-let* ((buffer-path buffer-file-name)
            (project-root (+mode-line-get-project-root))
            (project-name (file-name-nondirectory
                           (directory-file-name project-root))))
      (concat
       ;; project directory
       (propertize (concat project-name "/")
                   'face
                   ;; `((:weight normal) mode-line-buffer-id)
                   `((:weight normal) mode-line-emphasis)
                   ;; `(mode-line-emphasis)
                   )
       ;; relative path
       (propertize (when-let*
                       ((relative-path
                         (file-relative-name
                          (or (file-name-directory
                               (or (abbreviate-file-name buffer-path)
                                   buffer-path))
                              "./")
                          project-root)))
                     (if (string= relative-path "./")
                         ""
                       (substring (shrink-path--dirs-internal relative-path t)
                                  1)))
                   'face `(font-lock-string-face))
       ;; file name
       (propertize (file-name-nondirectory buffer-path)
                   'face `((:inherit bold) mode-line-buffer-id)))
    ;; fallback
    (propertize (buffer-name)
                'face `mode-line-buffer-id)))

(defvar +mode-line-segment-meow-state-alist
  `((normal "[N]" . (success (:inherit bold)))
    (insert "[I]" . (font-lock-keyword-face (:inherit bold)))
    (beacon "[B]" . (warning (:inherit bold)))
    (keypad "[K]" . (mode-line (:inherit bold)))
    (motion "[M]" . (+mode-line-doc-face-no-slant (:inherit bold)))))

(defun +mode-line-segment-meow-state ()
  (when (bound-and-true-p meow--current-state)
    (let ((icon-face-pair (alist-get meow--current-state
                                     +mode-line-segment-meow-state-alist)))
      (concat (propertize (car icon-face-pair)
                          'face (cdr icon-face-pair))))))

;;;;; Doom Modeline

(-setup doom-modeline :disabled
  ;; configuration
  (setq doom-modeline-height 30
        doom-modeline-modal-icon t
        doom-modeline-icon t
        doom-modeline-persp-icon nil
        doom-modeline-bar-width 4
        )

  (add-hook (+get-after-init-hook) #'doom-modeline-mode)

  (:when-loaded
    ;; hide bar
    (set-face-attribute 'mode-line-inactive nil
                        :foreground (face-background 'mode-line-inactive))

    ;; custom modeline
    (doom-modeline-def-modeline 'my-line
      '(eldoc
        bar
        window-state
        workspace-name
        window-number
        modals
        matches
        follow
        buffer-info
        remote-host
        buffer-position
        word-count
        parrot
        selection-info)
      '(compilation
        objed-state
        misc-info
        project-name
        persp-name
        battery
        grip irc
        mu4e
        gnus
        github
        debug
        repl
        lsp
        minor-modes
        input-method
        indent-info
        ;; buffer-encoding
        major-mode
        process
        vcs
        check
        time))
    ;; enable
    (add-hook 'doom-modeline-mode-hook
              (lambda ()
                (doom-modeline-set-modeline 'my-line 'default))))

  ;; (:option doom-modeline-height 30
  ;;          doom-modeline-icon nil)

  ;; (:when-loaded
  ;;   ;; custom modeline
  ;;   (doom-modeline-def-modeline 'my-simple-line
  ;;     '(eldoc window-state workspace-name window-number modals matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
  ;;     '(compilation objed-state misc-info project-name persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check time))

  ;;   ;; set
  ;;   (add-hook 'doom-modeline-mode-hook
  ;;             (lambda ()
  ;;               (doom-modeline-set-modeline 'my-simple-line 'default)))

  ;;   ;; enable
  ;;   ;; (doom-modeline-mode 1)
  ;;   )
  ;; (setq-default header-line-format '("%e" (:eval (doom-modeline-format--main))))
  ;; (setq-default mode-line-format nil)
  ;; (add-hook 'doom-modeline-mode-hook
  ;;           (lambda ()
  ;;             (setq-default mode-line-format nil)
  ;;             (dolist (buf (buffer-list))
  ;;               (with-current-buffer buf
  ;;                 (when mode-line-format
  ;;                   (setq mode-line-format nil))))))

  ;; (add-hook '+after-enable-theme-hook
  ;;           (lambda ()
  ;;             (unless mode-line-format
  ;;               (setq-default mode-line-format nil))))

  ;; :config
  ;; (setq doom-modeline-modal-icon nil)
  ;; (dolist (pair '((doom-modeline-vcs-default . "purple")
  ;;                 (doom-modeline-vcs . "purple")
  ;;                 (doom-modeline-meow-normal-state . "DarkOrchid4")))
  ;;   (let ((face (car pair))
  ;;         (color (cdr pair)))
  ;;     (set-face-attribute face nil :foreground color)))
  )

(-setup doom-modeline
  (:require-self))

;;;;; Mood-line

;; TODO: denote buffer formatting...

(-setup mood-line
  (:require-self)
  ;; (mood-line-mode 1) ;; DISABLED

  ;; if broken, run this:
  ;; (mood-line--process-format mood-line-format)

  ;; hide if not selected
  (advice-add 'mood-line--process-format :around
              (defun mood-line--process-format--advice (orig-fun &rest args)
                (if (mode-line-window-selected-p)
                    (apply orig-fun args)
                  (apply orig-fun (list +mode-line-empty)))))

  ;; glyphs
  (defconst mood-line-glyphs-custom
    '((:checker-info . ?↳)
      (:checker-issues . ?→)
      (:checker-good . ?✓)
      (:checker-checking . ?⟳)
      (:checker-errored . ?x)
      (:checker-interrupted . ?=)

      (:vc-added . ?+)
      (:vc-needs-merge . ?⟷)
      (:vc-needs-update . ?↓)
      (:vc-conflict . ?x)
      (:vc-good . ?✓)

      (:buffer-narrowed . ?◢)
      (:buffer-modified . ?●)
      (:buffer-read-only . ?■)

      (:frame-client . ?)

      (:count-separator . ?×))
    "Set of ASCII glyphs for use with mood-line.")

  (setq mood-line-glyph-alist mood-line-glyphs-unicode)

  ;; printing format
  (setq custom-mood-line-format
        (mood-line-defformat
         :left
         (" "
          ((+mode-line-segment-meow-state)           . " ")
          ((or (mood-line-segment-buffer-status) "") . " ")
          ((+mode-line-segment-buffer-name)          . "  ")
          ;; ((mood-line-segment-buffer-name)           . "  ")
          ((mood-line-segment-anzu)                  . "  ")
          ((mood-line-segment-multiple-cursors)      . "  ")
          ((+mode-line-segment-scroll)               . " ")
          ((+mode-line-segment-cursor-position)       . " ")
          (+mode-line-tall-space))
         :right
         (((mood-line-segment-misc-info)   . "  ")
          ((+mode-line-segment-persp-name) . "   ")
          ;; (+mode-line-half-space)
          ((mood-line-segment-vc)          . "  ")
          ((mood-line-segment-major-mode)  . "  ")
          ((mood-line-segment-checker)     . "  ")
          ((mood-line-segment-process)     . "  ")))

        mood-line-format custom-mood-line-format))

;;;;; Nano modeline (disabled)

;; nano theme and modeline

;; (-setup (nano-theme :host github :repo "rougier/nano-theme"))

(-setup (nano-modeline :host github :repo "rougier/nano-modeline") :disabled
        (:require-self))

(-setup (nano-modeline :host github :repo "rougier/nano-modeline") :disabled
        (:require-self)
        (setq-default mode-line-format nil)
        (defun +nano-modeline-setup-faces ()
          (face-spec-set
           'nano-modeline-active
           `((t (
                 :foreground ,(face-foreground 'default)
                 :background ,(face-background 'mode-line)
                 :box (:line-width 1 :color ,(face-background
                                              'default))))))
          (face-spec-set
           'nano-modeline-inactive
           `((t (
                 :foreground ,(face-foreground 'default)
                 :background ,(face-background 'mode-line-inactive)
                 :box (:line-width 1 :color ,(face-background
                                              'default))))))
          (face-spec-set
           'nano-modeline-status
           `((t (
                 ;; :inherit mode-line-highlight
                 :foreground ,(face-background 'mode-line-inactive)
                 :background ,(face-foreground 'default)
                 :box (:line-width 1 :color ,(face-background
                                              'default)))))))
        (+nano-modeline-setup-faces)
        (add-hook '+after-enable-theme-hook #'+nano-modeline-setup-faces)

        ;; configs

        (defun +nano-modeline-current-mode (&optional name)
          (let ((name (or name (car-safe mode-name))))
            (propertize (format-mode-line name)
                        'face (nano-modeline-face 'secondary))))

        (defun +nano-modeline-current-persp (&optional name)
          (let ((name (or name persp-lighter)))
            (propertize (format-mode-line name)
                        'face (nano-modeline-face 'secondary))))

        (defun +nano-modeline-cursor-position (&optional format)
          (let ((format (or format "%l:%c %p%% ")))
            (propertize (format-mode-line format)
                        'face (nano-modeline-face 'secondary))))

        (defun +nano-modeline--meow-state ()
          (when (boundp 'meow--current-state)
            (let ((mode-cons (alist-get meow--current-state
                                        '((normal "<N>" . success)
                                          (insert "<I>" . font-lock-keyword-face)
                                          (beacon "<B>" . warning)
                                          (motion "<M>" . +mode-line-doc-face-no-slant)
                                          (keypad "<K>" . mode-line)))))
              (concat (propertize (car mode-cons)
                                  'face (cdr mode-cons))))))

        (defun +nano-modeline-buffer-status (&optional status padding)
          (let* ((padding (or padding nano-modeline-padding))
                 (top (propertize " " 'display `(raise ,(car padding))))
                 (bot (propertize " " 'display `(raise ,(- (cdr padding)))))
                 (meow (delete ?> (delete ?< (+nano-modeline--meow-state)))))
            (propertize (concat top (or status meow) bot)
                        'face (nano-modeline-face 'status-RO))
            ;; (cond (buffer-read-only
            ;;        (propertize (concat top (or status "RO") bot)
            ;;                    'face (nano-modeline-face 'status-RO)))
            ;;       ((buffer-modified-p)
            ;;        (propertize (concat top (or status "**") bot)
            ;;                    'face (nano-modeline-face 'status-**)))
            ;;       (t
            ;;        (propertize (concat top (or status "RW") bot)
            ;;                    'face (nano-modeline-face 'status-RW))))
            ))

        (defun +nano-modeline-prog-mode (&optional default)
          (funcall nano-modeline-position
                   '((+nano-modeline-buffer-status) " "
                     (nano-modeline-buffer-name) " "
                     (nano-modeline-git-info))
                   '((+nano-modeline-current-persp) "  "
                     (+nano-modeline-current-mode) "  "
                     (+nano-modeline-cursor-position)
                     (nano-modeline-window-dedicated))
                   default))

        ;;   ;; (setq nano-modeline-position )
        (add-hook 'prog-mode-hook            #'+nano-modeline-prog-mode)
        (add-hook 'text-mode-hook            #'+nano-modeline-prog-mode)
        ;; (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
        (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
        (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
        ;;   (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
        ;;   (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
        ;;   (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
        ;;   (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
        ;;   (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
        ;;   (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
        ;;   (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
        ;;   (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
        ;; (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)
        )

;; (-setup (feline :host github :repo "chee/feline-mode")
;;   (feline-mode 0)
;;   (:option feline-line-prefix "L"
;;            feline-column-prefix "C"
;;            feline-mode-symbols
;;            '(emacs-lisp-mode "λ"
;;                              python-mode "py"
;;                              typescript-mode "ts"
;;                              rustic-mode "🦀"
;;                              rust-mode "🦀"
;;                              zig-mode "🦎"
;;                              scheme-mode "🐔")))

;;;;; Custom Modeline (disabled)

;; (-setup mood-line
;;   (:require-self))

;; (setup emacs :disabled
;;   (defvar +mode-line-format--default (get 'mode-line-format 'standard-value))

;;   (defgroup +mode-line-constructs nil
;;     "Mode line constructs.")

;;   (defun +mode-line--get-major-mode ()
;;     (format " %s "
;;             (capitalize
;;              (symbol-name major-mode))))

;;   (defcustom +mode-line-major-mode
;;     '(:eval
;;       (when (mode-line-window-selected-p)
;;         (list
;;          (propertize " λ" 'face 'shadow)
;;          (propertize (+mode-line--get-major-mode)
;;                      'face '((t))))))
;;     "Mode line construct for displaying the major mode."
;;     :group '+mode-line-constructs
;;     :risky t)

;;   (defun +mode-line--get-buffer-name ()
;;     (format " %s " (buffer-name)))

;;   (defcustom +mode-line-buffer-name
;;     '(:eval
;;       (when (mode-line-window-selected-p)
;;         (propertize (+mode-line--get-buffer-name)
;;                     'face '((t :inherit bold)))))
;;     "Mode line construct for displaying the buffer name."
;;     :group '+mode-line-constructs
;;     :risky t)

;;   (defvar +mode-line--to-format
;;     '(("%e"
;;        +mode-line-buffer-name)
;;       (+mode-line-major-mode)))

;;   (defun +mode-line--get-mode-line ()
;;     (let* ((left-str (car +mode-line--to-format))
;;            (right-str (cadr +mode-line--to-format))
;;            (reserve (length (format-mode-line (mapcar #'symbol-value (cadr +mode-line--to-format))))))
;;       `(,@left-str
;;         (:eval
;;          (propertize " "
;;                      'display `((space :align-to (- right ,right-len)))))
;;         ,@right-str)))

;;   (defun +mode-line--get-mode-line ()
;;     (let* ((left-str (car +mode-line--to-format))
;;            (right-str (cadr +mode-line--to-format))
;;            (reserve (length (format-mode-line right-str))))
;;       `(,@left-str
;;         (:eval
;;          (propertize " "
;;                      'display `((space :align-to (- right ,reserve)))))
;;         ,@right-str)))

;;   (setq-default mode-line-format (+mode-line--get-mode-line))

;;   (force-mode-line-update))

;;;; Scroll

;; Improve scroll
(setup emacs
  (global-so-long-mode 1)
  (:option scroll-preserve-screen-position t ; keep point in same position while scrolling
           scroll-conservatively 101 ; dont move cursor when point leaves the screen
           ;; scroll-conservatively 0      ; move cursor when point leaves the screen
           scroll-margin 3           ; scroll when 3 from end
           ;; scroll-margin 0           ; scroll when touch end
           scroll-step 1 ; smooth, step-by-step scrolling
           ;; scroll-step 0    ; no smoothing
           auto-window-vscroll nil        ; faster for long lines
           fast-but-imprecise-scrolling t ; faster for image and overlays
           mouse-wheel-scroll-amount
           '(2                                     ; faster vscroll speed
             ((shift) . hscroll)                   ; S-<scroll> for hscroll
             ((meta) . nil)                        ; M-<scroll> for PgUp/PgDn
             ((control) . text-scale)              ; C-<scroll> for zoom
             ((control meta) . global-text-scale)) ; C-M-<scroll> for global zoom
           mouse-wheel-scroll-amount-horizontal 2  ; faster hscroll speed
           ;; auto-window-vscroll nil ; TODO: what does this do?
           ))

;;;; Dashboard

(-setup dashboard
  (:require-self)
  (setq dashboard-items '((recents . 5)))
  (setq dashboard-center-content t)
  (when (< (length command-line-args) 2)
    (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
    (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
    (add-hook (+get-after-init-hook)
              (lambda ()
                (if (get-buffer "*Warnings*")
                    (setq initial-buffer-choice (lambda () (get-buffer "*Warnings*")))
                  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
                  (dashboard-insert-startupify-lists)
                  (dashboard-initialize))))))

;;;; prettify symbols

(setup prog-mode
  (global-prettify-symbols-mode 1))

;;;; fireplace

(-setup fireplace)

;;; end:

(provide '+ui)
;;; +ui.el ends here
