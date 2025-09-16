
;;; Fontconfig

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

;;; All the icons

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

;;; Ligatures

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

;;; Function: `load-theme' but fixed theme-bleeding issue.

;;; obsolete, replaced with consult-theme.

;; (defun +load-theme (theme &optional no-confirm no-enable)
;;   "Prevent `load-theme' from having theme-bleeding issues.
;; The args THEME, NO-CONFIRM and NO-ENABLE are passed to `load-theme'."
;;   (interactive
;;    (list
;;     (intern (completing-read "Load custom theme: "
;;                              (mapcar #'symbol-name
;;                                      (custom-available-themes))))
;;     nil nil))
;;   ;; disable all enabled themes
;;   (mapc #'disable-theme custom-enabled-themes)
;;   ;; enable theme
;;   (if (custom-theme-p theme)
;;       (enable-theme theme)
;;     (load-theme theme :no-confirm))
;;   )


;;; Themes
;;;; Function: sets a random theme.

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

(leaf emacs :elpaca nil
  :preface
  (leaf kaolin-themes
    :require t)
  (leaf ef-themes
    :require t)
  (leaf doom-themes
    :require t
    :setq
    (doom-themes-enable-bold   . t) ; if nil, bold is universally disabled
    (doom-themes-enable-italic . t) ; if nil, italics is universally disabled
    )

  :leaf-defer nil
  :bind
  ("C-c T t" . consult-theme)
  ("C-c T r" . +set-random-theme))

(leaf emacs :elpaca nil
  :after doom-themes kaolin-themes ef-themes
  :config
  (+set-random-theme))

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

;; list of programming modes to disable line-numbers on
(defvar +display-line-numbers-exclude '())

;; enable line-numbers on programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (memq major-mode +display-line-numbers-exclude)
              (display-line-numbers-mode 1))))

(setq display-line-numbers-type 'relative)

(global-visual-line-mode 1)
(diminish 'visual-line-mode) ; hide "Wrap" in mode-line

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

;; (leaf solaire-mode
;;   :config
;;   (defun real-buffer-p ()
;;     (or (solaire-mode-real-buffer-p)
;;         (equal (buffer-name) "*dashboard*")))
;;   (setq solaire-mode-real-buffer-fn #'real-buffer-p)

;;   (solaire-global-mode +1))

;; show column # on modeline
(column-number-mode 1)

(-setup doom-modeline :disabled
  (:option doom-modeline-height 40
           doom-modeline-icon t)
  (doom-modeline-mode 1)
  (setq-default header-line-format '("%e" (:eval (doom-modeline-format--main))))
  (setq-default mode-line-format nil)
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (setq-default mode-line-format nil)
              (dolist (buf (buffer-list))
                (with-current-buffer buf
                  (when mode-line-format
                    (setq mode-line-format nil))))))

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

(-setup doom-modeline :disabled
  (:require-self))

(-setup mood-line :disabled
  (mood-line-mode 1)
  (:option mood-line-glyph-alist mood-line-glyphs-fira-code
           ;; normal - doom-modeline-info
           ;; insert - font-lock-keyword-face
           ;; emacs - font-lock-builtin-face
           ;; motion - font-lock-doc-face (:slant normal)
           ;; beacon - mode-line, doom-modeline-warning, doom-modeline-urgent, doom-modeline-warning
           mood-line-segment-modal-meow-state-alist
           '((normal "<N>" .
                     font-lock-variable-name-face
                     ;; success
                     )
             (insert "<I>" .
                     font-lock-string-face
                     ;; font-lock-keyword-face
                     )
             (keypad "<K>" . font-lock-keyword-face)
             (beacon "<B>" .
                     font-lock-type-face
                     )
             (motion "<M>" . font-lock-constant-face))
           mood-line--settings-alist
           '((anzu-cons-mode-line-p
              . nil)
             (header-line-format
              . (:eval (mood-line--process-format mood-line-format)))
             (mode-line-format
              . nil))))

;; nano theme and modeline

(-setup (nano-theme :host github :repo "rougier/nano-theme"))
;; (-setup (nano-modeline :host github :repo "rougier/nano-modeline")
;;   (:require-self)
;;   ;; (:load-after nano-theme)
;;   (setq-default mode-line-format nil)
;;   (defun +nano-modeline-setup-faces ()
;;     (face-spec-set
;;      'nano-modeline-active
;;      `((t (:foreground ,(face-foreground 'default)
;;                        :background ,(face-background 'mode-line nil t)
;;                        :box (:line-width 1 :color ,(face-background
;;                                                     'default))))))
;;     (face-spec-set
;;      'nano-modeline-inactive
;;      `((t (:foreground ,(face-foreground 'default)
;;                        :background ,(face-background 'mode-line-inactive nil t)
;;                        :box (:line-width 1 :color ,(face-background
;;                                                     'default)))))))
;;   (+nano-modeline-setup-faces)
;;   (add-hook '+after-enable-theme-hook #'+nano-modeline-setup-faces)

;;   ;;   ;; (setq nano-modeline-position )
;;   ;;   (add-hook 'prog-mode-hook            #'nano-modeline-prog-mode)
;;   ;;   (add-hook 'text-mode-hook            #'nano-modeline-text-mode)
;;   ;;   (add-hook 'org-mode-hook             #'nano-modeline-org-mode)
;;   ;;   (add-hook 'pdf-view-mode-hook        #'nano-modeline-pdf-mode)
;;   ;;   (add-hook 'mu4e-headers-mode-hook    #'nano-modeline-mu4e-headers-mode)
;;   ;;   (add-hook 'mu4e-view-mode-hook       #'nano-modeline-mu4e-message-mode)
;;   ;;   (add-hook 'elfeed-show-mode-hook     #'nano-modeline-elfeed-entry-mode)
;;   ;;   (add-hook 'elfeed-search-mode-hook   #'nano-modeline-elfeed-search-mode)
;;   ;;   (add-hook 'term-mode-hook            #'nano-modeline-term-mode)
;;   ;;   (add-hook 'xwidget-webkit-mode-hook  #'nano-modeline-xwidget-mode)
;;   ;;   (add-hook 'messages-buffer-mode-hook #'nano-modeline-message-mode)
;;   ;;   (add-hook 'org-capture-mode-hook     #'nano-modeline-org-capture-mode)
;;   ;; (add-hook 'org-agenda-mode-hook      #'nano-modeline-org-agenda-mode)
;;   )

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

;; (-setup spacious-padding
;;   (spacious-padding-mode 0))

;; Improve scroll
(leaf emacs :elpaca nil
  :setq
  ;; (auto-window-vscroll nil) ; TODO: what does this do?
  (scroll-preserve-screen-position . t) ; keep point in same position while scrolling
  (scroll-conservatively . 101)     ; dont move cursor to center while scrolling
  (scroll-margin . 2)               ; scroll margin of one line
  (mouse-wheel-scroll-amount
   . '(2                                      ; faster vscroll speed
       ((shift) . hscroll)                    ; S-<scroll> for hscroll
       ((meta) . nil)                         ; M-<scroll> for PgUp/PgDn
       ((control) . text-scale)               ; C-<scroll> for zoom
       ((control meta) . global-text-scale))) ; C-M-<scroll> for global zoom
  (mouse-wheel-scroll-amount-horizontal . 2)) ; faster hscroll speed

(leaf dashboard
  :require t
  :config
  (setq dashboard-center-content t)
  (when (< (length command-line-args) 2)
    (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)
    (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
    (add-hook 'elpaca-after-init-hook
              (lambda ()
                (if (get-buffer "*Warnings*")
                    (setq initial-buffer-choice (lambda () (get-buffer "*Warnings*")))
                  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
                  (dashboard-insert-startupify-lists)
                  (dashboard-initialize))))))

(global-prettify-symbols-mode 1)

(provide '+ui)
