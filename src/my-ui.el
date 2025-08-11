(defvar +font-alist
  `((hack . "Hack")
    (tamzenPL-16
     . "-Misc-TamzenForPowerline-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (ttyp0-17   .   "-UW  -Ttyp0-regular-normal-normal-*-17-*-*-*-m-*-iso8859-1")
    (ttyp0-17-b .        "-UW-Ttyp0-bold-normal-normal-*-17-*-*-*-c-90-iso8859-1")
    (ttyp0-16   .   "-UW  -Ttyp0-regular-normal-normal-*-16-*-*-*-m-*-iso8859-1")
    (ttyp0-16-i .   "-UW  -Ttyp0-regular-italic-normal-*-16-*-*-*-m-*-iso10646-1")
    (gb-16 . "-AW-Greybeard 16px-regular-normal-normal-*-16-*-*-*-c-80-iso10646-1")
    (fira-code . "Fira Code")
    (maple-mono . "Maple Mono")))

(defun +get-font (font)
  (alist-get font +font-alist))

(defun +fontconfig ()
  "Set default font face."
  (let ((font
         'fira-code
         ;; 'ttyp0-16
         ;; 'fira-code
         ))
    (set-face-attribute 'default nil :font (+get-font font)))
  )

(+fontconfig)

;; HACK: fix bitmap fonts on emacsclient frames
(add-hook 'server-after-make-frame-hook #'+fontconfig)

;; all the icons
(leaf all-the-icons
  :config
  ;; Use 'prepend for the NS and Mac ports or Emacs will crash.
  (set-fontset-font t 'unicode (font-spec :family "all-the-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "file-icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Material Icons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "github-octicons") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "FontAwesome") nil 'append)
  (set-fontset-font t 'unicode (font-spec :family "Weather Icons") nil 'append))

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


;;; Function: sets a random theme.

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

(leaf whitespace :ensure nil
  :hook ((prog-mode-hook . +prog-mode-whitespace)
         (org-mode-hook  . +org-mode-whitespace)
         (text-mode-hook . +org-mode-whitespace))
  :init
  (defvar +base-whitespace-style '(face trailing tabs missing-newline-at-eof))
  (defun +prog-mode-whitespace ()
    (setq whitespace-style (append +base-whitespace-style
                                   '(tab-mark)))
    (whitespace-mode 1))
  (defun +org-mode-whitespace ()
    (setq whitespace-style (append +base-whitespace-style '()))
    (whitespace-mode 1))
  :config
  (setq whitespace-trailing 'whitespace-hspace))

(leaf solaire-mode
  :config
  (defun real-buffer-p ()
    (or (solaire-mode-real-buffer-p)
        (equal (buffer-name) "*dashboard*")))
  (setq solaire-mode-real-buffer-fn #'real-buffer-p)

  (solaire-global-mode +1))

;; show column # on modeline
(column-number-mode 1)

(leaf doom-modeline
  :config
  (doom-modeline-mode 1)
  ;; :config
  ;; (setq doom-modeline-modal-icon nil)
  )

;; Improve scroll
(leaf emacs :ensure nil
  :setq
  ;; (auto-window-vscroll nil) ; TODO: what does this do?
  (scroll-preserve-screen-position . t) ; keep point in same position while scrolling
  (scroll-conservatively . 101) ; dont move cursor to center while scrolling
  (scroll-margin . 2)           ; scroll margin of one line
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

(provide 'my-ui)
