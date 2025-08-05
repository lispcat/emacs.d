;;; early-init.el ---                                -*- lexical-binding: t; -*-

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

;; The first file loaded at startup.
;;
;; - loaded before gui creation.
;;   - disable gui components here.
;;
;; Sets the following:
;; - startup variables
;; - dirs
;; - UI
;;   - gui
;;   - transparency
;; - eln-cache
;;

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                startup vars                                ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; set startup gc
(setq gc-cons-threshold (* 50 1000 1000))
;; dont load package.el
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                    dirs                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     UI                                     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  eln-cache                                 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; changes the eln-cache dir to be inside a subdir for cleanliness
(when (and (fboundp 'startup-redirect-eln-cache)
           (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" my/emacs-local-dir))))

(setenv "LSP_USE_PLISTS" "true")
(setq lsp-use-plists t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                     end                                    ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'early-init)
;;; early-init.el ends here
