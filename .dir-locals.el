;;; .dir_locals.el --- local settings                -*- lexical-binding: t; -*-

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

;;

;;; Code:

;; ((emacs-lisp-mode . ((eval . (when (and outline-indent-minor-mode
;;                                         (buffer-file-name))
;;                                (my/outline-setup-hide-on-focus 'emacs-lisp-mode))))))

;; ((emacs-lisp-mode . ((eval . (when (and outline-indent-minor-mode
;;                                         (buffer-file-name))
;;                                (setq-local outline-first-focus nil)
;;                                (add-hook 'window-selection-change-functions
;;                                          (lambda (frame)
;;                                            (when (and (not outline-first-focus)
;;                                                       (eq major-mode 'emacs-lisp-mode)
;;                                                       (eq (current-buffer) (window-buffer)))
;;                                              (setq-local outline-first-focus t)
;;                                              (outline-hide-all)))
;;                                          nil t))))))

;; ((emacs-lisp-mode (my/outline-hide-default . t)))

;;; .dir_locals.el ends here
