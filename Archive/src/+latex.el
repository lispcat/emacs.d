;;; +latex.el --- latex setup                        -*- lexical-binding: t; -*-

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

;;;; Latex Symbols (Quail)

;; latex symbols
(setup quail
  (eval-after-load "quail/latin-ltx"
    `(progn
       ,@(mapcar (lambda (bind)
                   `(quail-defrule ,(car bind) ,(cdr bind) "TeX"))
                 `(("\\lnt"   . ?¬)
                   ("\\land"  . ?∧)
                   ("\\lor"   . ?∨)
                   ("\\lev"   . ?≡)
                   ("\\nlev"  . ?≢)
                   ("\\lrarr" . ?↔)
                   ("\\bic"   . ?↔)
                   ("\\To"  . ?⇒)
                   ("\\allint" . ?ℤ)
                   ("\\tf" . ?∴)
                   ("\\isct" . ?∩)
                   ("\\ints" . ?∩)
                   ("\\union" . ?∪)
                   ("\\unn" . ?∪)
                   ("\\sst" . ?⊆)
                   ("\\psst" . ?⊂)
                   ("\\nin" . ?∉)
                   ("\\*" . ?·)
                   ("\\boxul"  . ?┌)    ; box upper-left
                   ("\\boxur"  . ?┐)    ; box upper-right
                   ("\\boxdl"  . ?└)    ; box down-left
                   ("\\boxdr"  . ?┘)    ; box down-right
                   ("\\boxh"   . ?─)    ; box horizontal
                   ("\\boxv"   . ?│)    ; box vertical
                   ("\\boxtd"  . ?┬)    ; box tee down
                   ("\\boxtu"  . ?┴)    ; box tee up
                   ("\\boxtr"  . ?├)    ; box tee right
                   ("\\boxtl"  . ?┤)    ; box tee left
                   ("\\boxc"   . ?┼)    ; box cross
                   ;; ("\\vec"   . ?̅)
                   ("\\vec"   . ?̄)
                   ("_y" . ?ᵧ)
                   )))))

;;;; Auctex

(-setup auctex
  (:load-after org)
  (:when-loaded
    ;; comment this out if using in-line latex
    (with-eval-after-load 'ox-latex
      (:option org-latex-compiler "lualatex"
               org-latex-pdf-process
               '("%latex -interaction nonstopmode -output-directory %o %f")))

    ;; larger previews
    (defun preview-larger-previews ()
      (setq preview-scale-function
            (lambda () (* 1.25 (funcall (preview-scale-from-face))))))
    (add-hook 'LaTeX-mode-hook #'preview-larger-previews)))


;;;; CDLaTeX

(-setup cdlatex
  (:load-after auctex)
  (:when-loaded
    (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)))

;;;; LaTeX preview pane (disabled)

(-setup latex-preview-pane :disabled
        (add-hook 'LaTeX-mode-hook (lambda () (latex-preview-pane-mode 1)))
        (:option pdf-latex-command "lualatex"
                 preview-orientation 'below))

;;;; ox-typst (typst exporter)

;; Documentation: https://github.com/jmpunkt/ox-typst

(-setup ox-typst
  (:load-after org))

;;; End:

(provide '+latex)
;;; +latex.el ends here
