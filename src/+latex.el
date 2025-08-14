(leaf quail :ensure nil
  :config
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
                   ("\\boxul"  . ?┌)   ; box upper-left
                   ("\\boxur"  . ?┐)   ; box upper-right
                   ("\\boxdl"  . ?└)   ; box down-left
                   ("\\boxdr"  . ?┘)   ; box down-right
                   ("\\boxh"   . ?─)   ; box horizontal
                   ("\\boxv"   . ?│)   ; box vertical
                   ("\\boxtd"  . ?┬)   ; box tee down
                   ("\\boxtu"  . ?┴)   ; box tee up
                   ("\\boxtr"  . ?├)   ; box tee right
                   ("\\boxtl"  . ?┤)   ; box tee left
                   ("\\boxc"   . ?┼)   ; box cross
                   )))))

(leaf auctex
  :require t
  :hook ((LaTeX-mode-hook . preview-larger-previews))
  :config
  (with-eval-after-load 'ox-latex
    (setq org-latex-compiler "lualatex")
    (setq org-latex-pdf-process '("%latex -interaction nonstopmode -output-directory %o %f")))
  (defun preview-larger-previews ()
    (setq preview-scale-function
          (lambda () (* 1.25 (funcall (preview-scale-from-face)))))))

(leaf cdlatex
  :after auctex
  :hook ((LaTeX-mode-hook . turn-on-cdlatex)))

(leaf latex-preview-pane
  :init
  (add-hook 'LaTeX-mode-hook (lambda () (latex-preview-pane-mode 1)))
  :config
  (setq pdf-latex-command "lualatex")
  (setq preview-orientation 'below)
  )

(provide '+latex)
