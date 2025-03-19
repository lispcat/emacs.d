
(leaf omni-quotes
  :init (omni-quotes-mode 1)
  :bind (("M-s q m" . omni-quotes-mode)
         ("M-s q p" . omni-quotes-prev-set)
         ("M-s q n" . omni-quotes-next-set)
         ("M-s q s" . omni-quotes-shuffle-current-set)
         ("M-s q q" . omni-quotes-display-random-quote))
  :setq
  (omni-quotes-idle-interval . 60)
  (omni-quotes-fading . t)
  (omni-quotes-fading-delay . 30)
  :config
  (omni-quotes-load-simple-quote-file "~/Notes/org/quotes.txt" "personal"))


;;; Packages to install (check source code for malware for each first):

;; bind-map

;; restart-emacs

;; golden-ratio

;; ws-butler

;; flycheck-package

;; org-present

;; emacs-purpose

;; origami

;; cask

;; tao-theme

;; emacs-color-themes

;; hydra !!! so good
;; https://github.com/abo-abo/hydra/wiki/Hydras-by-Topic

;; literate programming?

;;

