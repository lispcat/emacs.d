;;; my-init-requirements.el --- required packages for the rest

;;; Commentary:

;;; Code:

(use-package general :ensure (:wait t)
  :demand t
  :config
  (general-create-definer general-my-map
    :prefix "C-c"))

(use-package diminish :ensure (:wait t)
  :demand t)

(use-package which-key :ensure (:wait t)
  :demand t
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3)
  (which-key-mode 1))

(use-package hydra :ensure (:wait t)
  :demand t)

(provide 'my-init-requirements)

;;; my-init-requirements.el ends here
