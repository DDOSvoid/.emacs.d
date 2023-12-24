;;; init-dev.el --- Development settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(use-package magit
  :ensure t
  :hook (git-commit-mode . flyspell-mode)
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch)
         ("C-c M-g" . magit-file-dispatch))
  :custom
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t))

(provide 'init-dev)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dev.el ends here
