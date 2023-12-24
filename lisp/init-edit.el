;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 不自动备份
(setq make-backup-files nil)
;; 不使用Emacs自带的自动保存
(setq auto-save-default nil)

;; Directly modify when selecting text
(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :bind ("s-u" . revert-buffer)
  :custom
  (auto-revert-interval 10)
  (auto-revert-avoid-polling t)
  (auto-revert-verbose nil)
  (auto-revert-remote-files t)
  (auto-revert-check-vc-info t)
  (global-auto-revert-non-file-buffers t))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
