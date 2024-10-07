;;; init-edit.el --- Editing settings -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

;; 不自动备份
(setq make-backup-files nil)
;; 不使用Emacs自带的自动保存
(setq auto-save-default nil)

(pixel-scroll-precision-mode t)

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

(use-package avy
  :ensure t
  :bind (("C-." . avy-goto-char-timer)
         :map isearch-mode-map
         ("C-." . avy-isearch))
  :config
  ;; Make `avy-goto-char-timer' support pinyin, refer to:
  ;; https://emacs-china.org/t/avy-avy-goto-char-timer/20900/2
  (defun my/avy-goto-char-timer (&optional arg)
    "Make avy-goto-char-timer support pinyin"
    (interactive "P")
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
        (setq avy--old-cands (avy--read-candidates
                              'pinyinlib-build-regexp-string))
        (avy-process avy--old-cands))))

  (defun avy-action-kill-whole-line (pt)
    "avy action: kill the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    "avy action: copy the whole line where avy selection is"
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    "avy action: copy the line where avy selection is and paste to current point"
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    "avy action: kill the line where avy selection is and paste to current point"
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-helpful (pt)
    "avy action: get helpful information at point"
    (save-excursion
      (goto-char pt)
      (helpful-at-point))
    t)

  (defun avy-action-mark-to-char (pt)
    "avy action: mark from current point to avy selection"
    (activate-mark)
    (goto-char pt))

  (defun avy-action-flyspell (pt)
    "avy action: flyspell the word where avy selection is"
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-correct-wrapper))))

  (defun avy-action-define (pt)
    "avy action: define the word in dictionary where avy selection is"
    (save-excursion
      (goto-char pt)
      (fanyi-dwim2)))

  (defun avy-action-embark (pt)
    "avy action: embark where avy selection is"
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (defun avy-action-google (pt)
    "avy action: google the avy selection when it is a word or browse it when it is a link"
    (save-excursion
      (goto-char pt)
      (my/search-or-browse)))

  (setf (alist-get ?k avy-dispatch-alist) 'avy-action-kill-stay
        (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line
        (alist-get ?w avy-dispatch-alist) 'avy-action-copy
        (alist-get ?W avy-dispatch-alist) 'avy-action-copy-whole-line
        (alist-get ?y avy-dispatch-alist) 'avy-action-yank
        (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line
        (alist-get ?t avy-dispatch-alist) 'avy-action-teleport
        (alist-get ?T avy-dispatch-alist) 'avy-action-teleport-whole-line
        (alist-get ?H avy-dispatch-alist) 'avy-action-helpful
        (alist-get ?  avy-dispatch-alist) 'avy-action-mark-to-char
        (alist-get ?\; avy-dispatch-alist) 'avy-action-flyspell
        (alist-get ?= avy-dispatch-alist) 'avy-action-define
        (alist-get ?o avy-dispatch-alist) 'avy-action-embark
        (alist-get ?G avy-dispatch-alist) 'avy-action-google
        )

  :custom
  (avy-timeout-seconds 0.5)
  (avy-all-windows t)
  (avy-background t)
  (avy-keys '(?a ?s ?d ?f ?g ?h ?j ?l ?q ?e ?r ?u ?i ?p ?n))
  )

(use-package symbol-overlay
  :ensure t
  :init
  (global-set-key (kbd "M-i") 'symbol-overlay-put)
  (global-set-key (kbd "M-n") 'symbol-overlay-switch-forward)
  (global-set-key (kbd "M-p") 'symbol-overlay-switch-backward)
  (global-set-key (kbd "<f7>") 'symbol-overlay-mode)
  (global-set-key (kbd "<f8>") 'symbol-overlay-remove-all))

(use-package thing-edit
  :ensure t
  :quelpa (thing-edit :fetcher github :repo "manateelazycat/thing-edit"))

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
