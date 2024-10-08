;;; init.el --- The main init entry for Emacs -*- lexical-binding: t -*-
;;; Commentary:

;;; Code:

(require 'package)
(setq package-archives
      '(("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")
        ("melpa-stable" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/stable-melpa/")
        ("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")))
(package-initialize)

;; 安装 `use-package'
;; become built-in in Emacs 29.1
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 配置 `use-package'
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (if (daemonp)
      (setq use-package-always-demand t)))

(eval-when-compile
  (require 'use-package))

;; 安装 `use-package' 的集成模块
(use-package use-package-ensure-system-package
  :ensure t)
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

(use-package quelpa
  :ensure t
  :commands quelpa
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-update-melpa-p nil)
  (quelpa-self-upgrade-p nil)
  (quelpa-checkout-melpa-p nil))

;; `quelpa' 与 `use-package' 集成
(use-package quelpa-use-package
  :ensure t)
(use-package quelpa
  :ensure t)

;; 将lisp目录放到加载路径的前面以加快启动速度
(let ((dir (locate-user-emacs-file "lisp")))
  (add-to-list 'load-path (file-name-as-directory dir)))

;; 加载各模块化配置
;; 不要在`*message*'缓冲区显示加载模块化配置的信息
(with-temp-message ""
  (require 'init-ui) 
  (require 'init-base)
  (require 'init-edit) 
  (require 'init-org)
  (require 'init-completion)
  (require 'init-dev)
  )

;; F2 to open init file
(defun ddosvoid/open-init-file()
  (interactive)
  (find-file "~/.emacs.d/emacs-config.org"))
(global-set-key (kbd "<f2>") 'ddosvoid/open-init-file)

;; sudo find-file
(defun ddosvoid/sudo-edit-current-file ()
  (interactive)
  (when (buffer-file-name)
    (let ((old-point (point)))
      (find-file (concat "/sudo:root@localhost:" (buffer-file-name)))
      (goto-char old-point))))

;; set proxy in wsl2
(defvar default-proxy "172.26.0.1:7890")
(defun ddosvoid/proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" default-proxy)
    (message "No HTTP proxy")))

(defun ddosvoid/proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services
        `(("http" . ,default-proxy)
          ("https" . ,default-proxy)
          ("no_proxy" . "^\\(localhost\\|192.168.*\\|10.*\\)")))
  (ddosvoid/proxy-http-show))

(defun ddosvoid/proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (ddosvoid/proxy-http-show))

(defun ddosovid/proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (ddosvoid/proxy-http-disable)
    (ddosvoid/proxy-http-enable)))

(provide 'init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
