;;; keycast-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "keycast" "keycast.el" (0 0 0 0))
;;; Generated autoloads from keycast.el

(defvar keycast-mode-line-mode nil "\
Non-nil if Keycast-Mode-Line mode is enabled.
See the `keycast-mode-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-mode-line-mode'.")

(custom-autoload 'keycast-mode-line-mode "keycast" nil)

(autoload 'keycast-mode-line-mode "keycast" "\
Show current command and its key binding in the mode line.

If called interactively, enable Keycast-Mode-Line mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar keycast-header-line-mode nil "\
Non-nil if Keycast-Header-Line mode is enabled.
See the `keycast-header-line-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-header-line-mode'.")

(custom-autoload 'keycast-header-line-mode "keycast" nil)

(autoload 'keycast-header-line-mode "keycast" "\
Show current command and its key binding in the header line.

If called interactively, enable Keycast-Header-Line mode if ARG
is positive, and disable it if ARG is zero or negative.  If
called from Lisp, also enable the mode if ARG is omitted or nil,
and toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar keycast-tab-bar-mode nil "\
Non-nil if Keycast-Tab-Bar mode is enabled.
See the `keycast-tab-bar-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-tab-bar-mode'.")

(custom-autoload 'keycast-tab-bar-mode "keycast" nil)

(autoload 'keycast-tab-bar-mode "keycast" "\
Show current command and its key binding in the tab bar.

If called interactively, enable Keycast-Tab-Bar mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(defvar keycast-log-mode nil "\
Non-nil if Keycast-Log mode is enabled.
See the `keycast-log-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `keycast-log-mode'.")

(custom-autoload 'keycast-log-mode "keycast" nil)

(autoload 'keycast-log-mode "keycast" "\
Log invoked commands and their key bindings in a buffer.

If called interactively, enable Keycast-Log mode if ARG is
positive, and disable it if ARG is zero or negative.  If called
from Lisp, also enable the mode if ARG is omitted or nil, and
toggle it if ARG is `toggle'; disable the mode otherwise.

\(fn &optional ARG)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "keycast" '("keycast-")))

;;;***

;;;### (autoloads nil nil ("keycast-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; keycast-autoloads.el ends here
