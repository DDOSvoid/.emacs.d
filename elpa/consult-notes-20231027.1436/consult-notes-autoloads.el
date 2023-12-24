;;; consult-notes-autoloads.el --- automatically extracted autoloads (do not edit)   -*- lexical-binding: t -*-
;; Generated by the `loaddefs-generate' function.

;; This file is part of GNU Emacs.

;;; Code:

(add-to-list 'load-path (or (and load-file-name (directory-file-name (file-name-directory load-file-name))) (car load-path)))



;;; Generated autoloads from consult-notes.el

(autoload 'consult-notes--file-dir-source "consult-notes" "\
Generate the notes source for each directory of files in `consult-notes-dir-sources'.

 Return a notes source list suitable for `consult--multi'.
NAME is the source name, CHAR is the narrowing character,
and DIR is the directory to find notes.

(fn NAME CHAR DIR)")
(autoload 'consult-notes--file-dir-annotate "consult-notes" "\
Annotate file CAND with its directory DIR, size, and modification time.

(fn NAME DIR CAND)")
(autoload 'consult-notes--make-file-dir-sources "consult-notes" "\
Add generated `consult--multi' sources to list of all sources.")
(defvar consult-notes-denote-mode nil "\
Non-nil if Consult-Notes-Denote mode is enabled.
See the `consult-notes-denote-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `consult-notes-denote-mode'.")
(custom-autoload 'consult-notes-denote-mode "consult-notes" nil)
(autoload 'consult-notes-denote-mode "consult-notes" "\
Toggle `consult-notes-denote-mode' to integrate consult with denote.

This is a global minor mode.  If called interactively, toggle the
`Consult-Notes-Denote mode' mode.  If the prefix argument is
positive, enable the mode, and if it is zero or negative, disable
the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='consult-notes-denote-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(defvar consult-notes-org-roam-mode nil "\
Non-nil if Consult-Notes-Org-Roam mode is enabled.
See the `consult-notes-org-roam-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `consult-notes-org-roam-mode'.")
(custom-autoload 'consult-notes-org-roam-mode "consult-notes" nil)
(autoload 'consult-notes-org-roam-mode "consult-notes" "\
Toggle `consult-notes-org-roam-mode' to integrate consult with org-roam.

By enabling `consult-notes-org-roam-mode' the functions
`org-roam-node-read' and `org-roam-ref-read' are overriden by
consult-notes' org-roam equivalents. Optional argument ARG indicates
whether the mode should be enabled or disabled.

(fn &optional ARG)" t)
(defvar consult-notes-org-headings-mode nil "\
Non-nil if Consult-Notes-Org-Headings mode is enabled.
See the `consult-notes-org-headings-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `consult-notes-org-headings-mode'.")
(custom-autoload 'consult-notes-org-headings-mode "consult-notes" nil)
(autoload 'consult-notes-org-headings-mode "consult-notes" "\
Toggle `consult-notes-org-headings-mode'.

This is a global minor mode.  If called interactively, toggle the
`Consult-Notes-Org-Headings mode' mode.  If the prefix argument
is positive, enable the mode, and if it is zero or negative,
disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `(default-value \\='consult-notes-org-headings-mode)'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

(fn &optional ARG)" t)
(autoload 'consult-notes-search-in-all-notes "consult-notes" "\
Search in all notes using `grep' or `ripgrep'.
Which search function is used depends on the value of `consult-notes-use-rg'." t)
(autoload 'consult-notes "consult-notes" "\
Find a file in a notes directory with consult-multi, or from SOURCES.

(fn &optional SOURCES)" t)
(register-definition-prefixes "consult-notes" '("consult-notes-"))


;;; Generated autoloads from consult-notes-denote.el

(register-definition-prefixes "consult-notes-denote" '("consult-notes-denote-"))


;;; Generated autoloads from consult-notes-org-headings.el

(register-definition-prefixes "consult-notes-org-headings" '("consult-"))


;;; Generated autoloads from consult-notes-org-roam.el

(autoload 'consult-notes-org-roam-find-node-relation "consult-notes-org-roam" "\
Navigate org-roam notes by link relation.

With universal ARG tries to navigate the tags of the current
note. Optionally takes a selected NODE and filepaths CHOICES.

(fn ARG &optional NODE CHOICES)" t)
(register-definition-prefixes "consult-notes-org-roam" '("consult-notes-org-roam-"))

;;; End of scraped data

(provide 'consult-notes-autoloads)

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; no-native-compile: t
;; coding: utf-8-emacs-unix
;; End:

;;; consult-notes-autoloads.el ends here