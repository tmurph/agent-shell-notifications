;;; agent-shell-notifications-osascript.el --- macOS osascript backend for agent-shell-notifications -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Zachary Hanham

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;;; Commentary:

;; macOS backend for agent-shell-notifications using `osascript' and the
;; AppleScript `display notification' command.
;;
;; This is a simple backend with no external dependencies beyond macOS
;; itself.  It supports :title and :body from the standard notification
;; plist.  Other fields are ignored because `display notification' does
;; not expose them:
;;
;;   - :app-icon    not supported
;;   - :timeout     not supported (macOS uses Notification Center settings)
;;   - :actions     not supported (clicking the notification opens Notification Center)
;;   - :on-action   not supported
;;
;; Because macOS does not return stable IDs for `display notification',
;; the close function is a no-op.  Notifications will remain in
;; Notification Center until dismissed by the user or cleared by the
;; system.

;;; Code:

(require 'subr-x)

(defun agent-shell-notifications--escape-applescript-string (s)
  "Escape string S for use as an AppleScript string literal."
  (concat "\"" (string-replace "\"" "\\\"" s) "\""))

(defun agent-shell-notifications--send-osascript (plist)
  "Send a notification described by PLIST via `osascript'.
PLIST must contain at least :title; :body is optional."
  (when-let ((title (plist-get plist :title)))
    (let* ((body (or (plist-get plist :body) ""))
           (script (format "display notification %s with title %s"
                           (agent-shell-notifications--escape-applescript-string body)
                           (agent-shell-notifications--escape-applescript-string title)))
           (exit-code (call-process "osascript" nil nil nil "-e" script)))
      (when (zerop exit-code)
        t))))

(defun agent-shell-notifications--close-osascript (_id)
  "Close a notification by ID.
This is a no-op because `osascript' notifications do not expose stable IDs
that can be dismissed programmatically."
  nil)

(defun agent-shell-notifications-osascript-setup ()
  "Configure agent-shell-notifications to use the osascript backend."
  (setq agent-shell-notifications-transform-timeout-function #'identity)
  (setq agent-shell-notifications-transform-function
        (lambda (plist)
          "Keep only keys supported by the osascript backend."
          (list :title (plist-get plist :title)
                :body (plist-get plist :body))))
  (setq agent-shell-notifications-send-function
        #'agent-shell-notifications--send-osascript)
  (setq agent-shell-notifications-close-function
        #'agent-shell-notifications--close-osascript))

(agent-shell-notifications-osascript-setup)

(provide 'agent-shell-notifications-osascript)

;;; agent-shell-notifications-osascript.el ends here
