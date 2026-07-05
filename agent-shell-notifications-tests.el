;;; agent-shell-notifications-tests.el --- Tests for agent-shell-notifications -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for agent-shell-notifications.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)

;; ---------------------------------------------------------------------------
;; Stub external dependencies so the file loads without agent-shell
;; ---------------------------------------------------------------------------

(unless (featurep 'agent-shell)
  (defvar agent-shell--state nil)
  (defun agent-shell-subscribe-to (&rest _args) nil)
  (defun agent-shell-unsubscribe (&rest _args) nil)
  (defun agent-shell--shorten-paths (s) s)
  (defun agent-shell--fetch-agent-icon (name) (concat "/icons/" name))
  (defun agent-shell--send-command (&rest _args) nil)
  (provide 'agent-shell))

(unless (featurep 'agent-shell-viewport)
  (defun agent-shell-viewport--buffer (&rest _args) nil)
  (defun agent-shell-viewport--shell-buffer (&optional _buf) nil)
  (defun agent-shell-viewport-reply (&rest _args) nil)
  (provide 'agent-shell-viewport))

;; Prevent loading the real libnotify provider (requires D-Bus).
;; Also pre-set send/close functions so the load-time set-provider call passes validation.
(setq agent-shell-notifications-provider nil)
(setq agent-shell-notifications-send-function #'ignore)
(setq agent-shell-notifications-close-function #'ignore)

(require 'agent-shell-notifications)

;; ---------------------------------------------------------------------------
;; Test provider
;; ---------------------------------------------------------------------------

(defvar asn-test--send-calls nil
  "List of plists passed to the test send function.")

(defvar asn-test--send-next-id 1
  "Next ID the test send function will return.")

(defvar asn-test--close-calls nil
  "List of IDs passed to the test close function.")

(defun asn-test--send (plist)
  "Test provider: record PLIST and return an auto-incrementing ID."
  (push plist asn-test--send-calls)
  (prog1 asn-test--send-next-id
    (cl-incf asn-test--send-next-id)))

(defun asn-test--close (id)
  "Test provider: record ID."
  (push id asn-test--close-calls))

;; ---------------------------------------------------------------------------
;; Test helpers
;; ---------------------------------------------------------------------------

(defvar asn-test--timers-created nil
  "List of (:secs SECS :func FUNC) plists from mock `run-with-timer'.")

(defvar asn-test--timers-cancelled nil
  "List of timer objects passed to mock `cancel-timer'.")

(defun asn-test--make-permission-event (&optional request-id kind title)
  "Create a permission-request event."
  (list :data (list :request-id (or request-id "req-1")
                    :tool-call (list :kind (or kind "read")
                                     :title (or title "Read foo.el")))))

(defun asn-test--make-permission-response-event (&optional request-id)
  "Create a permission-response event."
  (list :data (list :request-id (or request-id "req-1"))))

(defun asn-test--make-turn-complete-event (&optional stop-reason)
  "Create a turn-complete event."
  (list :data (list :stop-reason (or stop-reason "end_turn"))))

(defmacro asn-test--with-shell-buffer (&rest body)
  "Execute BODY in a temp buffer with test provider and mocked timers."
  (declare (indent 0) (debug t))
  `(let ((agent-shell-notifications--subscriptions nil)
         (agent-shell-notifications--timers nil)
         (agent-shell-notifications--notification-ids nil)
         (agent-shell-notifications-send-function #'asn-test--send)
         (agent-shell-notifications-close-function #'asn-test--close)
         (agent-shell-notifications-inhibit-functions nil)
         (agent-shell-notifications-transform-timeout-function #'identity)
         (agent-shell-notifications-transform-function #'identity)
         (agent-shell-notifications-format-function
          #'agent-shell-notifications--format-default)
         (agent-shell-notifications-shell-visible-function (lambda (_) nil))
         (agent-shell-notifications-immediate-notifications t)
         (agent-shell-notifications-idle-notifications '(permission-request))
         (agent-shell-notifications-idle-timeout 10)
         (agent-shell-notifications-timeout 0)
         (asn-test--send-calls nil)
         (asn-test--send-next-id 1)
         (asn-test--close-calls nil)
         (asn-test--timers-created nil)
         (asn-test--timers-cancelled nil))
     (cl-letf (((symbol-function 'cancel-timer)
                (lambda (timer) (push timer asn-test--timers-cancelled)))
               ((symbol-function 'run-with-timer)
                (lambda (secs _repeat func &rest _args)
                  (let ((timer (list :secs secs :func func)))
                    (push timer asn-test--timers-created)
                    timer))))
       (with-temp-buffer
         ,@body))))

(defmacro asn-test--with-visible-shell (&rest body)
  "Like `asn-test--with-shell-buffer' but shell reports as visible."
  (declare (indent 0) (debug t))
  `(asn-test--with-shell-buffer
     (setq agent-shell-notifications-shell-visible-function (lambda (_) t))
     ,@body))

;; ---------------------------------------------------------------------------
;; Tests: should-notify-p
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-should-notify-p ()
  "should-notify-p dispatches on setting type: t, nil, list, function."
  (should (agent-shell-notifications--should-notify-p t 'permission-request nil))
  (should-not (agent-shell-notifications--should-notify-p nil 'permission-request nil))
  (should (agent-shell-notifications--should-notify-p
           '(permission-request) 'permission-request nil))
  (should-not (agent-shell-notifications--should-notify-p
               '(permission-request) 'turn-complete nil))
  (should (agent-shell-notifications--should-notify-p
           (lambda (type _ev) (eq type 'turn-complete)) 'turn-complete nil))
  (should-not (agent-shell-notifications--should-notify-p
               (lambda (type _ev) (eq type 'turn-complete)) 'permission-request nil)))

;; ---------------------------------------------------------------------------
;; Tests: resolve-timeout
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-resolve-timeout ()
  "resolve-timeout handles number, alist (with default fallback), and function."
  (should (= 5 (agent-shell-notifications--resolve-timeout
                 5 'permission-request nil)))
  (should (= 3 (agent-shell-notifications--resolve-timeout
                 '((permission-request . 3) (turn-complete . 1))
                 'permission-request nil)))
  (should (= 3 (agent-shell-notifications--resolve-timeout
                 '((permission-request . 1) (default . 0.5) (turn-complete . 3))
                 'turn-complete nil)))
  (should (= 0.5 (agent-shell-notifications--resolve-timeout
                   '((default . 0.5)) 'turn-complete nil)))
  (let ((messages nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (should (= 0 (agent-shell-notifications--resolve-timeout
                    '((permission-request . 3)) 'turn-complete nil)))
      (should (string-match-p "no timeout configured for.*turn-complete" (car messages)))))
  (should (= 42 (agent-shell-notifications--resolve-timeout
                  (lambda (_type _event) 42) 'turn-complete nil))))

;; ---------------------------------------------------------------------------
;; Tests: format-default
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-format-default/permission-request ()
  "format-default returns :title with capitalized kind and :body with formatted message."
  (let ((result (agent-shell-notifications--format-default
                 'permission-request
                 (asn-test--make-permission-event "r1" "edit" "Edit README.md"))))
    (should (equal "Agent-shell: Edit" (plist-get result :title)))
    (should (equal "README.md" (plist-get result :body)))))

(ert-deftest asn-test-format-default/turn-complete ()
  "format-default returns Ready or Stopped based on stop-reason."
  (should (equal "Agent-shell: Ready"
                 (plist-get (agent-shell-notifications--format-default
                             'turn-complete
                             (asn-test--make-turn-complete-event "end_turn"))
                            :title)))
  (should (equal "Agent-shell: Stopped"
                 (plist-get (agent-shell-notifications--format-default
                             'turn-complete
                             (asn-test--make-turn-complete-event "error"))
                            :title))))

;; ---------------------------------------------------------------------------
;; Tests: format-permission-message
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-format-permission-message ()
  "format-permission-message handles read (shorten), execute (truncate), unknown (passthrough)."
  ;; execute: truncates to 50 chars
  (let* ((long-cmd (make-string 60 ?x))
         (result (agent-shell-notifications--format-permission-message
                  (list :kind "execute" :title (concat "Execute " long-cmd)))))
    (should (= 50 (length result)))
    (should (string-suffix-p "..." result)))
  ;; execute: multiline takes first line
  (should (equal "line1"
                 (agent-shell-notifications--format-permission-message
                  '(:kind "execute" :title "Execute line1\nline2"))))
  ;; unknown kind: passthrough with prefix stripped
  (should (equal "do something"
                 (agent-shell-notifications--format-permission-message
                  '(:kind "custom" :title "Custom do something")))))

;; ---------------------------------------------------------------------------
;; Tests: notification routing (schedule-or-notify)
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-routing/not-visible-notifies-immediately ()
  "When shell is not visible, send notification immediately with no timer."
  (asn-test--with-shell-buffer
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 1 (length asn-test--send-calls)))
    (should (= 0 (length asn-test--timers-created)))))

(ert-deftest asn-test-routing/visible-schedules-idle-timer ()
  "When shell is visible and idle enabled for type, schedule a timer that fires notification."
  (asn-test--with-visible-shell
    (setq agent-shell-notifications-idle-timeout 15)
    (let ((buf (current-buffer)))
      (agent-shell-notifications--schedule-or-notify
       'permission-request buf (asn-test--make-permission-event "req-idle"))
      ;; No immediate notification, one timer created
      (should (= 0 (length asn-test--send-calls)))
      (should (= 1 (length asn-test--timers-created)))
      (should (= 15 (plist-get (car asn-test--timers-created) :secs)))
      ;; Fire the timer callback
      (funcall (plist-get (car asn-test--timers-created) :func))
      ;; Now notification was sent and ID tracked
      (should (= 1 (length asn-test--send-calls)))
      (should (= 0 (length agent-shell-notifications--timers)))
      (should (= 1 (cdr (assoc "req-idle"
                                (map-elt agent-shell-notifications--notification-ids buf))))))))

(ert-deftest asn-test-routing/idle-notifications-disabled ()
  "When idle notifications setting does not match or is nil, nothing is sent even when  visible."
  (asn-test--with-visible-shell
   (setq agent-shell-notifications-idle-notifications nil)
    (agent-shell-notifications--schedule-or-notify
     'turn-complete (current-buffer) (asn-test--make-turn-complete-event))
    (should (= 0 (length asn-test--send-calls)))
    (should (= 0 (length asn-test--timers-created))))
  (asn-test--with-visible-shell
   (setq agent-shell-notifications-idle-notifications '(permission-request))
    (agent-shell-notifications--schedule-or-notify
     'turn-complete (current-buffer) (asn-test--make-turn-complete-event))
    (should (= 0 (length asn-test--send-calls)))
    (should (= 0 (length asn-test--timers-created)))))

(ert-deftest asn-test-routing/immediate-notifications-disabled ()
  "When immediate notifications setting does not match or is nil, nothing is sent even when not visible."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-immediate-notifications nil)
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 0 (length asn-test--send-calls))))
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-immediate-notifications '(turn-complete))
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 0 (length asn-test--send-calls)))))

(ert-deftest asn-test-routing/send-nil-return-not-tracked ()
  "When send function returns nil, no notification ID is tracked."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-send-function (lambda (_plist) nil))
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should-not (map-elt agent-shell-notifications--notification-ids
                         (current-buffer)))))

(ert-deftest asn-test-routing/notification-plist-contains-expected-keys ()
  "The plist passed to send-function contains :title, :timeout, :actions, :on-action."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-timeout 5)
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer)
     (asn-test--make-permission-event "r1" "edit" "Edit README.md"))
    (let ((plist (car asn-test--send-calls)))
      (should (equal "Agent-shell: Edit" (plist-get plist :title)))
      (should (equal "README.md" (plist-get plist :body)))
      (should (= 5 (plist-get plist :timeout)))
      (should (plist-get plist :actions))
      (should (functionp (plist-get plist :on-action))))))

(ert-deftest asn-test-routing/transform-function-applied ()
  "The transform function is applied to the plist before sending."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-transform-function
          (lambda (plist) (plist-put plist :custom "injected")))
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (equal "injected" (plist-get (car asn-test--send-calls) :custom)))))

(ert-deftest asn-test-routing/inhibit-functions-suppress-notification ()
  "When an inhibit function returns non-nil, no notification is sent."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-inhibit-functions
          (list (lambda (_type _event) t)))
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 0 (length asn-test--send-calls)))))

(ert-deftest asn-test-routing/inhibit-functions-type-specific ()
  "An inhibit function can suppress only specific event types."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-inhibit-functions
          (list (lambda (type _event) (eq type 'turn-complete))))
    ;; permission-request is not suppressed
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 1 (length asn-test--send-calls)))
    ;; turn-complete is suppressed
    (agent-shell-notifications--schedule-or-notify
     'turn-complete (current-buffer) (asn-test--make-turn-complete-event))
    (should (= 1 (length asn-test--send-calls)))))

(ert-deftest asn-test-routing/inhibit-cancelled-suppresses-cancelled-turn ()
  "The default inhibit function suppresses turn-complete with stop-reason \"cancelled\"."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-inhibit-functions
          (list #'agent-shell-notifications--inhibit-cancelled))
    ;; cancelled turn-complete is suppressed
    (agent-shell-notifications--schedule-or-notify
     'turn-complete (current-buffer) (asn-test--make-turn-complete-event "cancelled"))
    (should (= 0 (length asn-test--send-calls)))
    ;; end_turn is not suppressed
    (agent-shell-notifications--schedule-or-notify
     'turn-complete (current-buffer) (asn-test--make-turn-complete-event "end_turn"))
    (should (= 1 (length asn-test--send-calls)))))

(ert-deftest asn-test-routing/transform-timeout-function-applied ()
  "The transform-timeout function is applied to the resolved timeout before the plist is built."
  (asn-test--with-shell-buffer
    (setq agent-shell-notifications-timeout 5)
    (setq agent-shell-notifications-transform-timeout-function
          (lambda (secs) (+ 1 secs)))
    (agent-shell-notifications--schedule-or-notify
     'permission-request (current-buffer) (asn-test--make-permission-event))
    (should (= 6 (plist-get (car asn-test--send-calls) :timeout)))))

;; ---------------------------------------------------------------------------
;; Tests: event flow
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-event-flow/permission-request-then-response ()
  "Permission-response cancels the timer and closes the notification for its request."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      ;; Trigger permission-request notification
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-1"))
      (should (= 1 (length asn-test--send-calls)))
      (should (assoc "req-1" (map-elt agent-shell-notifications--notification-ids buf)))
      ;; Permission-response should close it
      (agent-shell-notifications--on-permission-response
       (asn-test--make-permission-response-event "req-1"))
      (should (equal '(1) asn-test--close-calls))
      (should-not (map-elt agent-shell-notifications--notification-ids buf)))))

(ert-deftest asn-test-event-flow/turn-complete-clears-prior-and-notifies ()
  "Turn-complete cancels all timers, closes all notifications, then sends its own."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      ;; Two pending permission notifications
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-A"))
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-B"))
      (should (= 2 (length asn-test--send-calls)))
      ;; Turn-complete clears both and sends its own
      (agent-shell-notifications--on-turn-complete
       (asn-test--make-turn-complete-event))
      ;; Both prior notifications closed
      (should (= 2 (length asn-test--close-calls)))
      ;; New turn-complete notification sent (3 total)
      (should (= 3 (length asn-test--send-calls)))
      (should (equal "Agent-shell: Ready"
                     (plist-get (car asn-test--send-calls) :title))))))

(ert-deftest asn-test-event-flow/concurrent-permissions-tracked-separately ()
  "Multiple permission requests get separate notification IDs; response closes only its own."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-A"))
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-B"))
      (let ((ids (map-elt agent-shell-notifications--notification-ids buf)))
        (should (= 2 (length ids)))
        (should (/= (cdr (assoc "req-A" ids)) (cdr (assoc "req-B" ids)))))
      ;; Close only req-A
      (agent-shell-notifications--on-permission-response
       (asn-test--make-permission-response-event "req-A"))
      (let ((ids (map-elt agent-shell-notifications--notification-ids buf)))
        (should (= 1 (length ids)))
        (should (assoc "req-B" ids))))))

(ert-deftest asn-test-event-flow/visible-permission-response-cancels-idle-timer ()
  "When visible, permission-response cancels the idle timer for that request."
  (asn-test--with-visible-shell
    (let ((buf (current-buffer)))
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-idle"))
      (should (= 1 (length asn-test--timers-created)))
      (should (assoc "req-idle" (map-elt agent-shell-notifications--timers buf)))
      ;; Response cancels the timer
      (agent-shell-notifications--on-permission-response
       (asn-test--make-permission-response-event "req-idle"))
      (should (= 1 (length asn-test--timers-cancelled)))
      (should-not (map-elt agent-shell-notifications--timers buf)))))

(ert-deftest asn-test-event-flow/send-command-clears-all ()
  "Sending a command cancels all timers and closes all notifications."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      (agent-shell-notifications--on-permission-request
       (asn-test--make-permission-event "req-1"))
      (agent-shell-notifications--on-send-command :shell-buffer buf)
      (should-not (map-elt agent-shell-notifications--timers buf))
      (should-not (map-elt agent-shell-notifications--notification-ids buf))
      (should (= 1 (length asn-test--close-calls))))))

;; ---------------------------------------------------------------------------
;; Tests: multi-buffer isolation
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-multi-buffer/notifications-isolated ()
  "Notification IDs are scoped per buffer; response in one buffer does not affect another."
  (asn-test--with-shell-buffer
    (let ((buf-a (current-buffer))
          (buf-b (generate-new-buffer " *shell-b*")))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (agent-shell-notifications--on-permission-request
               (asn-test--make-permission-event "req-A")))
            (with-current-buffer buf-b
              (agent-shell-notifications--on-permission-request
               (asn-test--make-permission-event "req-B")))
            ;; Each buffer owns only its own notification
            (should (assoc "req-A" (map-elt agent-shell-notifications--notification-ids buf-a)))
            (should-not (assoc "req-B" (map-elt agent-shell-notifications--notification-ids buf-a)))
            (should (assoc "req-B" (map-elt agent-shell-notifications--notification-ids buf-b)))
            (should-not (assoc "req-A" (map-elt agent-shell-notifications--notification-ids buf-b)))
            ;; Response in buf-a closes only buf-a's notification
            (with-current-buffer buf-a
              (agent-shell-notifications--on-permission-response
               (asn-test--make-permission-response-event "req-A")))
            (should-not (map-elt agent-shell-notifications--notification-ids buf-a))
            (should (assoc "req-B" (map-elt agent-shell-notifications--notification-ids buf-b)))
            ;; Turn-complete in buf-b clears buf-b only
            (with-current-buffer buf-b
              (agent-shell-notifications--on-turn-complete
               (asn-test--make-turn-complete-event)))
            (should (= 2 (length asn-test--close-calls))))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

(ert-deftest asn-test-multi-buffer/idle-timers-isolated ()
  "Idle timers are scoped per buffer; response in one buffer does not cancel another's timer."
  (asn-test--with-visible-shell
    (let ((buf-a (current-buffer))
          (buf-b (generate-new-buffer " *shell-b*")))
      (unwind-protect
          (progn
            (with-current-buffer buf-a
              (agent-shell-notifications--on-permission-request
               (asn-test--make-permission-event "req-A")))
            (with-current-buffer buf-b
              (agent-shell-notifications--on-permission-request
               (asn-test--make-permission-event "req-B")))
            ;; Each buffer has its own timer
            (should (assoc "req-A" (map-elt agent-shell-notifications--timers buf-a)))
            (should-not (assoc "req-B" (map-elt agent-shell-notifications--timers buf-a)))
            (should (assoc "req-B" (map-elt agent-shell-notifications--timers buf-b)))
            ;; Response in buf-a cancels only buf-a's timer
            (with-current-buffer buf-a
              (agent-shell-notifications--on-permission-response
               (asn-test--make-permission-response-event "req-A")))
            (should (= 1 (length asn-test--timers-cancelled)))
            (should-not (map-elt agent-shell-notifications--timers buf-a))
            (should (assoc "req-B" (map-elt agent-shell-notifications--timers buf-b)))
            ;; Fire buf-b's timer — notification sent only for buf-b
            (funcall (plist-get (cdr (assoc "req-B"
                                            (map-elt agent-shell-notifications--timers buf-b)))
                                :func))
            (should (= 1 (length asn-test--send-calls)))
            (should (assoc "req-B" (map-elt agent-shell-notifications--notification-ids buf-b)))
            (should-not (map-elt agent-shell-notifications--notification-ids buf-a)))
        (when (buffer-live-p buf-b) (kill-buffer buf-b))))))

;; ---------------------------------------------------------------------------
;; Tests: subscribe / unsubscribe
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-subscribe/creates-subscriptions ()
  "Subscribe creates three event subscriptions; re-subscribing unsubscribes first."
  (asn-test--with-shell-buffer
    (let ((sub-count 0)
          (unsub-count 0))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest _args)
                   (cl-incf sub-count)
                   (make-symbol "tok")))
                ((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest _args) (cl-incf unsub-count))))
        (agent-shell-notifications-subscribe (current-buffer))
        (should (= 3 sub-count))
        (should (= 3 (length (map-elt agent-shell-notifications--subscriptions
                                      (current-buffer)))))
        ;; Re-subscribe: unsubscribes first
        (agent-shell-notifications-subscribe (current-buffer))
        (should (= 3 unsub-count))
        (should (= 6 sub-count))))))

(ert-deftest asn-test-subscribe/rollback-on-error ()
  "If subscribe fails partway, previously created subscriptions are cleaned up."
  (asn-test--with-shell-buffer
    (let ((call-count 0)
          (unsub-tokens nil))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest _args)
                   (cl-incf call-count)
                   (when (= call-count 2) (error "Simulated failure"))
                   (make-symbol (format "tok-%d" call-count))))
                ((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest args)
                   (push (plist-get args :subscription) unsub-tokens))))
        (should-error (agent-shell-notifications-subscribe (current-buffer)))
        (should-not (map-elt agent-shell-notifications--subscriptions
                             (current-buffer)))
        (should (= 1 (length unsub-tokens)))))))

(ert-deftest asn-test-unsubscribe/clears-everything ()
  "Unsubscribe clears subscriptions, cancels timers, and closes notifications."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest _args) (make-symbol "tok")))
                ((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest _args) nil)))
        (agent-shell-notifications-subscribe buf)
        (agent-shell-notifications--add-timer buf "k" '(:id t))
        (agent-shell-notifications--add-notification-id buf "k" 99)
        (agent-shell-notifications-unsubscribe buf)
        (should-not (map-elt agent-shell-notifications--subscriptions buf))
        (should-not (map-elt agent-shell-notifications--timers buf))
        (should-not (map-elt agent-shell-notifications--notification-ids buf))
        (should (= 1 (length asn-test--timers-cancelled)))
        (should (equal '(99) asn-test--close-calls))))))

(ert-deftest asn-test-unsubscribe/collects-errors ()
  "Unsubscribe collects errors from individual calls but still cleans up state."
  (asn-test--with-shell-buffer
    (let ((buf (current-buffer)))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest _args) (make-symbol "tok")))
                ((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest _args) (error "Unsub error"))))
        (agent-shell-notifications-subscribe buf)
        (let ((err (should-error
                    (agent-shell-notifications-unsubscribe buf)
                    :type 'agent-shell-notifications-unsubscribe-error)))
          (should (= 3 (length (cdr err)))))
        (should-not (map-elt agent-shell-notifications--subscriptions buf))))))

;; ---------------------------------------------------------------------------
;; Tests: minor mode
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-mode/enable-and-disable ()
  "Enabling the mode subscribes and adds advice; disabling unsubscribes and removes it."
  (asn-test--with-shell-buffer
    (cl-letf (((symbol-function 'agent-shell-subscribe-to)
               (lambda (&rest _args) (make-symbol "tok")))
              ((symbol-function 'agent-shell-unsubscribe)
               (lambda (&rest _args) nil)))
      (agent-shell-notifications-mode 1)
      (should agent-shell-notifications-mode)
      (should (map-elt agent-shell-notifications--subscriptions (current-buffer)))
      (should (advice-member-p #'agent-shell-notifications--on-send-command
                               #'agent-shell--send-command))
      (agent-shell-notifications-mode -1)
      (should-not agent-shell-notifications-mode)
      (should-not (map-elt agent-shell-notifications--subscriptions (current-buffer)))
      (should-not (advice-member-p #'agent-shell-notifications--on-send-command
                                   #'agent-shell--send-command)))))

(ert-deftest asn-test-mode/advice-kept-with-other-buffers ()
  "Advice is NOT removed when other buffers still have subscriptions."
  (asn-test--with-shell-buffer
    (cl-letf (((symbol-function 'agent-shell-subscribe-to)
               (lambda (&rest _args) (make-symbol "tok")))
              ((symbol-function 'agent-shell-unsubscribe)
               (lambda (&rest _args) nil)))
      (let ((other-buf (generate-new-buffer " *other-shell*")))
        (unwind-protect
            (progn
              (setf (map-elt agent-shell-notifications--subscriptions other-buf)
                    (list (make-symbol "other-tok")))
              (agent-shell-notifications-mode 1)
              (agent-shell-notifications-mode -1)
              (should (advice-member-p #'agent-shell-notifications--on-send-command
                                       #'agent-shell--send-command))
              (setq agent-shell-notifications--subscriptions nil)
              (advice-remove #'agent-shell--send-command
                             #'agent-shell-notifications--on-send-command))
          (kill-buffer other-buf))))))

;; ---------------------------------------------------------------------------
;; Tests: viewport modes
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-viewport-edit-mode/hooks ()
  "Enabling viewport-edit-mode adds window-selection hook; disabling removes it."
  (asn-test--with-shell-buffer
    (agent-shell-notifications-viewport-edit-mode 1)
    (should (memq #'agent-shell-notifications--on-window-activity
                  window-selection-change-functions))
    (agent-shell-notifications-viewport-edit-mode -1)
    (should-not (memq #'agent-shell-notifications--on-window-activity
                      window-selection-change-functions))))

(ert-deftest asn-test-viewport-view-mode/hooks-and-advice ()
  "Enabling viewport-view-mode adds window hook and viewport-reply advice; disabling removes both."
  (asn-test--with-shell-buffer
    (agent-shell-notifications-viewport-view-mode 1)
    (should (memq #'agent-shell-notifications--on-window-activity
                  window-selection-change-functions))
    (should (advice-member-p #'agent-shell-notifications--on-viewport-activity
                             #'agent-shell-viewport-reply))
    (agent-shell-notifications-viewport-view-mode -1)
    (should-not (memq #'agent-shell-notifications--on-window-activity
                      window-selection-change-functions))
    (should-not (advice-member-p #'agent-shell-notifications--on-viewport-activity
                                 #'agent-shell-viewport-reply))))

(ert-deftest asn-test-viewport-view-mode/advice-kept-with-other-buffers ()
  "Viewport-reply advice is NOT removed when other buffers still have viewport-view-mode active."
  (asn-test--with-shell-buffer
    (let ((other-buf (generate-new-buffer " *other-viewport*")))
      (unwind-protect
          (progn
            (with-current-buffer other-buf
              (agent-shell-notifications-viewport-view-mode 1))
            (agent-shell-notifications-viewport-view-mode 1)
            (agent-shell-notifications-viewport-view-mode -1)
            (should (advice-member-p #'agent-shell-notifications--on-viewport-activity
                                     #'agent-shell-viewport-reply))
            (with-current-buffer other-buf
              (agent-shell-notifications-viewport-view-mode -1))
            (should-not (advice-member-p #'agent-shell-notifications--on-viewport-activity
                                         #'agent-shell-viewport-reply)))
        (when (buffer-live-p other-buf) (kill-buffer other-buf))))))

;; ---------------------------------------------------------------------------
;; Tests: buffer kill cleanup
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-buffer-kill/full-cleanup ()
  "Killing a buffer with mode active cleans up subscriptions, timers, and notifications."
  (asn-test--with-shell-buffer
    (let ((buf (generate-new-buffer " *kill-test*")))
      (cl-letf (((symbol-function 'agent-shell-subscribe-to)
                 (lambda (&rest _args) (make-symbol "tok")))
                ((symbol-function 'agent-shell-unsubscribe)
                 (lambda (&rest _args) nil)))
        (unwind-protect
            (progn
              (with-current-buffer buf
                (agent-shell-notifications-mode 1))
              (agent-shell-notifications--add-timer buf "k" '(:id kill-timer))
              (agent-shell-notifications--add-notification-id buf "k" 99)
              (kill-buffer buf)
              (should-not (map-elt agent-shell-notifications--subscriptions buf))
              (should-not (map-elt agent-shell-notifications--timers buf))
              (should-not (map-elt agent-shell-notifications--notification-ids buf)))
          (when (buffer-live-p buf) (kill-buffer buf))
          (advice-remove #'agent-shell--send-command
                         #'agent-shell-notifications--on-send-command))))))

;; ---------------------------------------------------------------------------
;; Tests: set-provider
;; ---------------------------------------------------------------------------

(ert-deftest asn-test-set-provider/nil-skips-require ()
  "set-provider with nil skips loading but still validates functions are set."
  (let ((agent-shell-notifications-send-function #'asn-test--send)
        (agent-shell-notifications-close-function #'asn-test--close))
    (agent-shell-notifications-set-provider nil)
    (should t)))

(ert-deftest asn-test-set-provider/errors-when-send-nil ()
  "set-provider errors when send-function is not set."
  (let ((agent-shell-notifications-send-function nil)
        (agent-shell-notifications-close-function #'asn-test--close))
    (should-error (agent-shell-notifications-set-provider nil))))

(ert-deftest asn-test-set-provider/errors-when-close-nil ()
  "set-provider errors when close-function is not set."
  (let ((agent-shell-notifications-send-function #'asn-test--send)
        (agent-shell-notifications-close-function nil))
    (should-error (agent-shell-notifications-set-provider nil))))

(ert-deftest asn-test-set-provider/custom-set-variable-reloads-provider ()
  "Changing provider via Custom applies the active provider functions."
  (let ((original-provider agent-shell-notifications-provider)
        (original-transform-timeout-function
         agent-shell-notifications-transform-timeout-function)
        (original-transform-function agent-shell-notifications-transform-function)
        (original-send-function agent-shell-notifications-send-function)
        (original-close-function agent-shell-notifications-close-function))
    (unwind-protect
        (progn
          (setq agent-shell-notifications-transform-timeout-function
                (lambda (secs) (* secs 2)))
          (setq agent-shell-notifications-transform-function
                (lambda (plist) (plist-put plist :old-provider t)))
          (setq agent-shell-notifications-send-function #'asn-test--send)
          (setq agent-shell-notifications-close-function #'asn-test--close)
          (customize-set-variable
           'agent-shell-notifications-provider
           'agent-shell-notifications-libnotify)
          (should (eq agent-shell-notifications-send-function
                      #'agent-shell-notifications--send-libnotify))
          (should (eq agent-shell-notifications-close-function
                      #'agent-shell-notifications--close-libnotify))
          (should (= (funcall agent-shell-notifications-transform-timeout-function 1)
                     1000))
          (should-not
           (plist-get (funcall agent-shell-notifications-transform-function nil)
                      :old-provider)))
      (setq agent-shell-notifications-provider original-provider)
      (setq agent-shell-notifications-transform-timeout-function
            original-transform-timeout-function)
      (setq agent-shell-notifications-transform-function original-transform-function)
      (setq agent-shell-notifications-send-function original-send-function)
      (setq agent-shell-notifications-close-function original-close-function))))

(ert-deftest asn-test-set-provider/reloads-built-in-providers ()
  "set-provider reapplies already-loaded built-in provider setup functions."
  (let ((original-provider agent-shell-notifications-provider)
        (original-transform-timeout-function
         agent-shell-notifications-transform-timeout-function)
        (original-transform-function agent-shell-notifications-transform-function)
        (original-send-function agent-shell-notifications-send-function)
        (original-close-function agent-shell-notifications-close-function))
    (unwind-protect
        (progn
          (agent-shell-notifications-set-provider
           'agent-shell-notifications-knockknock)
          (should (eq agent-shell-notifications-send-function
                      #'agent-shell-notifications--send-knockknock))
          (should (eq agent-shell-notifications-close-function
                      #'agent-shell-notifications--close-knockknock))
          (should (eq agent-shell-notifications-transform-function
                      #'agent-shell-notifications--transform-knockknock))

          (agent-shell-notifications-set-provider
           'agent-shell-notifications-libnotify)
          (should (eq agent-shell-notifications-send-function
                      #'agent-shell-notifications--send-libnotify))
          (should (eq agent-shell-notifications-close-function
                      #'agent-shell-notifications--close-libnotify)))
      (setq agent-shell-notifications-provider original-provider)
      (setq agent-shell-notifications-transform-timeout-function
            original-transform-timeout-function)
      (setq agent-shell-notifications-transform-function original-transform-function)
      (setq agent-shell-notifications-send-function original-send-function)
      (setq agent-shell-notifications-close-function original-close-function))))

(provide 'agent-shell-notifications-tests)

;;; agent-shell-notifications-tests.el ends here
