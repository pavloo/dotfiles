;;; agent-shell-workers.el --- Periodic background workers for agent-shell -*- lexical-binding: t; -*-

;;; Commentary:
;; Drive an agent-shell buffer from a timer.  Currently provides a Slack
;; watcher that periodically invokes a skill (by default
;; ~/.claude/skills/crux-review-watch) once for each of a list of Slack
;; channels or Apps; the skill fetches new messages via slack-mcp and
;; reviews the CRs they reference.
;;
;; Usage: open a dedicated agent-shell buffer, then
;;   M-x my/slack-watch-start
;; Stop with M-x my/slack-watch-stop.

;;; Code:

(declare-function agent-shell-insert "agent-shell")
(declare-function agent-shell--state "agent-shell")
(declare-function agent-shell--update-fragment "agent-shell")
(declare-function shell-maker-busy "shell-maker")

(defvar my/slack-watch-timer nil
  "Timer driving the Slack watcher.")
(defvar my/slack-watch-drain-timer nil
  "Timer that submits the next channel of the current round once the shell is idle.")
(defvar my/slack-watch-drain-delay 10
  "Seconds between checks for an idle shell while a round is in progress.")
(defvar my/slack-watch-buffer nil
  "agent-shell buffer the Slack watcher submits prompts to.")
(defvar my/slack-watch-skill nil
  "Skill the Slack watcher runs, once per channel.")
(defvar my/slack-watch-channels nil
  "Slack channels or Apps the watcher reads, in order.")
(defvar my/slack-watch--pending nil
  "Channels still to run in the current round.")
(defvar my/slack-watch-interval nil
  "Seconds between watcher rounds.")
(defvar my/slack-watch-typing-idle 2
  "Idle seconds after typing in the watcher buffer before the timer is reset.")
(defvar my/slack-watch--reset-timer nil
  "Pending idle timer that resets `my/slack-watch-timer' after typing stops.")

(defun my/slack-watch--log-attempt (channel)
  "Show in the chat when the watcher last tried to read CHANNEL.
Must run in the watcher buffer."
  (let ((stamp (format-time-string "%Y-%m-%d %H:%M:%S")))
    (agent-shell--update-fragment
     :state (agent-shell--state)
     :block-id (format "slack-watch-%s-%s" channel stamp)
     :label-left (propertize (format "Checking %s messages @ %s" channel stamp)
                             'font-lock-face 'agent-shell-section-heading)
     :create-new t
     :above-last-prompt t)))

(defun my/slack-watch--drain ()
  "Submit the next pending channel once the shell is idle.
Runs one `/SKILL CHANNEL' prompt at a time, so each run finishes
before the next channel starts."
  (setq my/slack-watch-drain-timer nil)
  (cond
   ((not (buffer-live-p my/slack-watch-buffer))
    (my/slack-watch-stop))
   ((null my/slack-watch--pending))
   ((with-current-buffer my/slack-watch-buffer (shell-maker-busy))
    (my/slack-watch--schedule-drain))
   (t
    (let ((channel (pop my/slack-watch--pending)))
      (with-current-buffer my/slack-watch-buffer
        (my/slack-watch--log-attempt channel)
        (agent-shell-insert :shell-buffer my/slack-watch-buffer
                            :text (format "/%s %s" my/slack-watch-skill channel)
                            :submit t
                            :no-focus t)))
    (when my/slack-watch--pending
      (my/slack-watch--schedule-drain)))))

(defun my/slack-watch--schedule-drain ()
  "Check for an idle shell again in `my/slack-watch-drain-delay' seconds."
  (setq my/slack-watch-drain-timer
        (run-at-time my/slack-watch-drain-delay nil #'my/slack-watch--drain)))

(defun my/slack-watch--tick ()
  "Start a round over every channel, unless the previous round is still going."
  (if (not (buffer-live-p my/slack-watch-buffer))
      (my/slack-watch-stop)
    (unless my/slack-watch--pending
      (setq my/slack-watch--pending (copy-sequence my/slack-watch-channels))
      (my/slack-watch--drain))))

(defun my/slack-watch--reset ()
  "Restart `my/slack-watch-timer' so the next round is a full interval away."
  (setq my/slack-watch--reset-timer nil)
  (when (timerp my/slack-watch-timer)
    (cancel-timer my/slack-watch-timer)
    (setq my/slack-watch-timer
          (run-at-time my/slack-watch-interval my/slack-watch-interval
                       #'my/slack-watch--tick))))

(defun my/slack-watch--on-typing ()
  "Debounce: reset the watcher timer once typing pauses.
Only the first keystroke of a burst schedules anything; the idle timer
fires after `my/slack-watch-typing-idle' seconds without input."
  (unless my/slack-watch--reset-timer
    (setq my/slack-watch--reset-timer
          (run-with-idle-timer my/slack-watch-typing-idle nil
                               #'my/slack-watch--reset))))

(defun my/slack-watch-start (channels skill minutes)
  "Every MINUTES, ask the agent to run SKILL for each Slack channel in CHANNELS.
CHANNELS is a list of Slack channel names (\"#name\"), IDs or Slack App
names (\"CRUX\"); a single string is also accepted.  Each round submits
`/SKILL CHANNEL' for one channel at a time, waiting for the shell to go
idle between them.  SKILL owns the instructions (state, fetching,
reviewing).  Run from the agent-shell buffer that should do the work."
  (interactive
   (list (split-string (read-string "Slack channels or Apps (comma-separated): " "CRUX")
                       "[, ]+" t)
         (read-string "Skill name: " "crux-review-watch")
         (read-number "Interval (minutes): " 5)))
  (unless (derived-mode-p 'agent-shell-mode)
    (user-error "Run this from an agent-shell buffer"))
  (let ((channels (if (stringp channels) (list channels) channels)))
    (unless channels
      (user-error "No Slack channels given"))
    (my/slack-watch-stop)
    (setq my/slack-watch-buffer (current-buffer)
          my/slack-watch-skill skill
          my/slack-watch-channels channels
          my/slack-watch-interval (* 60 minutes)
          my/slack-watch-timer
          (run-at-time 0 my/slack-watch-interval #'my/slack-watch--tick))
    (add-hook 'post-self-insert-hook #'my/slack-watch--on-typing nil t)
    (message "Watching %s every %d min with /%s"
             (string-join channels ", ") minutes skill)))

(defun my/slack-watch-crux (&optional minutes)
  "Watch the CRUX Slack app DM and review CRs from its messages.
Polls every MINUTES (default 5; prompt for it with a prefix argument).
Run from the agent-shell buffer that should do the work."
  (interactive
   (list (when current-prefix-arg
           (read-number "Interval (minutes): " 5))))
  (my/slack-watch-start '("CRUX") "crux-review-watch" (or minutes 5)))

(defun my/slack-watch-stop ()
  "Stop the Slack watcher."
  (interactive)
  (dolist (timer (list my/slack-watch-timer my/slack-watch-drain-timer
                       my/slack-watch--reset-timer))
    (when (timerp timer)
      (cancel-timer timer)))
  (when (buffer-live-p my/slack-watch-buffer)
    (with-current-buffer my/slack-watch-buffer
      (remove-hook 'post-self-insert-hook #'my/slack-watch--on-typing t)))
  (setq my/slack-watch-timer nil
        my/slack-watch-drain-timer nil
        my/slack-watch--reset-timer nil
        my/slack-watch--pending nil))

(provide 'agent-shell-workers)
;;; agent-shell-workers.el ends here
