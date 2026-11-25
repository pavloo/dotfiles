;;; amazon-ai-code-reviews.el --- Agent-shell driven CRUX code reviews -*- lexical-binding: t; -*-

;;; Commentary:
;; Start a dedicated Claude agent shell that watches Slack for CR review
;; requests (via `my/slack-watch-start' and the crux-review-watch skill)
;; and leaves DRAFT comments.
;;
;; Usage: M-x my/start-code-reviews

;;; Code:

(declare-function agent-shell-start "agent-shell")
(declare-function shell-maker-set-buffer-name "shell-maker")
(declare-function my/slack-watch-start "agent-shell-workers")
(defvar my/agent-shell-auto-approve)

(defvar my/code-reviews-buffer-name "*CR Reviews*"
  "Buffer name for the agent shell started by `my/start-code-reviews'.")

(defvar my/code-reviews-channels '("CRUX" "#cross-console-all")
  "Slack channels or Apps `my/start-code-reviews' reads CR review requests from.
Use \"#name\" for channels and the App name (e.g. \"CRUX\") for App DMs.")

(defun my/start-code-reviews ()
  "Start a new Claude agent shell that reviews CRs posted to Slack.
Reads review requests from every entry in `my/code-reviews-channels'.
Permission requests are auto-approved, except local filesystem mutations,
which are rejected (only watcher state under `my/agent-shell-writable-dirs'
may be written)."
  (interactive)
  (require 'agent-shell)
  (if-let* ((existing (get-buffer my/code-reviews-buffer-name)))
      (progn (pop-to-buffer existing)
             (message "%s already running" my/code-reviews-buffer-name))
    (let ((shell-buffer (agent-shell-start
                         :config (my/agent-shell-read-only-claude-config))))
      (with-current-buffer shell-buffer
        (shell-maker-set-buffer-name shell-buffer my/code-reviews-buffer-name)
        (setq my/agent-shell-auto-approve t)
        (my/slack-watch-start my/code-reviews-channels "crux-review-watch" 5)))))

(provide 'amazon-ai-code-reviews)
;;; amazon-ai-code-reviews.el ends here
