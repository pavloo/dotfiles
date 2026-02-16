;;; keybinding.el -*- lexical-binding: t; -*-

(map! :ne "SPC y" #'yank-from-kill-ring)

;; gptel
(map! :ne "SPC l l" #'gptel)
(map! :ne "SPC l m" #'gptel-menu)
(map! :nev "SPC l s" #'gptel-send)
(map! :nev "SPC l a" #'gptel-add)
(map! :nev "SPC l q" #'gptel-quick)
(map! :nev "SPC l k" #'gptel-abort)
(map! :nev "SPC l c" #'gptel-context-remove-all)
(map! :nev "SPC l r" #'gptel-rewrite)
(map! :nev "SPC m c s" #'org-babel-mark-block)

(defun gptel-quick-error-at-point ()
  "Calls gptel-quick sending flycheck error under cursor."
  (interactive)
  (let ((error (flycheck-overlay-errors-at (point))))
    (gptel-quick
     (format "Please explain what this compilation error means and how to fix it: %s"
             (flycheck-error-message (car error))))))

(map! :ne "SPC l e" #'gptel-quick-error-at-point)

;; multiple cursor
(map! :ne "SPC k n" #'mc/mark-next-symbol-like-this)
(map! :ne "SPC k p" #'mc/mark-previous-like-this-symbol)

;; code
(map! :ne "SPC c l" #'get-crux-url-for-current-file)
(map! :ne "SPC c m" #'flycheck-eglot-mode)

(map! :ne "SPC f w" #'save-without-autoformatting)

;; org roam daily

(map! :ne "SPC n r d /"
      (defun search-org-daily-days (period)
        "Search for a string in daily notes.
         Specify period:
         w (week/7 days, default), m (month/30 days), y (year/365 days)."
        (interactive
         (list (pcase (read-char-choice "Period: [w]eek, [m]onth, [y]ear (default w)? " '(?w ?m ?y ?\r ?\n))
                 (?w 7)
                 (?m 30)
                 (?y 365)
                 (_ 7))))
        (search-dated-org-files period)))
