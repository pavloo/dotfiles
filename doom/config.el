;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is opTIONAL.
;; (setq user-full-name "John Doe"
;;       user-mail-address "john@doe.com")

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(pixel-scroll-precision-mode)

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;

(defun is-in-wsl ()
  (when (> (length (getenv "WSL_DISTRO_NAME")) 0) 't))

;; `:size' as an integer means pixels. On macOS those are logical points that a
;; Retina display doubles into real pixels; on Windows Emacs is DPI-aware and
;; uses real pixels directly, so it needs twice the number for the same look.
(defvar my-font-size
  (if (eq system-type 'windows-nt) 28 14)
  "Pixel size for `doom-font', adjusted for how each platform reports pixels.")

(setq doom-font (font-spec :family "CaskaydiaCove NF" :size my-font-size :weight 'normal))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(if (is-in-wsl)
    (set-fontset-font t 'symbol (font-spec :family "Noto Color Emoji") nil 'prepend))

;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-zenburn)
                                        ;(load-theme 'zenburn)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
;;
;;

(defun search-dated-org-files (days &optional directory search-string)
  "Search for SEARCH-STRING in .org files dated within DAYS in DIRECTORY."
  (interactive
   (list (read-number "Days: " 7)
         nil
         nil))
  (let* ((directory (or directory (concat org-directory "roam/daily")))
         (search-string (or search-string (read-string "Search string: ")))
         (cutoff-date (time-subtract (current-time) (days-to-time days)))
         (results '()))
    (if (string-empty-p search-string) (error "Search string is empty."))
    (dolist (file (directory-files directory t "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$"))
      (let ((file-date (date-to-time (substring (file-name-nondirectory file) 0 10))))
        (when (time-less-p cutoff-date file-date)
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (search-forward search-string nil t)
              (push (format "%s:%d:%s" 
                            file
                            (line-number-at-pos)
                            (string-trim (thing-at-point 'line t)))
                    results))))))
    (if results
        (let ((buf (get-buffer-create "*search-dated-files*")))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (mapconcat 'identity (nreverse results) "\n"))
              (grep-mode))
            (goto-char (point-min)))
          (display-buffer buf))
      (message "No matches found"))))

(load! "./keybindings.el")

;; https://www.rahuljuliato.com/posts/eglot-rassumfrassum
(with-eval-after-load 'eglot
  (add-to-list
   'eglot-server-programs
   '((tsx-ts-mode typescript-ts-mode)
     . ("rass"
	"--"
	"typescript-language-server" "--stdio"
	;; "--"
	;; "eslint-lsp" "--stdio"
        )))
  (add-to-list
   'eglot-server-programs
   '((python-ts-mode python-mode)
     . ("rass"
	"--"
	"ty" "server"
	"--"
	"pylsp"))))

(setq! eldoc-echo-area-use-multiline-p nil)

(require 'magit)

(defun get-crux-url-for-current-file ()
  "Prints Amazon code link from the position in the current file."
  (interactive)
  (let*  ((remote-url (magit-get "remote" "origin" "url"))
          (commit (magit-rev-parse "HEAD"))
          (relative-path (file-relative-name (buffer-file-name) (magit-toplevel)))
          (line-number (line-number-at-pos))
          (package-name (when (string-match "ssh:\/\/git\.amazon\.com:2222\/pkg\/\\(.*\\)" remote-url)
                          (match-string 1 remote-url)
                          ))
          (crux-url (when package-name
                      (concat "https://code.amazon.com/packages/"
                              package-name
                              "/blobs/"
                              commit
                              "/--/"
                              relative-path
                              "/"
                              "#L"
                              (number-to-string line-number))
                      )))
    (if crux-url
        (progn
          (kill-new crux-url)
          (message crux-url)
          )
      (message "Error creating Amazon link."))))

(setq tramp-histfile-override "")

(if (is-in-wsl)
    (let ((cmd-exe "chromium")
          (cmd-args '("--ozone-platform=wayland")))
      (setq browse-url-generic-program  cmd-exe
            browse-url-generic-args     cmd-args
            browse-url-browser-function 'browse-url-generic
            search-web-default-browser 'browse-url-generic)))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))

(setq js-indent-level 2)
(setq typescript-indent-level 2)
(setq web-mode-code-indent-offset 2)

(setq leetcode-prefer-language "typescript")

(setq vterm-shell "/bin/zsh")
(after! vterm
  (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))
  )

(defun disable-autoformatting ()
  "Disable autoformatting for the current buffer."
  (interactive)
  (apheleia-mode -1)
  )

(defun save-without-autoformatting ()
  "Save file without invoking auto-formatter."
  (interactive)
  (apheleia-mode -1)
  (save-buffer)
  (apheleia-mode)
  (message "Saved current buffer without formatting.")
  )

;; Display emojis everywhere
(after! emojify
  (setq emojify-display-style 'unicode) ; Enable globally (default is prog-mode and text-mode only)
  (global-emojify-mode))  ; or 'image for image-based emojis

(add-hook 'python-mode-hook
          (lambda ()
            (require 'eglot)
            (add-to-list 'eglot-server-programs '(python-mode . ("pylsp")))
            (eglot-ensure)))

(use-package! agent-shell-kiro
  :custom
  (agent-shell-kiro-acp-command '("kiro-cli" "acp" "--agent" "permissive")))

(use-package! agent-shell-manager
  :config
  (set-evil-initial-state! 'agent-shell-manager-mode 'emacs))

(setq agent-shell-mcp-servers
      '(((name . "builder-mcp")
         (command . "builder-mcp")
         (args . ())
         (env . ()))
        ((name . "aws-cx-builder-mcp")
         (command . "aws-cx-builder-mcp")
         (args . ())
         (env . ()))
        ((name . "sage-plus-service-mcp")
         (command . "sage-plus-service-mcp")
         (args . ())
         (env . ()))
        ((name . "amazon-sharepoint-mcp")
         (command . "amazon-sharepoint-mcp")
         (args . ())
         (env . ()))
        ((name . "venue-mcp")
         (command . "venue-mcp")
         (args . ())
         (env . ()))
        ((name . "threatcomposer-ai-mcp")
         (command . "uvx")
         (args . ("--from"
                  "git+https://github.com/awslabs/threat-composer.git#subdirectory=packages/threat-composer-ai"
                  "threat-composer-ai-mcp"))
         (env . ()))
        ((name . "playwright")
         (command . "npx")
         (args . ("@playwright/mcp@latest"))
         (env . ()))
        ((name . "local-chorus-mcp")
         (command . "local-chorus-mcp")
         (args . ())
         (env . ()))
        ((name . "slack-mcp")
         (command . "slack-mcp")
         (args . ())
         (env . ()))))

(setq agent-shell-permission-responder-function nil)

;; Make claude-agent-acp use the Toolbox claude instead of its bundled SDK CLI
(setenv "CLAUDE_CODE_EXECUTABLE" (expand-file-name "~/.toolbox/bin/claude"))

(setq markdown-command "pandoc")

;; org-present: center text with visual-fill-column
(defun my/org-present-start ()
  (setq-local visual-fill-column-width 200
              visual-fill-column-center-text t)
  (visual-fill-column-mode 1)
  (display-line-numbers-mode 0))

(defun my/org-present-end ()
  (visual-fill-column-mode 0)
  (display-line-numbers-mode 1)
  (kill-local-variable 'visual-fill-column-width)
  (kill-local-variable 'visual-fill-column-center-text))

(after! org-present
  (add-hook 'org-present-mode-hook #'my/org-present-start)
  (add-hook 'org-present-mode-quit-hook #'my/org-present-end))

(use-package! org-download
  :after org
  :hook (dired-mode . org-download-enable)
  :config
  (setq org-download-screenshot-method "pngpaste %s"))

;; Resize inline images with the mouse wheel instead of scrolling when the
;; pointer is over an image. Org inline images use `image-map', and overlay
;; keymaps take precedence over `pixel-scroll-precision-mode-map'.
(after! image
  (map! :map image-map
        [wheel-up]          #'image-mouse-increase-size
        [wheel-down]        #'image-mouse-decrease-size
        [double-wheel-up]   #'image-mouse-increase-size
        [double-wheel-down] #'image-mouse-decrease-size
        [triple-wheel-up]   #'image-mouse-increase-size
        [triple-wheel-down] #'image-mouse-decrease-size))

;; Persist mouse-wheel image resizes in org as `#+attr_org: :width N'.
;; Requires `org-image-actual-width' to be nil (the default) so org honors it.
(defvar my/org-image--pending nil "Pending (MARKER . WIDTH) to write.")
(defvar my/org-image--timer nil)

(defun my/org-image--write-width (marker width)
  (when (buffer-live-p (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (let* ((para (org-element-lineage (org-element-context) '(paragraph) t))
               (beg (and para (org-element-property :begin para)))
               (post (and para (org-element-property :post-affiliated para)))
               (case-fold-search t))
          (cond
           ((not para) (message "Image is not in a paragraph; width not saved"))
           ((progn (goto-char beg) (re-search-forward "^[ \t]*#\\+attr_org:.*$" post t))
            (let ((eol (match-end 0)))
              (goto-char (match-beginning 0))
              (if (re-search-forward ":width[ \t]+[^ \t\n]+" eol t)
                  (replace-match (format ":width %d" width) t t)
                (goto-char eol)
                (insert (format " :width %d" width)))))
           ((progn (goto-char post) (skip-chars-backward " \t") (bolp))
            (let ((indent (current-indentation)))
              (beginning-of-line)
              (insert (make-string indent ?\s)
                      (format "#+attr_org: :width %d\n" width))))
           (t (message "Can't place #+attr_org here; width not saved"))))))
    (set-marker marker nil)))

(defun my/org-image--flush ()
  (when my/org-image--timer (cancel-timer my/org-image--timer))
  (setq my/org-image--timer nil)
  (when my/org-image--pending
    (let ((p my/org-image--pending))
      (setq my/org-image--pending nil)
      (my/org-image--write-width (car p) (cdr p)))))

(defun my/org-image-persist-width (event &rest _)
  (let* ((posn (event-start event))
         (win (posn-window posn))
         (pos (posn-point posn)))
    (when (and (windowp win) pos)
      (with-current-buffer (window-buffer win)
        (let ((img (get-char-property pos 'display)))
          (when (and (derived-mode-p 'org-mode) (eq (car-safe img) 'image))
            (let ((start (or (previous-single-char-property-change (1+ pos) 'display) pos))
                  (width (round (car (image-size img t)))))
              (when (and my/org-image--pending
                         (not (equal (marker-position (car my/org-image--pending)) start)))
                (my/org-image--flush))
              (when my/org-image--pending
                (set-marker (car my/org-image--pending) nil))
              (setq my/org-image--pending (cons (copy-marker start) width))
              (when my/org-image--timer (cancel-timer my/org-image--timer))
              (setq my/org-image--timer
                    (run-with-idle-timer 0.5 nil #'my/org-image--flush)))))))))

(advice-add 'image-mouse-increase-size :after #'my/org-image-persist-width)
(advice-add 'image-mouse-decrease-size :after #'my/org-image-persist-width)
