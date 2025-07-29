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
(setq doom-font (font-spec :family "Cascadia Code" :size 14 :weight 'normal))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
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
(load! "./keybindings.el")

(after! eglot
  :config
  (add-to-list 'eglot-server-programs
               '(typescript-tsx-mode . ("typescript-language-server" "--stdio"))
               ;; '(typescript-mode . ("node" (format "~/.nvm/versions/node/%s/lib/node_modules/eslint-server/lib/index.js" "--stdio") (string-trim (shell-command-to-string "node --version"))))
               '(typescript-mode . ("typescript-language-server" "--stdio"))
               ))

;; TODO finish this function to work universally
;; instead of SPC c m hack
(defun switch-eglot ()
  "If there are a few LSP servers added for the same language mode,
  this function will switch between them"
  (interactive)
  (let ((current-mode-server-programs (alist-get major-mode eglot-server-programs)))
    (when (> (length current-mode-server-programs) 1)
      (message "YO")))
  )

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


(setq llama-cpp-url (getenv "LLAMA_CPP_URL"))
(setq llama-cpp-http-basic-creds (getenv "LLAMA_CPP_BASIC_AUTH"))

(let ((url-regexp "^\\(http[s]?\\)://\\(\\([^/]+\\)\\)"))
  (setq llama-cpp-url-parts
        (when (string-match url-regexp llama-cpp-url)
          (let ((protocol (match-string 1 llama-cpp-url))
                (host (match-string 2 llama-cpp-url)))
            (list protocol host)))))

(setq llama-cpp-protocol (car llama-cpp-url-parts))
(setq llama-cpp-host (car (cdr llama-cpp-url-parts)))

(defun is-in-wsl ()
  (when (> (length (getenv "WSL_DISTRO_NAME")) 0) 't))

(use-package! gptel
  :config
  (let
      ((llama (gptel-make-openai "llama-cpp"
                :stream t
                :protocol llama-cpp-protocol
                :host llama-cpp-host
                :header `(("Authorization" . ,(format "Basic %s" llama-cpp-http-basic-creds)))
                :models '(test)))
       (diya (gptel-make-openai "diya"
               :stream t
               :protocol "http"
               :host "localhost:5962"
               :models '(claude-3-5-sonnet))))
    (if (is-in-wsl)
        (setq gptel-backend llama)
      (setq gptel-backend diya))))

(use-package! eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(setq tramp-histfile-override "")

(if (is-in-wsl)
    (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
          (cmd-args '("/c" "start")))
      (when (file-exists-p cmd-exe)
        (setq browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic
              search-web-default-browser 'browse-url-generic))))

(setq gptel-default-mode 'org-mode)

(setq js-indent-level 2)
(setq typescript-indent-level 2)

(setq leetcode-prefer-language "typescript")
