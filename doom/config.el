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
(setq doom-font (font-spec :family "Menlo" :size 14 :weight 'semi-light))
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'zenburn)
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

(map! :ne "SPC y" #'yank-from-kill-ring)

;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(after! eglot
  :config
  (add-to-list 'eglot-server-programs
               '(typescript-tsx-mode . ("typescript-language-server" "--stdio"))))

(require 'magit)

(defun get-crux-url-for-current-file ()
  "Prints Amazon code link to the position in the current file."
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

(map! :ne "SPC c l" #'get-crux-url-for-current-file)

(setq llama-cpp-host (getenv "LLAMA_CPP_HOST"))
(setq llama-cpp-http-basic-creds (getenv "LLAMA_CPP_BASIC_AUTH"))
(message llama-cpp-http-basic-creds)

(use-package! gptel
  :config
  (setq
   gptel-model   'test
   gptel-backend (gptel-make-openai "llama-cpp"
                   :stream t
                   :protocol "https"
                   :host llama-cpp-host
                   :header `(("Authorization" . ,(format "Basic %s" llama-cpp-http-basic-creds)))
                   :models '(test))
   )
  )

;; M-x package-vc-install https://github.com/jdtsmith/eglot-booster
;; and you also need to have https://github.com/blahgeek/emacs-lsp-booster binary installed in the system
(use-package! eglot-booster
  :after eglot
  :config (eglot-booster-mode))
