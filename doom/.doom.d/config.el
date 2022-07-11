;;; .doom.d/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(load-theme 'zenburn t)

;; Web
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))

(after! web-mode
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )

(map! :ne "f" #'avy-goto-char)

;; LSP
(map!
 :ne "SPC m r" #'lsp-find-definition)
(map!
 :ne "SPC m R" #'lsp-rename)
(map!
 :ne "SPC m m" #'lsp-ui-imenu)

(map!
 :ne "SPC m f" #'lsp-find-references)

(setq
 lsp-ui-sideline-enable nil
 ;; lsp-flycheck-live-reporting nil
 ;; lsp-eldoc-hook nil
 lsp-prefer-capf t
 )

(defun copy-name-of-ember-test ()
  (interactive)
  (let ((result-fmt "localhost:4200/tests?filter=\"%s\"") (match "") (source (thing-at-point 'line)))
    (string-match "test('\\(.+\\)'.+" source)
    (setq match (match-string 1 source))

    (with-temp-buffer
      (insert (format result-fmt match))
      (clipboard-kill-region (point-min) (point-max)))
    )
  )

(add-to-list 'auto-mode-alist '("\\.js?\\'" . rjsx-mode))
(add-hook! js2-mode
  :append
  ;; (flycheck-select-checker 'javascript-eslint)
  (setq
    js2-basic-offset 2
    js-indent-level 2))

(add-hook 'typescript-tsx-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook 'prettier-js-mode)
(add-hook 'typescript-mode-hook (lambda() (setq typescript-indent-level 2 indent-tabs-mode nil)))
(add-hook 'typescript-tsx-mode-hook (lambda() (setq typescript-indent-level 2 indent-tabs-mode nil)))

(add-hook! enh-ruby-mode
  (rvm-activate-corresponding-ruby))

(defun copy-current-line-position-to-clipboard (p)
    "Copy current line in file to clipboard as '</path/to/file>:<line-number>'"
    (interactive "sAbsolute path y/n?: ")
    (let ((path-with-line-number) (file-name (buffer-file-name)))
      (when (and (not (string= p "y")) (projectile-project-root))
        (setq file-name (file-relative-name buffer-file-name (projectile-project-root)))
        )
      (setq path-with-line-number (concat file-name ":" (number-to-string (line-number-at-pos))))
      (x-select-text path-with-line-number)
      (message (concat path-with-line-number " copied to clipboard"))))

;; org
(after! org
  (map! :map org-mode-map
        :n "M-j" #'org-metadown
        :n "M-k" #'org-metaup))

;; (setq +popup-default-display-buffer-actions '(+popup-display-buffer-stacked-side-window))

;; (setq debug-on-error t)

;; Projectile
(map! :ne "SPC / p" #'counsel-projectile-ag)

;; deal with strange evil-mode bug with delete https://emacs.stackexchange.com/questions/35946/strange-behaviour-on-evil-delete
(defun stop-using-minibuffer ()
    "kill the minibuffer"
    (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
      (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

(setq default-directory "~/")

(map!
 :ne "SPC y" #'counsel-yank-pop)

(map!
 :ne "SPC v" #'set-mark-command)

(map!
 :ne "SPC g l m" #'git-link)

(global-evil-matchit-mode 1)

;; IRC
(after! circe
  (set-irc-server! "chat.freenode.net"
    `(:tls t
      :port 6697
      :nick "Pavloo"
      :sasl-username "Pavloo"
      :sasl-password ,(+pass-get-secret "Chat/freenode")
      :channels ("#emacs"))))

;; Workspaces
(after! workspaces
  (setq +workspaces-on-switch-project-behavior nil)
  )

;; cursor
;; (unless (display-graphic-p)
;;   (require 'evil-terminal-cursor-changer)
;;   (evil-terminal-cursor-changer-activate) ; or (etcc-on)
;;   )
(map!
:n "M-k" #'move-text-up)
(map!
 :n "M-j" #'move-text-down)

(setq magit-auto-revert-mode nil)
(setq global-auto-revert-mode nil)

(use-package! atomic-chrome
  :after-call focus-out-hook
  :config
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  (atomic-chrome-start-server))

;; Prevents some cases of Emacs flickering
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(unless (display-graphic-p)
  (terminal-focus-reporting-mode))

(- (+ (- (+ (/ 10 9) (* 8 7)) (* 6 5)) (/ 4 3)) (* 2 1))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#00212B" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(custom-safe-themes
   (quote
    ("0f0a885f4ce5b6f97e33c7483bfe4515220e9cbd9ab3ca798e0972f665f8ee4d" "5d09b4ad5649fea40249dd937eaaa8f8a229db1cec9a1a0ef0de3ccf63523014" default)))
 '(fci-rule-color "#405A61")
 '(jdee-db-active-breakpoint-face-colors (cons "#073642" "#268bd2"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#073642" "#859900"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#073642" "#56697A"))
 '(objed-cursor-color "#dc322f")
 '(pdf-view-midnight-colors (cons "#839496" "#002b36"))
 '(rustic-ansi-faces
   ["#002b36" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(safe-local-variable-values (quote ((web-mode-auto-quote-style . 2))))
 '(vc-annotate-background "#002b36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#859900")
    (cons 40 "#959300")
    (cons 60 "#a58e00")
    (cons 80 "#b58900")
    (cons 100 "#bc7407")
    (cons 120 "#c35f0e")
    (cons 140 "#cb4b16")
    (cons 160 "#cd4439")
    (cons 180 "#d03d5d")
    (cons 200 "#d33682")
    (cons 220 "#d63466")
    (cons 240 "#d9334a")
    (cons 260 "#dc322f")
    (cons 280 "#ba3f41")
    (cons 300 "#994d54")
    (cons 320 "#775b67")
    (cons 340 "#405A61")
    (cons 360 "#405A61")))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
