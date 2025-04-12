;;; keybinding.el -*- lexical-binding: t; -*-

(map! :ne "SPC y" #'yank-from-kill-ring)
(map! :ne "SPC c l" #'get-crux-url-for-current-file)

;; gptel
;;
(map! :ne "SPC l l" #'gptel)
(map! :nev "SPC l s" #'gptel-send)
(map! :nev "SPC l a" #'gptel-abort)

;; multiple cursor
(map! :ne "SPC k n" #'mc/mark-next-symbol-like-this)
(map! :ne "SPC k p" #'mc/mark-previous-like-this-symbol)
