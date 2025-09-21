;;
;; Basic UI customiztion
;;
;; Only standard Emacs configs here - this is loaded before packages.
;;

(message "Loading ui")

;; hide tool bar
(tool-bar-mode -1)

;; nay scroll bars
(scroll-bar-mode -1)

;; yay manu
(menu-bar-mode 1)

;; non-blinking cursor
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)

;; highligh current line
(global-hl-line-mode 1)

;; line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
;; disable line numbers for some modes:
(dolist (mode '(term-mode-hook shell-mode-hook eshell-mode-hook treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; show matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

 ;; start maximized
(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

;; only use text mode
(setq use-dialog-box nil)

;; set font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 140
                    :weight 'light)


(provide 'ui)
