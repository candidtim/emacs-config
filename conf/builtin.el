;;; builtin.el --- Emacs native features -*- lexical-binding: t; -*-

;;; Commentary:
;;; Configuration of built-in Emacs features

;;; Code:

(message "*** STARTUP: Configuring built-in Emacs features")

;;
;; UI configuration
;;

(tool-bar-mode -1) ;; hide tool bar
(scroll-bar-mode -1) ;; nay scroll bars
(menu-bar-mode 1) ;; yay menu
(tooltip-mode -1) ;; nay tooltips

(blink-cursor-mode 0) ;; non-blinking cursor
(setq ring-bell-function 'ignore) ;; no bell
(global-hl-line-mode 1) ;; highligh current line

;; Line numbers
(column-number-mode)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
;; disable line numbers for some modes:
(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook
                treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Show matching parenthesis
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Start maximized
(custom-set-variables '(initial-frame-alist '((fullscreen . maximized))))

;; Only use text mode
(setq use-dialog-box nil)

;; Scrolling
(setq scroll-margin 3
      scroll-step 1)

;; Not too many warnings, please
(setq warning-minimum-level :emergency)


;;
;; Built-in minor modes
;;

;; Find recent files
(recentf-mode 1)
(add-hook 'recentf-dialog-mode-hook 'evil-normal-state)
(add-to-list 'recentf-exclude "treemacs")

;; Save location in file
(save-place-mode 1)

;; Autosave
(auto-save-mode -1)

;; Autoload
(global-auto-revert-mode 1) ; files
(setq global-auto-revert-non-file-buffers t) ; e.g., dired

;; No lock files
(setq create-lockfiles nil)

;; Minibuffer history
(setq history-length 10)
(savehist-mode 1)

;; Which key
(setq which-key-idle-delay 1)
(which-key-mode)

;;
;; Feature customization
;;

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil
              tab-width 4)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; Keep UI-customized configuration in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; No backup files
(setq make-backup-files nil)

;;
;; Custom behavior
;;

;; Remove trailing whitespace upon save
(add-hook 'before-save-hook 'delete-trailing-whitespace)


(provide 'builtin)

;;; builtin.el ends here
