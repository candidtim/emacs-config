;;
;; Editor behavior customization
;;

(message "Loading editor")

;;
;; Enable minor modes
;;

;; Find recent files
(recentf-mode 1)
(add-hook 'recentf-dialog-mode-hook 'evil-normal-state)

;; Save location in file
(save-place-mode 1)

;; Autosave
(auto-save-mode -1)
; TODO : configure auto-save to the same file

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
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
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


(provide 'editor)
