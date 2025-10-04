;;; keybindings.el --- Global keybindings -*- lexical-binding: t; -*-

;;; Commentary:
;;; Custom global key bindings, not related to any specific mode or
;;; otherwise globally used.

;;; Code:

(message "*** STARTUP: Loading global keybindings")

;; C-hjkl window movements
(evil-define-key 'normal 'global (kbd "C-h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "C-j") 'evil-window-down)
(evil-define-key 'normal 'global (kbd "C-k") 'evil-window-up)
(evil-define-key 'normal 'global (kbd "C-l") 'evil-window-right)

;; Introspection
(evil-define-key 'normal 'global (kbd "SPC h") 'help-command)
(evil-define-key 'normal 'global (kbd "SPC h f") 'counsel-describe-function)
(evil-define-key 'normal 'global (kbd "SPC h v") 'counsel-describe-variable)

;; Open and write files
(evil-define-key 'normal 'global (kbd "SPC f s") 'evil-write)
(evil-define-key 'normal 'global (kbd "SPC f e") 'evil-read)
(evil-define-key 'normal 'global (kbd "SPC f r") 'counsel-recentf)
(evil-define-key 'normal 'global (kbd "SPC f f") 'counsel-find-file)
(evil-define-key 'normal 'global (kbd "SPC f p") 'counsel-git)
(evil-define-key 'normal 'global (kbd "SPC p") 'projectile-command-map)

;; Splits, windows and buffers
(evil-define-key 'normal 'global (kbd "SPC s")
  (lambda () (interactive) (evil-window-split) (other-window 1)))
(evil-define-key 'normal 'global (kbd "SPC v")
  (lambda () (interactive) (evil-window-vsplit) (other-window 1)))
(evil-define-key 'normal 'global (kbd "SPC q") 'evil-quit)
(evil-define-key 'normal 'global (kbd "SPC k") 'kill-buffer)
(evil-define-key 'normal 'global (kbd "SPC b b") 'counsel-switch-buffer)

;; Temp macro ('qq' to record, 'Q' to execute)
(evil-define-key 'normal 'global (kbd "Q") (kbd "@q"))

;; M-x
(evil-define-key 'normal 'global (kbd "SPC x x") 'counsel-M-x)

;; Search
(evil-define-key 'normal 'global (kbd "SPC a a") 'counsel-ag)

;; Avy
(evil-define-key 'normal 'global (kbd "SPC SPC w") 'avy-goto-word-0)
(evil-define-key 'normal 'global (kbd "SPC SPC j") 'avy-goto-line)


(provide 'keybindings)

;;; keybindings.el ends here
