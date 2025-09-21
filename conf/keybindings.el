;;
;; Custom key mappings
;;

(message "Loading keybindings")

;; ESC
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Evil
(evil-global-set-key 'motion "j" 'evil-next-visual-line)
(evil-global-set-key 'motion "k" 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
(define-key evil-normal-state-map (kbd "SPC x x") 'counsel-M-x)

;; Treemacs
(defun candidtim/treemacs-toggle ()
  "Toggle treemacs and add current project if opening."
  (interactive)
  (pcase (treemacs-current-visibility)
    ('visible (delete-window (treemacs-get-local-window)))
    ('exists (treemacs-select-window))
    ('none (if (treemacs--find-project-for-buffer)
            (treemacs-add-and-display-current-project)
            (treemacs)))))
(define-key evil-normal-state-map (kbd "C-e") 'candidtim/treemacs-toggle)

;; Ivy + Evil
(define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
(define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)

;; Introspection
(define-key evil-normal-state-map (kbd "SPC h f") 'counsel-describe-function)
(define-key evil-normal-state-map (kbd "SPC h v") 'counsel-describe-variable)

;; Open and write files
(define-key evil-normal-state-map (kbd "SPC f s") 'evil-write)
(define-key evil-normal-state-map (kbd "SPC f e") 'evil-read)
(define-key evil-normal-state-map (kbd "SPC f r") 'counsel-recentf)
(define-key evil-normal-state-map (kbd "SPC f f") 'counsel-find-file)
(define-key evil-normal-state-map (kbd "SPC f p") 'counsel-git)
(define-key evil-normal-state-map (kbd "SPC p") 'projectile-command-map)

;; Splits, windows and buffers
(define-key evil-normal-state-map (kbd "SPC s") 'evil-window-split)
(define-key evil-normal-state-map (kbd "SPC v") 'evil-window-vsplit)
(define-key evil-normal-state-map (kbd "SPC q") 'evil-quit)
(define-key evil-normal-state-map (kbd "SPC k") 'kill-buffer)
(define-key evil-normal-state-map (kbd "SPC b b") 'counsel-switch-buffer)

;; Temp macro ('qq' to record, 'Q' to execute)
(define-key evil-normal-state-map (kbd "Q") (kbd "@q"))

;; Search
(define-key evil-normal-state-map (kbd "SPC a a") 'counsel-ag)

;; 0 <-> ^
(defun candidtim/smart-beginning-of-line ()
  "Toggle between beginning of line and first non-whitespace character"
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (= oldpos (point))
      (beginning-of-line))))
(define-key evil-normal-state-map (kbd "0") 'candidtim/smart-beginning-of-line)

;; Ace Jump
(define-key evil-normal-state-map (kbd "SPC SPC w") 'evil-ace-jump-word-mode)
(define-key evil-normal-state-map (kbd "SPC SPC j") 'evil-ace-jump-line-mode)

;; UI
(define-key evil-normal-state-map (kbd "SPC t d")
  (lambda () (interactive) (load-theme 'doom-solarized-dark t)))
(define-key evil-normal-state-map (kbd "SPC t l")
  (lambda () (interactive) (load-theme 'doom-solarized-light t)))

;; Copilot
(define-key evil-insert-state-map (kbd "C-c c") 'copilot-complete)

;; GPTel
(define-key evil-normal-state-map (kbd "SPC g c") 'gptel)
(define-key evil-normal-state-map (kbd "SPC g s") 'gptel-send)


(provide 'keybindings)
