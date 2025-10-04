;;; ide.el -- IDE-like configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Load and configure modules for coding, such as LSP, Copilot, etc.
;;;
;;; Configuration:
;;; (setq projectile-project-search-path '(...))

;;; Code:

(message "*** STARTUP: Loading IDE configuration")

;; Projectile
(use-package projectile
  :init (setq projectile-enable-caching t)
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

;; Highligting
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

;; Lint
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (add-to-list 'display-buffer-alist
               '("\\*Flycheck errors\\*"
                 (display-buffer-at-bottom)
                 (window-height . 0.25))))

(evil-define-key 'normal 'global (kbd "SPC dl")
  (lambda ()
    (interactive)
    (flycheck-list-errors)
    (other-window 1)))


;; LSP
(use-package lsp-mode
  :init
  (evil-define-key 'normal lsp-mode-map
    (kbd "SPC c") lsp-command-map ; FIXME: which-key does not show prefixes
    (kbd "gd") 'lsp-find-definition
    (kbd "gr") 'lsp-find-references
    (kbd "gci") 'lsp-treemacs-call-hierarchy
    (kbd "gco") 'lsp-treemacs-call-hierarchy
    (kbd "gt") 'lsp-find-type-definition
    (kbd "gs") 'lsp-signature-activate
    (kbd "K") 'lsp-describe-thing-at-point
    (kbd "go") 'lsp-treemacs-symbols
    (kbd "ga") 'lsp-execute-code-action
    (kbd "gn") 'lsp-rename
    (kbd "gf") 'lsp-format-buffer
    (kbd "SPC cf") 'lsp-format-buffer
    (kbd "SPC dn") 'flycheck-next-error
    (kbd "SPC dp") 'flycheck-previous-error
    (kbd "SPC dl") 'lsp-treemacs-errors-list)
  (evil-define-key 'visual lsp-mode-map
    (kbd "ga") 'lsp-execute-code-action)
  (setq gc-cons-threshold 100000000)
  (setq lsp-enable-snippet nil)
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-headerline-breadcrumb-enable nil)
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))

(use-package lsp-ui
  :after lsp)

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

;; Company
(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind (:map company-active-map
         ("<tab>" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 2)
  (company-idle-delay 0.5))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; Treemacs
(use-package treemacs
  :config
  (setq treemacs-is-never-other-window t)
  (setq treemacs-project-follow-cleanup t)
  (setq treemacs-position 'right)
  (treemacs-resize-icons 16)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

;; TODO: open files in the most recent window
;;       (use 'treemacs-visit-node-in-most-recently-used-window somehow)

(use-package treemacs-evil
  :after (treemacs evil))

(use-package treemacs-magit
  :after (treemacs magit))

(use-package lsp-treemacs
  :after (treemacs lsp))

(use-package treemacs-projectile
  :after (treemacs projectile))

(defun candidtim/treemacs-toggle ()
  "Toggle Treemacs window."
  ;; TODO: more consistent toggle logic
  (interactive)
  (if (eq (treemacs-current-visibility) 'visible)
    (treemacs) ;; toggles it off
    (treemacs-add-and-display-current-project-exclusively)))
(evil-define-key 'normal 'global (kbd "C-e") #'candidtim/treemacs-toggle)

;; TODO: NERDTree-like keybindings (this implementation does not work...)
;; (evil-define-key 'normal treemacs-mode-map
;;   "ma" 'treemacs-create-file
;;   "md" 'treemacs-delete-file
;;   "mm" 'treemacs-move-file
;;   "mc" 'treemacs-copy-file
;;   "x" 'treemacs-collapse-parent-node
;;   "u" 'treemacs-goto-parent-node
;;   (kbd "C-r") 'treemacs-change-root-to-here
;;   "cd" 'treemacs-set-scope-type
;;   (kbd "S-i") 'treemacs-toggle-show-dotfiles
;;   "t" 'treemacs-visit-node-in-most-recently-used-window
;;   "R" 'treemacs-refres)

;; Copilot
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  ;; no automatic completion, on C-c c only:
  (setq copilot-idle-delay nil)
  (evil-define-key 'insert 'global (kbd "C-c c") 'copilot-complete))


(provide 'ide)

;;; ide.el ends here
