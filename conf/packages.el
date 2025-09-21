;;
;; Install and enable packages
;;
;; Applies minimal configuration requied to enable the installed packages only.
;;

(message "Loading packages")

;;
;; Setup 'use-package' and auto-update
;;

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :init
  (setq auto-package-update-prompt-before-update t)
  :defer t
  :hook (after-init . auto-package-update-maybe))

;;
;; Some packages must be installed early on:
;;

(use-package no-littering)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))


;;
;; Evil
;;

;; Evil Mode
(use-package evil
  :init
  ; needed for evil-collection:
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  ; for consistency:
  (setq evil-want-C-u-scroll t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1))

;; better Evil keybindings
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Evil Leader
(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

;; Goto Chg
(use-package goto-chg)

;; Evil Surround
(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; Ace Jump
(use-package ace-jump-mode)

;;
;; Other packages
;;

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-solarized-dark t))

(use-package all-the-icons) ; NOTE: nun all-the-icons-install-fonts on first install
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Ivy, Counsel, Swiper
(use-package counsel
  :diminish ivy-mode counsel-mode
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-height 15)
  (setq ivy-re-builders-alist ; use fuzzy matching everywhere
        '((swiper . ivy--regex-plus)
          (t . ivy--regex-fuzzy))))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; Projectile
(use-package projectile
  :init
  (when (file-directory-p "~/src")
    (setq projectile-project-search-path '("~/src")))
  (setq projectile-switch-project-action #'projectile-dired)
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy)))

(use-package counsel-projectile
  :after projectile
  :config (counsel-projectile-mode))

;; Magit
(use-package magit)

;; LSP and code editing
(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :init
  (evil-define-key 'normal lsp-mode-map (kbd "SPC c") lsp-command-map)
  (setq gc-cons-threshold 100000000)
  (setq lsp-enable-snippet nil)
  (setq lsp-headerline-breadcrumb-segments '(file))
  :commands (lsp lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration))
;; FIXME: which-key no prefixes

(use-package lsp-ui
  :after lsp)

(use-package lsp-ivy
  :after lsp
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :after lsp)

(use-package treemacs-projectile
  :after (treemacs projectile))

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

;; Helpful
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

;; Fic Mode
(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode))

;; Languages
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Copilot
(use-package copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("C-<tab>" . 'copilot-accept-completion-by-word)
              ("C-n" . 'copilot-next-completion)
              ("C-p" . 'copilot-previous-completion))
  :config
  ;; no automatic completion, see keybindings:
  (setq copilot-idle-delay nil))

;; GPTel
(use-package gptel
  :init
  (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  (setq gptel-api-key (getenv "OPENAI_API_KEY")))


(provide 'packages)
