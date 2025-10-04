;;; ui.el --- UI and UX configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Better Emacs UI and UX with Ivy, Counsel, etc.

;;; Code:

(message "*** STARTUP: UI and UX configuration")

;;
;; UI
;;

;; Font
(set-face-attribute 'default nil
                    :family "Fira Code"
                    :height 140
                    :weight 'light)

;; Theme
(use-package doom-themes
  :init (load-theme 'doom-gruvbox t))

(use-package all-the-icons) ; NOTE: run all-the-icons-install-fonts on first install
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Toggle between light and dark themes
(evil-define-key 'normal 'global (kbd "SPC t t")
  (lambda ()
    (interactive)
    (let ((current-theme (car custom-enabled-themes)))
      (mapc 'disable-theme custom-enabled-themes)
      (if (eq current-theme 'doom-gruvbox)
          (load-theme 'doom-gruvbox-light t)
        (load-theme 'doom-gruvbox t)))))

;; Startup screen (dashboard)
(use-package dashboard
  :config
  (setq dashboard-banner-logo-title "Welcome home!")
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content t)
  (setq dashboard-vertically-center-content t)
  (setq dashboard-icon-type 'all-the-icons)
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-items '((recents   . 5)
                          (projects  . 5)))
  (setq initial-buffer-choice (lambda () (get-buffer-create dashboard-buffer-name)))
  (dashboard-setup-startup-hook))

;; "Lock screen" (switch to the *dashboard* buffer)
(evil-define-key 'normal 'global (kbd "s-l") 'dashboard-open)


;;
;; UX
;;

;; Ivy, Counsel, Swiper
(use-package counsel
  :diminish ivy-mode counsel-mode
  :config
  (ivy-mode 1)
  (counsel-mode 1)
  (setq ivy-height 15)
  ;; Use fuzzy matching everywhere:
  (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))
  ;; Evilish Ivy:
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

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


(provide 'ui)

;;; ui.el ends here
