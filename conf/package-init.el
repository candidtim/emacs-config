;;; package-init.el --- Package system initizliation -- lexical-binding: t; -*-

;;; Commentary:
;;; Configures and initializes `use-package`.
;;; Loads packages that must be loaded early.

;;; Code:

(message "*** STARTUP: Initializing package system")

;;
;; Setup 'use-package' and auto-update
;;

(setq package-archives '(("elpa" . "https://elpa.gnu.org/packages/")
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

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package no-littering)


(provide 'package-init)

;;; package-init.el ends here
