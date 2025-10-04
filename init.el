;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; candidtim's custom Emacs cnonfiguration

;;; Code:

(add-to-list 'load-path (locate-user-emacs-file "conf"))

(require 'package-init)
(require 'builtin)
(require 'vim)
(require 'ui)
(require 'ide)
(require 'llm)
(require 'terminal)
(require 'keybindings)

;; load local configuration, if available
(when (file-exists-p (locate-user-emacs-file "conf/local.el"))
  (require 'local))

;;; init.el ends here
