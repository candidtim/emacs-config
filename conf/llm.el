;;; llm.el --- LLM and MCP -*- lexical-binding: t; -*-

;;; Commentary:
;;; LLM client and related configuration (MCP, etc.)
;;;
;;; To configure GPT API tokens:
;;;
;;;   (setq gptel-api-key ...)
;;;   (gptel-make-anthropic "Claude" :stream t :key ...)
;;;
;;; To configure MCP servers:
;;;
;;;   (eval-after-load 'mcp-hub
;;;     (setq mcp-hub-servers '(...))

;;; Code:

(message "*** STARTUP: Loading LLM packages")

(use-package gptel
  :config
  (require 'gptel-integrations)
  (push '(python-mode . "# ") gptel-response-prefix-alist)
  (push '(elisp-mode . ";; ") gptel-response-prefix-alist)
  (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(use-package mcp
  :after gptel
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))

(evil-define-key 'normal 'global (kbd "SPC g c") 'gptel)
(evil-define-key 'normal 'global (kbd "SPC g m") 'gptel-menu)
(evil-define-key 'visual 'global (kbd "SPC g s") 'gptel-send)


(provide 'llm)

;;; llm.el ends here
