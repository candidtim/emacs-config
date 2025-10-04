;;; terminal.el --- Vterm configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Load and configure vterm

;;; Code:

(message "*** STARTUP: Loading terminal configuration")

(use-package vterm
  :config
  (add-to-list 'display-buffer-alist
               '("\\*vterm\\*"
                 (display-buffer-at-bottom)
                 (window-height . 0.25))))

(add-hook 'vterm-mode-hook
  (lambda ()
    (display-line-numbers-mode 0)
    (hl-line-mode 'toggle))) ; FIXME: 'toggle -> -1

;;
;; C-j to open the vterm in a bottom split:
;;

(defun candidtim/list-vterm-buffers ()
  "Return a list of vterm buffers."
  (seq-filter
    (lambda (buf)
      (with-current-buffer buf
        (eq major-mode 'vterm-mode)))
    (buffer-list)))

(defun candidtim/vterm-display-or-new ()
  "Display an existing vterm buffer, or start a new vterm."
  (interactive)
  (let ((vterm-buffers (candidtim/list-vterm-buffers)))
    (if vterm-buffers
        (progn
          (display-buffer (car vterm-buffers))
          (other-window 1))
        (vterm))))

(defun candidtim/vterm-split-toggle ()
  "Toogle a bottom split with a vterm."
  (interactive)
  (if (eq major-mode 'vterm-mode)
      (delete-window)
      (candidtim/vterm-display-or-new)))

(evil-define-key '(normal insert) 'global (kbd "s-j") 'candidtim/vterm-split-toggle)


(provide 'terminal)

;;; terminal.el ends here
