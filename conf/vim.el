;;; vim.el --- Evil configuration -*- lexical-binding: t; -*-

;;; Commentary:
;;; Evil and some configuration for better Vim compatibility, including
;;; typical Vim plugins (surround, word jump, etc.).

;;; Code:

(message "*** STARTUP: Loading Evil")

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

;; Better Evil keybindings
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

;; Avy
(use-package avy)


;;
;; Better Vim compatibility
;;

;; Emacs-Vim Esc/C-g compatibility
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(evil-define-key 'insert 'global (kbd "C-g") 'evil-normal-state)

;; j/k use visual lines:
;; FIXME: cannot use because overriding below with custom implemenations
;;        => make those work over visual lines
;; (evil-global-set-key 'motion "j" 'evil-next-visual-line)
;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

;; j/k are full linewise in motions:
(evil-define-motion candidtim/evil-next-line (count)
  :type line
  (let ((line-move-visual (and visual-line-mode (not (evil-operator-state-p)))))
    (evil-line-move (or count 1))))
(evil-define-motion candidtim/evil-previous-line (count)
  :type line
  (let ((line-move-visual (and visual-line-mode (not (evil-operator-state-p)))))
    (evil-line-move (- (or count 1)))))
(evil-define-key 'motion 'global "j" 'candidtim/evil-next-line)
(evil-define-key 'motion 'global "k" 'candidtim/evil-previous-line)

;; 0 <-> ^
(defun candidtim/beginning-of-line ()
  "Toggle between beginning of line and first non-whitespace character."
  (interactive)
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (= oldpos (point))
      (beginning-of-line))))
(evil-define-key 'normal 'global "0" 'candidtim/beginning-of-line)


(provide 'vim)

;;; vim.el ends here
