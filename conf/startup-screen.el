;;
;; Custom welcome screen
;;

(message "Loading startup-screen")

(setq inhibit-startup-message t)

(defun candidtim/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

;; (defun candidtim/kill-startup-buffer ()
;;   (let ((buffer (get-buffer "*Startup*")))
;;     (when buffer
;;       (kill-buffer buffer)
;;       (remove-hook 'buffer-list-update-hook #'candidtim/kill-startup-buffer))))

;; (defun candidtim/startup-message ()
;;   ;; startup:
;;   (switch-to-buffer "*Startup*")
;;   (insert "Let's get started!\n")
;;   (goto-char (point-min))
;;   (text-mode)
;;   (setq buffer-read-only t)
;;   ;; recentf:
;;   (let ((window (split-window-below 10))) ; split and limit to 10 lines
;;     (other-window 1)
;;     (recentf-open-files)
;;     (add-hook 'buffer-list-update-hook #'candidtim/kill-startup-buffer)))

(add-hook 'emacs-startup-hook 'recentf-open-files)
(add-hook 'emacs-startup-hook 'candidtim/display-startup-time)

(provide 'startup-screen)
