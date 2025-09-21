(add-to-list 'load-path (locate-user-emacs-file "conf"))

(require 'packages)
(require 'ui)
(require 'editor)
(require 'startup-screen)

(with-eval-after-load 'evil
  (when evil-mode
    (require 'keybindings)))
