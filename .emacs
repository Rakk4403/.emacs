; listup packages required
(setq package-list '(auto-complete
		     projectile))
		     
; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; start auto-complete with emacs
(require 'auto-complete)
;do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

; projectile
(require 'projectile)
;; turn on projectile
(projectile-global-mode)
