;; listup packages required
(setq package-list '(auto-complete
                     projectile
                     cmake-mode
                     wakatime-mode))

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; start auto-complete with emacs
(require 'auto-complete)
;; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)

;; projectile
(require 'projectile)
;; turn on projectile
(projectile-global-mode)

;; indentations
(setq-default c-basic-offset 4
              tab-width 4
			  indent-tabs-mode nil)  ;; indent with spaces

(smart-tabs-insinuate 'c 'c++ 'python 'javascript)
(smart-tabs-advice python-indent-line-1 python-indent)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
                            (setq tab-width (default-value 'tab-width))))

;; ecb-settings
(require 'ecb)
;; if just want, just
;; (require 'ecb)

;; python-mode-settings
;;(add-hook 'python-mode-hook
  ;;        (lambda ()
    ;;        (setq-default indent-tabs-mode nil)
      ;;      (setq-default tab-width 4)
        ;;    (setq-default py-indent-tabs-mode nil)
          ;;  (add-to-list 'write-file-functions 'delete-trailing-whitespace)))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval highlight-regexp "^ *")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


