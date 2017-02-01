;; listup packages required
(setq package-list '(auto-complete
                     projectile
                     cmake-mode
                     wakatime-mode
                     sr-speedbar
                     company
                     company-c-headers
                     ggtags
                     smart-tabs-mode
                     ecb
                     color-theme-solarized))

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
 '(global-wakatime-mode t)
 '(safe-local-variable-values (quote ((eval highlight-regexp "^ *"))))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin "/System/Library/Frameworks/Python.framework/Versions/2.7/Resources/Python.app/Contents/MacOS/Python"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Source code Navigation Setting

(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'javascript-mode)
              (ggtags-mode 1))))

(define-key ggtags-mode-map (kbd "C-c g s") 'ggtags-find-other-symbol)
(define-key ggtags-mode-map (kbd "C-c g h") 'ggtags-view-tag-history)
(define-key ggtags-mode-map (kbd "C-c g r") 'ggtags-find-reference)
(define-key ggtags-mode-map (kbd "C-c g f") 'ggtags-find-file)
(define-key ggtags-mode-map (kbd "C-c g c") 'ggtags-create-tags)
(define-key ggtags-mode-map (kbd "C-c g u") 'ggtags-update-tags)

(define-key ggtags-mode-map (kbd "M-,") 'pop-tag-mark)

;; if use HELM, uncomment below then comment upper region
;; (setq
;;  helm-gtags-ignore-case t
;;  helm-gtags-auto-update t
;;  helm-gtags-use-input-at-cursor t
;;  helm-gtags-pulse-at-cursor t
;;  helm-gtags-prefix-key "\C-cg"
;;  helm-gtags-suggested-key-mapping t
;;  )

;; (require 'helm-gtags)
;; ;; Enable helm-gtags-mode
;; (add-hook 'dired-mode-hook 'helm-gtags-mode)
;; (add-hook 'eshell-mode-hook 'helm-gtags-mode)
;; (add-hook 'c-mode-hook 'helm-gtags-mode)
;; (add-hook 'c++-mode-hook 'helm-gtags-mode)
;; (add-hook 'asm-mode-hook 'helm-gtags-mode)

;; (define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
;; (define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
;; (define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
;; (define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
;; (define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
;; (define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


;; speedbar setting
(setq speedbar-show-unknown-files t)


;; company settings
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; settings for company mode with Clang
;; (setq company-backends (delete 'company-semantic company-backends))
;; (define-key c-mode-map [(tab)] 'company-complete)
;; (define-key c++-mode-map [(tab)] 'company-complete)

;; company-c-headers settings
(add-to-list 'company-backends 'company-c-headers)
;; if needed custom path, use below
;; (add-to-list 'company-c-headers-path-system "/usr/include/somewhere/")
;; if use project local(.dir-locals.el), use "company-c-headers-path-user" instead of "-system"


;; gdb settings
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; Add cmake listfile names to the mode list.
(setq auto-mode-alist
      (append
       '(("CMakeLists\\.txt\\'" . cmake-mode))
       '(("\\.cmake\\'" . cmake-mode))
       auto-mode-alist))

(autoload 'cmake-mode "~/.emacs.d/elpa/cmake-mode-20110824/cmake-mode.el" t)

(require 'color-theme)
(color-theme-solarized)
(put 'narrow-to-region 'disabled nil)

;; transparent window
(set-frame-parameter (selected-frame) 'alpha '(85 65))
(add-to-list 'default-frame-alist '(alpha 85 65))

