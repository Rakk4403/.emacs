;; listup packages required
(setq package-list '(use-package
                     auto-complete
                     projectile
                     cmake-mode
                     wakatime-mode
                     sr-speedbar
                     company
                     company-c-headers
                     ggtags
                     ecb
                     ;; color-theme-solarized
                     smooth-scrolling
                     which-key
                     js2-mode
                     ac-js2
                     ivy
                     counsel
                     swiper
                     magit
                     pyenv-mode
                     multi-term
                     flycheck
                     json-mode
                     web-mode
                     exec-path-from-shell
                     tern
                     tern-auto-complete
                     elpy
                     rjsx-mode
                     docker
                     dockerfile-mode
                     ace-window
                     ))

;; add melpa, marmalade
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

;; elpy mode (for python)
(elpy-enable)

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
(setq-default c-basic-offset 2
              js-indent-level 2
              tab-width 4
              indent-tabs-mode nil)  ;; indent with spaces

(add-hook 'python-mode-hook
          (lambda ()
            (setq tab-width (default-value 'tab-width))))

;; c++-mode for .h
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook
           (lambda ()
             (setq-default indent-tabs-mode nil)))

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
 '(custom-enabled-themes (quote (wombat)))
 '(global-wakatime-mode t)
 '(package-selected-packages
   (quote
    (exec-path-from-shell web-mode json-mode tern tern-auto-complete flycheck color-theme-monokai anzu multi-term pyenv-mode magit counsel ivy js2-mode which-key smooth-scrolling ecb smart-tabs-mode ggtags company-c-headers company sr-speedbar wakatime-mode cmake-mode projectile auto-complete use-package)))
 '(safe-local-variable-values (quote ((eval highlight-regexp "^ *"))))
 '(wakatime-cli-path "/home/chris/.local/bin/wakatime")
 '(wakatime-python-bin "/usr/bin/python"))
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

;; setup CEDET for removing no clang error in linux
(semantic-mode 1)


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

;; (require 'color-theme)
;; (color-theme-solarized)
(put 'narrow-to-region 'disabled nil)

;; transparent window
(set-frame-parameter (selected-frame) 'alpha '(85 65))
(add-to-list 'default-frame-alist '(alpha 85 65))

;; smooth scrolling
(use-package smooth-scrolling
  :init
  (setq scroll-margin 5
        scroll-conservatively 9999
        scroll-step 1)
  )

;; numbering find results
(use-package anzu
  :ensure t
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode +1))


(use-package which-key
  :config
  (which-key-mode))


(use-package js2-mode
  :config
  (js2-mode))


;; ivy
(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))


;; pyenv-mode
(pyenv-mode)

;; C-c p p activates pyenv-mode in projectile
(require 'pyenv-mode)

(defun projectile-pyenv-mode-set ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (member project (pyenv-mode-versions))
        (pyenv-mode-set project)
      (pyenv-mode-unset))))

(add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)



;; jshint with flycheck
(require 'flycheck)
;; terun on flycheck globally
(add-hook 'after-init-hook #'global-flycheck-mode)

(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))
;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hupefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

              
;; json auto mode
(add-to-list 'auto-mode-alist'("\\.json$" . js-mode))

;; js2 auto load
;; disable minor-mode by using eslint
;;(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; ac-js2 settings
(setq js2-highlight-level 3)

;; tern settings
(require 'tern)
(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . rjsx-mode))

;; python
(add-to-list 'auto-mode-alist '("\\.py$" . elpy-mode))
(add-to-list 'auto-mode-alist '("\\.py$" . python-mode))

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)
