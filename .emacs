; melpa, marmalade를 추가한다.
(require 'package)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

; start auto-complete with emacs
(require 'auto-complete)
;do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
