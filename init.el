;;                 _     _       _         _  _            _ 
;;     /\         | |   ( )     (_)       (_)| |          | |
;;    /  \    ___ | |__ |/ ___   _  _ __   _ | |_     ___ | |
;;   / /\ \  / __|| '_ \  / __| | || '_ \ | || __|   / _ \| |
;;  / ____ \ \__ \| | | | \__ \ | || | | || || |_  _|  __/| |
;; /_/    \_\|___/|_| |_| |___/ |_||_| |_||_| \__|(_)\___||_|

;; Quicker startup by setting garbage collect high
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

;; Setup package control
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(setq package-enable-at-startup nil)
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;;
;; G E N E R A L
;;
(setq inhibit-splash-screen t
      inhibit-startup-message t
      initial-major-mode 'org-mode)
(tool-bar-mode -1) ; No toolbar
(scroll-bar-mode -1) ; Hide scrollbars
(menu-bar-mode -1) ; No menubar
(show-paren-mode t) ; Highlights matching parens
(setq initial-scratch-message "") ; No scratch text
(fset 'yes-or-no-p 'y-or-n-p) ; y/n instead of yes/no
(setq-default indent-tabs-mode nil)
(column-number-mode t)
(delete-selection-mode 1) ; Replace selection on insert
(setq vc-follow-symlinks t) ; Always follow symlinks
(setq custom-file "~/.emacs.d/custom.el") ; Set custom file
(load custom-file 'noerror) ; Load custom file
(setq scroll-margin 10 ; Scroll like vim
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)
(setq explicit-shell-file-name
      (if (file-readable-p "/usr/bin/zsh") "/usr/bin/zsh" "/bin/bash"))

;;
;; E V I L
;;

(use-package evil
  :demand
  :init
  (setq evil-want-C-u-scroll t) ; Unbind <C-u> for evil mode's use
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode t)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq-default evil-symbol-word-search t)
  (define-key evil-ex-map "e " 'ido-find-file) ; Trigger file completion :e
  )

;;
;; P A C K A G E S
;;

(use-package cherry-blossom-theme)

;; Saner defaults for emacs
(use-package better-defaults)

;; Better M-x
(use-package smex
  :demand
  :bind ("M-x" . smex))

;; Use newest org with additional packages
(use-package org
  :pin org
  :ensure org-plus-contrib
  :config
  (requrie 'ox-confluence))

;; Better looking org headers
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; Displays helpful popups for key bindings
(use-package which-key
  :config
  (which-key-mode))

;; Magic git
(use-package magit
  :config
  (global-set-key "\C-x\g" 'magit-status)
  (use-package evil-magit))

;; Interface for accessing Jira's REST API
(use-package org-jira
  :config
  (setq jiralib-url "https://indigoca.atlassian.net"))

;; org to confluence
(add-to-list 'load-path "c:/Users/aramanathan/Documents/Uni-Stuff/")
(use-package ox-confluence
  :ensure nil)

;;
;; C O M P A N Y
;;
(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.5) ; Delay to complete
  (setq company-minimum-prefix-length 1)
  (setq company-selection-wrap-around t) ; Loops around suggestions

  (if (display-graphic-p)
      (define-key company-active-map [tab] 'company-select-next)
    (define-key company-active-map (kbd "C-i") 'company-select-next))

  ;; C / C++
  (setq company-clang-insert-arguments nil)
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)

  (use-package company-irony
    :config
    (eval-after-load 'company
      '(add-to-list 'company-backends 'company-irony)))

  (ignore-errors
    (require 'color)
    (let ((bg (face-attribute 'default :background)))
      (custom-set-faces
       `(company-tooltip ((t (:inherit default :background ,(color-lighten-name bg 2)))))
       `(company-scrollbar-bg ((t (:background ,(color-lighten-name bg 10)))))
       `(company-scrollbar-fg ((t (:background ,(color-lighten-name bg 5)))))
       `(company-tooltip-selection ((t (:inherit font-lock-function-name-face))))
       `(company-tooltip-common ((t (:inherit font-lock-constant-face))))))))

(use-package company-jedi
  :config
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  (add-hook 'python-mode-hook 'my/python-mode-hook))

(use-package omnisharp
  :config
  (setq omnisharp-server-executable-path "c:/emacs/omnisharp/OmniSharp.exe")
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (eval-after-load 'company
    '(add-to-list 'company-backends 'company-omnisharp)))

;;
;; B A C K U P S
;;
(setq backup-by-copying t) ; Stop shinanigans with links
(setq backup-directory-alist '((".*" . "~/.bak.emacs/backup/")))
;; Creates directory if it doesn't already exist
(if (eq nil (file-exists-p "~/.bak.emacs/"))
    (make-directory "~/.bak.emacs/"))
;; Creates auto directory if it doesn't already exist
(if (eq nil (file-exists-p "~/.bak.emacs/auto"))
    (make-directory "~/.bak.emacs/auto"))
;; backup in one place. flat, no tree structure
(setq auto-save-file-name-transforms '((".*" "~/.bak.emacs/auto/" t)))

(use-package ox-twbs)
