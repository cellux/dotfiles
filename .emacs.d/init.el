;; do not show startup screen
(setq inhibit-startup-screen t)

;; set location of automatically saved customizations
(setq custom-file
  (expand-file-name "settings.el" user-emacs-directory))

;; I decided to configure everything here via use-package
;(load-file custom-file)

;; add $HOME/bin to exec-path
(let ((user-bin-path (expand-file-name "~/bin")))
  (unless (member user-bin-path exec-path)
    (setq exec-path (append exec-path (list user-bin-path)))))

;; enable all commands
(setq disabled-command-function nil)

;; default to utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

;; show matching parentheses
(show-paren-mode 1)

;; highlight current line
(hl-line-mode t)

;; scroll by one line when reaching top/bottom of screen
(setq scroll-step 1)

;; don't blink the cursor
(blink-cursor-mode 0)

;; no toolbar
(tool-bar-mode 0)

;; disable electric indent
(electric-indent-mode 0)

;; do not indent with tabs by default
(setq-default indent-tabs-mode nil)

;; use two spaces per tab stop
(setq-default tab-width 2)

;; indentation settings which may affect several modes
(setq c-basic-offset 2)
(setq c-default-style "linux")
(setq sh-basic-offset 2)

;; turn on syntax highlighting
(global-font-lock-mode)

;; automatically revert unchanged buffers
;; when the underlying file is changed on disk
(global-auto-revert-mode t)

;; do not make backup files
(setq make-backup-files nil)

;; remember recently opened files
(setq recentf-max-saved-items 50)
(recentf-mode 1)

;; use the mouse wheel
(mouse-wheel-mode t)

;; show both line and column numbers in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; indicate empty lines beyond the end of file
(setq-default indicate-empty-lines t)

;; show name of current buffer in frame title
(setq frame-title-format "%f")

;; searches and matches should ignore case
(setq-default case-fold-search t)

;; follows symlinks to version controlled files
(setq vc-follow-symlinks t)

;;; packages

;; do not activate all installed packages automatically
(setq package-enable-at-startup nil)

(require 'package)

(add-to-list 'package-archives
  (cons "melpa" "https://melpa.org/packages/") t)

(add-to-list 'package-archives
  (cons "elpa" "https://elpa.gnu.org/packages/") t)

(setq package-selected-packages
  '(use-package
     zenburn-theme
     queue
     spinner
     company
     rainbow-delimiters
     iedit
     highlight
     treemacs
     helpful
     realgud
     eyebrowse
     expand-region
     deadgrep
     lispy
     paredit
     helm
     iqa
     projectile helm-projectile
     helm-dash
     helm-ag
     hydra
     sly helm-sly sly-asdf sly-quicklisp
     flycheck
     flycheck-clang-analyzer
     flycheck-clang-tidy
     clojure-mode cider flycheck-clojure
     janet-mode
     undo-tree
     yaml-mode
     go-mode company-go
     forth-mode
     rust-mode cargo flycheck-rust
     lsp-mode lsp-ui lsp-treemacs helm-lsp
     toml-mode
     json-mode
     ace-mc ace-jump-mode
     smart-jump dumb-jump
     osc
     markdown-mode
     magit
     lua-mode
     js2-mode
     npm-mode
     geiser
     systemd))

;; initialize Emacs package manager
(package-initialize)

(require 'use-package)

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t)
    (set-frame-font
      (font-spec :name "Droid Sans Mono" :size 15)
      nil t)))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package ace-mc
  :bind (("C-(" . ace-mc-add-multiple-cursors)
         ("C-M-(" . ace-mc-add-multiple-cursors)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; save/restore last cursor position in edited files
(use-package saveplace
  :custom
  (save-place-file (concat user-emacs-directory "places"))
  :config
  (save-place-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

;; helper for quickly opening this file
(use-package iqa)

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-o" . helm-recentf))
  :config
  (helm-mode 1))

(use-package helm-dash
  :commands helm-dash)

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (projectile-mode 1))

(use-package helm-projectile
  :config
  (helm-projectile-on))

(use-package deadgrep
  :bind ("M-<f7>" . deadgrep))

(use-package dumb-jump
  :custom
  (dumb-jump-max-find-time 10)
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'helm)
  :bind (("M-." . dumb-jump-go)
         ("C-x 4 M-." . dumb-jump-go-other-window)
         ("M-," . dumb-jump-back)))

(use-package smart-jump
  :config
  (smart-jump-setup-default-registers))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package iedit
  :defer t)

(use-package eyebrowse
  :config
  (eyebrowse-mode t))

(use-package treemacs
  :commands treemacs)

; easily move between windows
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package gdb-mi
  :custom
  (gdb-many-windows nil)
  (gdb-show-main t))

(use-package realgud
  :defer t)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 1))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-enable-snippet nil))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package c-mode
  :mode "\\.c\\'"
  :hook (c-mode . lsp))

(use-package c++-mode
  :mode ("\\.cc\\'" "\\.cpp\\'" "\\.cxx\\'")
  :hook (c++-mode . lsp))

(use-package forth-mode
  :mode ("\\.f\\'" "\\.fth\\'" "\\.fs\\'" "\\.b\\'"))

(use-package janet-mode
  :mode ("\\.janet\\'"))

(use-package extempore-mode
  :defer t
  :mode "\\.xtm\\'"
  :init
  (setq user-extempore-directory "/usr/bin/"))

(use-package geiser
  :mode ("\\.scm\\'" . geiser-mode)
  :init (setq geiser-chicken--binary "chicken-csi"))

(use-package clojure-mode
  :mode ("\.clj\\'" . clojure-mode))

(use-package cider
  :defer t)

(use-package symex
  :bind (("C-;" . symex-mode-interface)))

(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package toml-mode
  :mode "\\.toml\\'")

(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :config (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package go-mode
  :mode "\\.go\\'"
  :init (let ((go-bin-path (expand-file-name "~/go/bin")))
          (unless (member go-bin-path exec-path)
            (setq exec-path (append exec-path (list go-bin-path)))))
  :hook (go-mode . lsp))

(use-package sclang
  :load-path "/usr/share/emacs/site-lisp/SuperCollider"
  :mode ("\\.sc\\'" . sclang-mode)
  :mode ("\\.scd\\'" . sclang-mode)
  :preface
  (defun sclang:find-definition-at-point ()
    (interactive)
    (let ((subject (thing-at-point 'word t)))
      (sclang-find-definitions subject)))
  :bind (:map sclang-mode-map
          ("M-." . sclang:find-definition-at-point)
          ("M-," . sclang-pop-definition-mark)))

(use-package php-mode
  :defer t
  :mode "\\.php[345]?\\'")

(use-package python-mode
  :mode "\\.py\\'"
  :interpreter "python"
  :custom (python-indent-offset 4))

(use-package sly
  :commands sly
  :custom (inferior-lisp-program
           (seq-find (lambda (binary-name)
                       (locate-file binary-name exec-path nil 'executable))
                     '("sbcl-rb" "sbcl"))))

(use-package sly-asdf
  :after sly)

(use-package sly-quicklisp
  :after sly)

(use-package lispy-mode
  :hook (emacs-lisp-mode
         lisp-mode
         clojure-mode
         scheme-mode
         extempore-mode))

(use-package llvm-mode
  ; did not find this package on MELPA
  :disabled t
  :mode ("\\.ir\\'" "\\.ll\\'"))

(use-package css-mode
  :mode "\\.css\\'"
  :custom (css-indent-offset 2))

(use-package web-mode
  :commands web-mode)

(use-package js2-mode
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  (js2-bounce-indent-p nil))

(use-package json-mode
  :mode "\\.json\\'")

(use-package sonic-pi
  :defer t
  :init
  (setq sonic-pi-path "/usr/lib/sonic-pi/"))

(use-package rasid-mode
  :defer t
  :mode "\\.rasid\\'"
  :load-path "/home/rb/zz/src/github.com/cellux/rasid"
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package floyd-mode
  :defer t
  :mode "\\.fld\\'"
  :load-path "/home/rb/zz/src/github.com/cellux/floyd"
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package acme-mode
  :defer t
  :mode "\\.a\\'"
  :load-path "/home/rb/src/acme-mode")

(use-package hydra
  :config
  (progn
    (defhydra hydra-rb ()
      "rb tools"
      ("P" (dired "~/projects") "projects")
      ("S" (dired "~/src") "src")
      ("i" iqa-find-user-init-file "init.el")
      ("p" (package-list-packages) "packages")
      ("q" (dired "~/quicklisp/local-projects") "quicklisp/local")
      ("Q" (dired "~/quicklisp/dists/quicklisp/software") "quicklisp/global"))
    (global-set-key (kbd "<f12>") 'hydra-rb/body)))
