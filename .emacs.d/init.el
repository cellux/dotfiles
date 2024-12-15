;; disable native compilation
(setq native-comp-speed -1)
(setq native-comp-async-report-warnings-errors 'silent)
(setq native-comp-jit-compilation nil)

;; do not show startup screen
(setq inhibit-startup-screen t)

;; set location of automatically saved customizations
(setq custom-file
  (expand-file-name "settings.el" user-emacs-directory))

;; I decided to configure everything here via use-package
;(load-file custom-file)

;; add $HOME/bin to exec-path
(add-to-list 'exec-path (expand-file-name "~/bin"))

;; enable all commands
(setq disabled-command-function nil)

(let ((mib (* 1024 1024)))
  ;; allow more consing between garbage collections
  (setq gc-cons-threshold (max gc-cons-threshold (* 128 mib)))
  ;; maximum number of bytes to read from a subprocess in a single chunk
  (setq read-process-output-max (max read-process-output-max (* 1 mib))))

;; default to utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

;; show matching parentheses
(show-paren-mode 1)

;; scroll by one line when reaching top/bottom of screen
(setq scroll-step 1)

;; no menubar
(menu-bar-mode 0)

;; no toolbar
(tool-bar-mode 0)

;; disable electric indent
(electric-indent-mode 0)

;; try to indent first, auto-complete if already indented
(setq tab-always-indent 'complete)

;; indent with spaces by default
(setq-default indent-tabs-mode nil)

;; use two spaces per tab stop
(setq-default tab-width 2)

;; turn on syntax highlighting
(global-font-lock-mode)

;; automatically revert unchanged buffers
;; when the underlying file is changed on disk
(global-auto-revert-mode t)

;; do not make backup files
(setq make-backup-files nil)

;; do not create lock files
(setq create-lockfiles nil)

;; all auto-save files shall go to /tmp
(add-to-list
 'auto-save-file-name-transforms
 `("\\`/\\([^/]*/\\)*\\([^/]*\\)\\'"
   ,(concat temporary-file-directory "\\2") t)
 :at-end)

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

(require 'package)

(add-to-list 'package-archives
  (cons "melpa" "https://melpa.org/packages/") t)

(add-to-list 'package-archives
  (cons "elpa" "https://elpa.gnu.org/packages/") t)

(require 'use-package)

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package zenburn-theme
  :ensure t
  :demand t
  :config
  (load-theme 'zenburn t)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     `(vertico-current ((t (:inherit highlight :extend t))))
     `(completions-annotations ((t (:foreground ,zenburn-fg-05))))))
  (enable-theme 'zenburn))

(use-package doom-modeline
  :ensure t
  :demand t
  :config (doom-modeline-mode 1))

(use-package faces
  :demand t
  :config
  (set-face-attribute 'default nil
                      :family "Droid Sans Mono"
                      :height 110))

(use-package frame
  :demand t
  :custom
  (blink-cursor-mode 0))

(use-package simple
  :demand t
  :bind (("C-z" . undo)))

(use-package dired
  :demand t
  :bind (:map dired-mode-map
              ("E" . wdired-change-to-wdired-mode)))

;; keep track of recently used files
(use-package recentf
  :demand t
  :custom
  (recentf-max-saved-items 50)
  :bind (("C-o" . recentf))
  :config
  (recentf-mode 1))

;; save/restore last cursor position in edited files
(use-package saveplace
  :demand t
  :custom
  (save-place-file (concat user-emacs-directory "places"))
  :config
  (save-place-mode 1))

; easily move between windows
(use-package windmove
  :demand t
  :config
  (windmove-default-keybindings))

;; lookup auth info in Freedesktop Secret Service
(use-package auth-source
  :defer t
  :custom
  (auth-sources '((:source (:secrets default)))))

(use-package compile
  :demand t
  :bind (("<f9>" . compile)))

;; lists
(use-package dash
  :ensure t
  :demand t)

;; files
(use-package f
  :ensure t
  :demand t)

;; hash tables
(use-package ht
  :ensure t
  :demand t)

;; strings
(use-package s
  :ensure t
  :demand t)

(use-package highlight
  :ensure t
  :defer t)

(use-package helpful
  :ensure t
  :defer t
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h C-h" . helpful-at-point)))

(use-package which-key
  :ensure t
  :demand t
  :bind (("C-h K" . which-key-show-top-level))
  :config
  (which-key-mode))

(use-package dmacro
  :ensure t
  :demand t
  :custom `((dmacro-key . ,(kbd "C-S-e")))
  :config (global-dmacro-mode))

(use-package rg
  :ensure t
  :demand t
  :config
  (rg-enable-default-bindings))

(use-package wgrep
  :ensure t
  :defer t)

(use-package projectile
  :ensure t
  :demand t
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "^node_modules$"))

(use-package perspective
  :ensure t
  :demand t
  :bind (("C-x b" . persp-switch-to-buffer*)
         ("C-x k" . persp-kill-buffer*)
         (:map perspective-map ("M-p" . persp-prev)))
  :custom
  (persp-mode-prefix-key (kbd "M-p"))
  (persp-modestring-short t)
  :config
  (persp-mode)
  (advice-add
   'projectile-switch-project-by-name
   :before
   (lambda (project &rest _)
     (persp-switch project))))

(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status)))

(use-package vertico
  :ensure t
  :demand t
  :custom
  (vertico-indexed-mode t)
  :config
  (vertico-mode))

(use-package orderless
  :ensure t
  :demand t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :ensure t
  :demand t
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :demand t
  :bind (("M-e" . embark-act)))

(use-package consult
  :ensure t
  :demand t
  :bind (("M-s p" . (lambda () (interactive) (consult-ripgrep)))
         ("M-s d" . (lambda () (interactive) (consult-ripgrep default-directory)))
         ("M-s f" . (lambda () (interactive) (consult-line)))))

(use-package embark-consult
  :ensure t
  :demand t)

(use-package corfu
  :ensure t
  :demand t
  :init
  (global-corfu-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :ensure t
  :defer t
  :config
  (editorconfig-mode 1))

;; helper for quickly opening this file
(use-package iqa
  :ensure t
  :defer t)

(use-package gdb-mi
  :defer t
  :custom
  (gdb-many-windows nil)
  (gdb-show-main t))

(use-package realgud
  :ensure t
  :defer t)

(use-package dap-mode
  :ensure t
  :defer t
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package lsp-mode
  :ensure t
  :defer t
  :commands lsp
  :custom
  (lsp-enable-snippet nil)
  (lsp-enable-file-watchers nil)
  (lsp-enable-suggest-server-download nil))

(use-package lsp-ui
  :ensure t
  :defer t
  :commands lsp-ui-mode)

(use-package flycheck
  :ensure t
  :defer t
  :hook (prog-mode . flycheck-mode))

(use-package vterm
  :ensure t
  :defer t)

;; org

(use-package org
  :defer t
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ("M-." . org-open-at-point)
         ("M-," . org-mark-ring-goto))
  :custom
  (org-replace-disputed-keys t)
  (org-capture-templates '(("t" "Task" entry (file+headline "tasks.org" "Tasks") nil)
                           ("i" "Idea" entry (file+headline "ideas.org" "Ideas") nil)
                           ("n" "Note" entry (file+headline "notes.org" "Notes") nil)))
  (org-agenda-files '("~/org"))
  (org-refile-targets '((org-agenda-files . (:level . 1))))
  (org-return-follows-link t))

(use-package org-super-agenda
  :ensure t
  :defer t)

(setq rb--org-roam-directory (expand-file-name "~/org-roam"))

(use-package org-roam
  :ensure t
  :defer t
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n n" . org-roam-capture)
         ("C-c n b" . org-roam-buffer-toggle))
  :custom
  (org-roam-directory rb--org-roam-directory)
  :init
  (unless (f-dir-p rb--org-roam-directory)
    (f-mkdir rb--org-roam-directory))
  :config
  (org-roam-db-autosync-mode))

;; file formats

(use-package cmake-mode
  :ensure t
  :defer t
  :mode ("\\`CMakeLists\\.txt\\'"))

(use-package css-mode
  :defer t
  :mode "\\.css\\'"
  :custom (css-indent-offset 2))

(use-package json-mode
  :ensure t
  :defer t
  :mode "\\.json\\'")

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package npm-mode
  :ensure t
  :defer t)

(use-package systemd
  :ensure t
  :defer t)

(use-package toml-mode
  :ensure t
  :defer t
  :mode "\\.toml\\'")

(use-package web-mode
  :ensure t
  :defer t
  :mode ("\\.[jt]sx\\'"
         "\\.html\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-sql-indent-offset 2))

(use-package yaml-mode
  :ensure t
  :defer t
  :mode "\\.ya?ml\\'")

;; programming modes

(use-package c-mode
  :defer t
  :mode "\\.c\\'"
  :hook (c-mode . lsp))

(use-package c++-mode
  :defer t
  :mode ("\\.cc\\'" "\\.cpp\\'" "\\.cxx\\'")
  :hook (c++-mode . lsp))

(use-package clang-format+
  :ensure t
  :defer t
  :hook (c-mode-common . clang-format+-mode)
  :custom
  (clang-format+-always-enable t)
  (clang-format-style "LLVM"))

(use-package flycheck-clang-analyzer
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . flycheck-clang-analyzer-setup))

(use-package flycheck-clang-tidy
  :ensure t
  :defer t
  :hook ((c-mode c++-mode) . flycheck-clang-tidy-setup))

(use-package forth-mode
  :ensure t
  :defer t
  :mode ("\\.f\\'" "\\.fth\\'" "\\.fs\\'" "\\.b\\'"))

(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :init (let ((go-bin-path (expand-file-name "~/go/bin")))
          (unless (member go-bin-path exec-path)
            (setq exec-path (append exec-path (list go-bin-path)))))
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save)))

(use-package groovy-mode
  :ensure t
  :defer t)

(use-package java-mode
  :defer t
  :hook (java-mode . lsp))

(use-package lsp-java
  :ensure t
  :defer t)

(use-package js-mode
  :defer t
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2))

(use-package lua-mode
  :ensure t
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package nim-mode
  :ensure t
  :defer t
  :mode ("\\.nim\\'"
         "\\.nims\\'"
         "\\.nimble\\'"
         "\\`nim.cfg\\'"))

(use-package php-mode
  :ensure t
  :defer t
  :mode "\\.php[345]?\\'")

(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :interpreter "python"
  :custom (python-indent-offset 4))

(use-package flycheck-pyflakes
  :ensure t
  :defer t
  :hook (python-mode . (lambda () (require 'flycheck-pyflakes))))

(use-package blacken
  :ensure t
  :defer t
  :hook (python-mode . blacken-mode))

(use-package rust-mode
  :ensure t
  :defer t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp))

(use-package cargo
  :ensure t
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :ensure t
  :defer t
  :hook (rust-mode . flycheck-rust-setup))

(use-package sh-script
  :defer t
  :custom
  (sh-basic-offset 2))

(use-package zig-mode
  :ensure t
  :defer t
  :mode "\\.zig\\'"
  :hook (zig-mode . lsp))

(use-package typescript-mode
  :ensure t
  :defer t)

(use-package tide
  :ensure t
  :defer t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup))))

;; lisp worlds

(use-package lispy
  :ensure t
  :defer t
  :hook (emacs-lisp-mode
         lisp-mode
         clojure-mode
         scheme-mode
         extempore-mode
         janet-mode
         langsam-mode)
  :config
  (setq lispy-colon-p nil)
  (let ((item (assq 'clojure-mode lispy-goto-symbol-alist)))
    (setcdr item '(cider-find-dwim))))

(use-package sly
  :ensure t
  :defer t
  :custom
  (inferior-lisp-program
   (seq-find (lambda (binary-name)
               (locate-file binary-name exec-path nil 'executable))
             '("sbcl-rb" "sbcl"))))

(use-package sly-asdf
  :ensure t
  :defer t
  :after sly)

(use-package sly-quicklisp
  :ensure t
  :defer t
  :after sly)

(use-package clojure-mode
  :ensure t
  :defer t
  :mode ("\.clj\\'" . clojure-mode))

(use-package cider
  :ensure t
  :defer t
  :config
  (setq cider-repl-display-help-banner nil)
  (setq nrepl-sync-request-timeout 600))

(use-package clj-refactor
  :ensure t
  :defer t)

(use-package flycheck-clojure
  :ensure t
  :defer t
  :hook (clojure-mode . flycheck-clojure-setup))

(use-package janet-mode
  :ensure t
  :defer t
  :mode ("\\.janet\\'"))

(use-package extempore-mode
  :ensure t
  :defer t
  :mode "\\.xtm\\'"
  :init
  (setq user-extempore-directory "/usr/bin/"))

(use-package scheme-mode
  :defer t
  :mode ("\\.scm\\'" . scheme-mode))

(use-package geiser
  :ensure t
  :defer t
  :hook (scheme-mode . geiser-mode)
  :init
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken guile)))

(use-package geiser-chicken
  :ensure t
  :defer t)

(use-package geiser-guile
  :ensure t
  :defer t)

;; music & sound

(use-package csound-mode
  :ensure t
  :defer t
  :mode "\\.csd\\'"
  :custom
  (csound-repl-sr 48000)
  (csound-repl-ksmps 32)
  (csound-repl-nchnls 2)
  (csound-repl-0dbfs 1))

(use-package osc
  :ensure t
  :defer t)

(use-package sclang
  :if (file-accessible-directory-p "/usr/share/emacs/site-lisp/SuperCollider")
  :defer t
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

(use-package sonic-pi
  :ensure t
  :if (file-accessible-directory-p "/usr/lib/sonic-pi")
  :defer t
  :init
  (setq sonic-pi-path "/usr/lib/sonic-pi/"))

(use-package tidal
  :ensure t
  :defer t
  :mode "\\.tidal\\'")

;; private

(use-package rasid-mode
  :if (file-accessible-directory-p "/home/rb/zz/src/github.com/cellux/rasid")
  :defer t
  :mode "\\.rasid\\'"
  :load-path "/home/rb/zz/src/github.com/cellux/rasid"
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package floyd-mode
  :if (file-accessible-directory-p "/home/rb/zz/src/github.com/cellux/floyd")
  :defer t
  :mode "\\.fld\\'"
  :load-path "/home/rb/zz/src/github.com/cellux/floyd"
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package langsam-mode
  :if (file-accessible-directory-p "/home/rb/projects/langsam")
  :defer t
  :mode "\\.l\\'"
  :load-path "/home/rb/projects/langsam")

;; hydra

(use-package hydra
  :ensure t
  :demand t
  :config
  (progn
    (defhydra rb--hydra ()
      "rb tools"
      ("P" (dired (car (prune-directory-list '("~/projects" "~/Projects")))) "projects")
      ("S" (dired (car (prune-directory-list '("~/src" "~/Software/src")))) "src")
      ("i" iqa-find-user-init-file "init.el")
      ("o" (dired "~/org") "org")
      ("p" (package-list-packages) "packages")
      ("q" (dired "~/quicklisp/local-projects") "quicklisp/local")
      ("Q" (dired "~/quicklisp/dists/quicklisp/software") "quicklisp/global"))
    (global-set-key (kbd "<f12>") 'rb--hydra/body)))
