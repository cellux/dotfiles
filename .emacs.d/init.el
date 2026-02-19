;; -*- lexical-binding: t -*-

;;; packages

(require 'package)

(add-to-list 'package-archives
  (cons "melpa" "https://melpa.org/packages/") t)

(add-to-list 'package-archives
  (cons "elpa" "https://elpa.gnu.org/packages/") t)

(require 'use-package)

(use-package emacs
  :custom

  ;; disable native compilation
  (native-comp-speed -1)
  (native-comp-async-report-warnings-errors 'silent)
  (native-comp-jit-compilation nil)

  ;; don't beep
  (ring-bell-function 'ignore)

  ;; maximize frames
  (default-frame-alist '((fullscreen . maximized)))

  ;; do not show startup screen
  (inhibit-startup-screen t)

  ;; set location of automatically saved customizations
  (custom-file (expand-file-name "settings.el" user-emacs-directory))

  ;; scroll by one line when reaching top/bottom of screen
  (scroll-step 1)

  ;; try to indent first, auto-complete if already indented
  (tab-always-indent 'complete)

  ;; indent with spaces by default
  (indent-tabs-mode nil)

  ;; use two spaces per tab stop
  (tab-width 2)

  ;; do not make backup files
  (make-backup-files nil)

  ;; do not create lock files
  (create-lockfiles nil)

  ;; indicate empty lines beyond the end of file
  (indicate-empty-lines t)

  ;; searches and matches should ignore case
  (case-fold-search t)

  ;; follow symlinks to version controlled files
  (vc-follow-symlinks t)

  :config

  ;; add $HOME/bin to exec-path
  (let ((home-bin-path (expand-file-name "~/bin")))
    (when (file-accessible-directory-p home-bin-path)
      (add-to-list 'exec-path home-bin-path)))

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

  ;; no menubar
  (menu-bar-mode 0)

  ;; no toolbar
  (tool-bar-mode 0)

  ;; disable electric indent
  (electric-indent-mode 0)

  ;; turn on syntax highlighting
  (global-font-lock-mode 1)

  ;; use Google's Noto Color Emoji font for emojis if available
  (let ((noto-color-emoji (font-spec :family "Noto Color Emoji")))
    (when (find-font noto-color-emoji)
      (set-fontset-font t 'emoji noto-color-emoji)))

  ;; automatically revert unchanged buffers
  ;; when the underlying file is changed on disk
  (global-auto-revert-mode t)

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

  ;; show name of current buffer in frame title
  (setq frame-title-format "%f"))

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

(defun rb--eshell-other-window ()
  (interactive)
  (let* ((dir (if (eq major-mode 'dired-mode)
                  (dired-current-directory)
                default-directory))
         (bufname (format "*eshell: %s*" dir))
         (w (select-window (split-window-horizontally)))
         (buf (switch-to-buffer (get-buffer-create bufname)))
         (eshell-buffer-name bufname))
    (eshell)
    (keymap-local-set "C-d" (lambda ()
                              (interactive)
                              (kill-buffer buf)
                              (delete-window)))))

(use-package eshell
  :demand t
  :bind (("C-c RET" . rb--eshell-other-window)))

(use-package dired
  :demand t
  :bind (:map dired-mode-map
              ("E" . wdired-change-to-wdired-mode))
  :custom
  (dired-dwim-target t))

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
  :bind (("<f9>" . compile))
  :init
  (when (file-directory-p "/etc/debuginfod")
    (let* ((files (file-expand-wildcards "/etc/debuginfod/*.urls"))
           (lines (mapcan (lambda (filename)
                            (with-temp-buffer
                              (insert-file-contents filename)
                              (split-string (buffer-string) "\n" t))) files))
           (debuginfod-urls (mapconcat #'identity lines)))
      (setenv "DEBUGINFOD_URLS" debuginfod-urls))))

(use-package inspector
  :ensure t
  :defer t)

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

;; parsing expression grammars
(use-package peg
  :ensure t
  :defer t)

;; tree-sitter
(use-package treesit
  :demand t
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (c "https://github.com/tree-sitter/tree-sitter-c")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (java "https://github.com/tree-sitter/tree-sitter-java")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript"))))

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

(use-package rg
  :ensure t
  :demand t
  :config
  (rg-enable-default-bindings))

(use-package wgrep
  :ensure t
  :defer t)

(use-package xref
  :demand t
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :ensure t
  :demand t
  :custom
  (dumb-jump-prefer-searcher 'rg)
  :config
  ;; xref package sets etags--xref-backend as default at depth 90
  ;;
  ;; with depth 80 we ensure that dumb-jump has priority over etags
  ;; but not over any backend added by prog modes later
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 80))

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
         (:map perspective-map ("M-[" . persp-prev)))
  :custom
  (persp-mode-prefix-key (kbd "M-["))
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
  :bind (("M-s p" . consult-ripgrep)
         ("M-s d" . (lambda () (interactive) (consult-ripgrep default-directory)))
         ("M-s f" . consult-line)
         ("C-x b" . consult-buffer)))

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
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-emacs-lisp-load-path 'inherit)
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
         ("M-," . org-mark-ring-goto)
         ("C-c C-x C-l" . org-toggle-link-display)
         :map org-src-mode-map
         ("C-c `" . org-edit-src-abort))
  :custom
  (org-replace-disputed-keys t)
  (org-capture-templates '(("t" "Task" entry (file "tasks.org") nil)
                           ("T" "Tool" entry (file "tools.org") nil)
                           ("l" "Learn" entry (file "learn.org") nil)
                           ("i" "Idea" entry (file "ideas.org") nil)
                           ("n" "Note" entry (file "notes.org") nil)))
  (org-agenda-files '("~/org/agenda.org"))
  (org-refile-targets '((org-agenda-files . (:level . 1))))
  (org-refile-use-outline-path 'file)
  (org-return-follows-link t)
  (org-confirm-babel-evaluate nil)
  :hook
  ((org-mode . visual-line-mode)
   (org-mode . (lambda () (toggle-word-wrap 1)))
   (before-save . whitespace-cleanup)))

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

(use-package org-jira
  :if (file-accessible-directory-p (expand-file-name  "~/.org-jira"))
  :init
  (when-let* ((auth-info (car (auth-source-search :service "jira")))
              (host (plist-get auth-info :host))
              (user (plist-get auth-info :user))
              (pat (auth-info-password auth-info)))
    (setq jiralib-url (format "https://%s/" host))
    (setq jiralib-user user)
    (setq jiralib-token (cons "Authorization" (concat "Bearer " pat)))))

(use-package ob-clojure
  :custom
  (org-babel-clojure-backend 'cider))

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

(use-package forth-mode
  :ensure t
  :defer t
  :mode ("\\.f\\'" "\\.fth\\'" "\\.fs\\'" "\\.b\\'"))

(use-package go-mode
  :ensure t
  :defer t
  :mode "\\.go\\'"
  :init (let ((go-bin-path (expand-file-name "~/go/bin")))
          (when (file-accessible-directory-p go-bin-path)
            (add-to-list 'exec-path go-bin-path)))
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
  :mode "\\.m?js\\'"
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
  :custom
  (lispy-no-space t)
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
  :custom
  (cider-repl-display-help-banner nil)
  (cider-allow-jack-in-without-project t)
  (nrepl-sync-request-timeout 600))

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

;; AI

(use-package gptel-agent
  :ensure t
  :demand t
  :after gptel)

(use-package gptel
  :ensure t
  :demand t
  :hook ((org-mode . gptel-mode)
         (gptel-mode . visual-line-mode)
         (gptel-mode . (lambda () (toggle-word-wrap 1))))
  :config
  (require 'gptel-org)
  (setq gptel-org-branching-context t)
  (setf (alist-get 'org-mode gptel-prompt-prefix-alist) "@rb ")
  (setf (alist-get 'org-mode gptel-response-prefix-alist) "@ai ")
  (setq gptel-expert-commands t)
  (setq gptel-curl-file-size-threshold 4096)
  (setq gptel-max-tokens 32768)
  (setq gptel-temperature 0.1))

;; documentation

(use-package rfc-mode
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
  :if (file-accessible-directory-p (expand-file-name "~/zz/src/github.com/cellux/rasid"))
  :defer t
  :mode "\\.rasid\\'"
  :load-path (lambda () (expand-file-name "~/zz/src/github.com/cellux/rasid"))
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package floyd-mode
  :if (file-accessible-directory-p (expand-file-name "~/zz/src/github.com/cellux/floyd"))
  :defer t
  :mode "\\.fld\\'"
  :load-path (lambda () (expand-file-name "~/zz/src/github.com/cellux/floyd"))
  :init
  (add-to-list 'exec-path (expand-file-name "~/zz/bin")))

(use-package langsam-mode
  :if (file-accessible-directory-p (expand-file-name "~/projects/langsam"))
  :defer t
  :mode "\\.l\\'"
  :load-path (lambda () (expand-file-name "~/projects/langsam")))

(use-package mixtape
  :if (file-accessible-directory-p (expand-file-name "~/projects/mixtape"))
  :defer t
  :mode ("\\.tape\\'" . mixtape-mode)
  :load-path (lambda () (expand-file-name "~/projects/mixtape")))

;; hydra

(use-package hydra
  :ensure t
  :demand t
  :config
  (defhydra rb--hydra-dired (:exit t)
    "rb directories"
    ("p" (dired (car (prune-directory-list '("~/projects" "~/Projects")))) "projects")
    ("s" (dired (car (prune-directory-list '("~/src" "~/Software/src")))) "src")
    ("o" (dired "~/org") "org")
    ("q" (dired "~/quicklisp/local-projects") "quicklisp/local")
    ("Q" (dired "~/quicklisp/dists/quicklisp/software") "quicklisp/global"))
  (defhydra rb--hydra-emacs-debug nil
    "rb emacs debug settings"
    ("e" toggle-debug-on-error (format "debug-on-error:%s" (if debug-on-error "ON" "OFF")))
    ("q" toggle-debug-on-quit (format "debug-on-quit:%s" (if debug-on-quit "ON" "OFF")))
    ("s" (lambda ()
           (interactive)
           (setq debug-on-signal (not debug-on-signal)))
     (format "debug-on-signal:%s" (if debug-on-signal "ON" "OFF"))))
  (defhydra rb--hydra-emacs (:exit t)
    "rb emacs"
    ("i" iqa-find-user-init-file "init.el")
    ("p" package-list-packages "packages")
    ("d" rb--hydra-emacs-debug/body "debug"))
  (defhydra rb--hydra-top (:exit t)
    "rb hydras"
    ("d" rb--hydra-dired/body "dired")
    ("e" rb--hydra-emacs/body "emacs"))
  :bind ("<f12>" . rb--hydra-top/body))

;; my own packages

;; add init.el directory to load path
(let* ((initialization-file-path (file-truename (or load-file-name buffer-file-name)))
       (initialization-file-dir (file-name-directory initialization-file-path)))
  (add-to-list 'load-path initialization-file-dir))

(use-package rb-bash)
(use-package rb-clojure)
(use-package rb-elisp)
(use-package rb-fd)
(use-package rb-file)
(use-package rb-gptel)
(use-package rb-io)
(use-package rb-object-store)
(use-package rb-rg)
(use-package rb-tools)
(use-package rb-ts)

;; context-specific settings

(let ((context (cond ((file-exists-p (expand-file-name "~/.work")) :work)
                     (:home))))
  (setq gptel-backend (pcase context
                        (:work (gptel-make-gh-copilot "Copilot"))
                        (:home (gptel-make-openai "OpenRouter"
                                 :host "openrouter.ai"
                                 :endpoint "/api/v1/chat/completions"
                                 :stream t
                                 :key (lambda ()
                                        (when-let ((auth-info (auth-source-search :gptel-backend-name "OpenRouter")))
                                          (auth-info-password (car auth-info))))
                                 :models '(anthropic/claude-haiku-4.5
                                           anthropic/claude-opus-4.5
                                           anthropic/claude-sonnet-4.5
                                           deepseek/deepseek-v3.2
                                           google/gemini-2.5-pro
                                           google/gemini-3-flash-preview
                                           google/gemini-3-pro-image-preview
                                           google/gemini-3-pro-preview
                                           meta-llama/llama-3.1-8b-instruct
                                           meta-llama/llama-3.2-11b-vision-instruct
                                           meta-llama/llama-3.2-90b-vision-instruct
                                           meta-llama/llama-3.3-70b-instruct
                                           meta-llama/llama-4-maverick
                                           meta-llama/llama-4-scout
                                           minimax/minimax-m2.1
                                           mistralai/codestral-2508
                                           mistralai/devstral-2512:free
                                           mistralai/devstral-2512
                                           mistralai/devstral-medium
                                           mistralai/devstral-small
                                           mistralai/devstral-small-2505
                                           mistralai/ministral-14b-2512
                                           mistralai/ministral-8b-2512
                                           mistralai/ministral-3b-2512
                                           mistralai/mistral-large-2512
                                           mistralai/mistral-nemo
                                           mistralai/mistral-small-3.2-24b-instruct
                                           mistralai/mixtral-8x22b-instruct
                                           mistralai/mixtral-8x7b-instruct
                                           mistralai/pixtral-12b
                                           mistralai/pixtral-large-2411
                                           mistralai/voxtral-small-24b-2507
                                           moonshotai/kimi-k2-0905
                                           moonshotai/kimi-k2-thinking
                                           moonshotai/kimi-k2.5
                                           openai/gpt-5.1
                                           openai/gpt-5.1-chat
                                           openai/gpt-5.1-codex
                                           openai/gpt-5.1-codex-max
                                           openai/gpt-5.1-codex-mini
                                           openai/gpt-5.2
                                           openai/gpt-5.2-chat
                                           openai/gpt-5.2-codex
                                           openai/gpt-5.2-pro
                                           openai/gpt-oss-120b
                                           openai/gpt-oss-20b
                                           openai/o3
                                           openai/o3-deep-research
                                           openai/o3-mini
                                           openai/o3-mini-high
                                           openai/o3-pro
                                           openai/o4-mini
                                           openai/o4-mini-deep-research
                                           openai/o4-mini-high
                                           openrouter/auto
                                           openrouter/bodybuilder
                                           qwen/qwen3-235b-a22b-2507
                                           qwen/qwen3-235b-a22b-thinking-2507
                                           qwen/qwen3-coder
                                           qwen/qwen3-next-80b-a3b-instruct
                                           qwen/qwen3-next-80b-a3b-thinking
                                           qwen/qwen3-vl-235b-a22b-instruct
                                           qwen/qwen3-vl-235b-a22b-thinking
                                           x-ai/grok-4.1-fast
                                           x-ai/grok-code-fast-1
                                           z-ai/glm-4.6
                                           z-ai/glm-4.7
                                           z-ai/glm-4.7-flash
                                           z-ai/glm-4.6v)))))
  (setq gptel-model (pcase context
                      (:work 'gpt-5.2)
                      (:home 'openai/gpt-5.1-codex-mini))))
