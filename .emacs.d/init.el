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

;; don't blink the cursor
(blink-cursor-mode 0)

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

;; lookup auth info in Freedesktop Secret Service
(setq auth-sources '((:source (:secrets default))))

;;; packages

(require 'package)

(add-to-list 'package-archives
  (cons "melpa" "https://melpa.org/packages/") t)

(add-to-list 'package-archives
  (cons "elpa" "https://elpa.gnu.org/packages/") t)

(setq package-selected-packages
  '(benchmark-init
     zenburn-theme
     vertico
     orderless
     marginalia
     which-key
     rainbow-delimiters
     editorconfig
     highlight
     helpful
     realgud
     lispy
     magit
     org-super-agenda
     iqa
     projectile
     rg
     hydra
     vterm
     sly sly-asdf sly-quicklisp
     flycheck
     flycheck-clang-analyzer
     flycheck-clang-tidy
     clojure-mode cider flycheck-clojure clj-refactor
     janet-mode
     geiser geiser-chicken geiser-guile
     extempore-mode
     groovy-mode
     go-mode
     zig-mode
     nim-mode
     python-mode blacken
     lua-mode
     php-mode
     web-mode
     js2-mode npm-mode
     typescript-mode tide
     forth-mode
     rust-mode cargo flycheck-rust
     lsp-mode lsp-ui
     dap-mode
     clang-format+
     cmake-mode
     markdown-mode
     yaml-mode
     toml-mode
     json-mode
     osc
     csound-mode
     sonic-pi
     tidal
     systemd))

;; ensure all packages listed above are installed
(package-install-selected-packages)

;; load and activate all installed packages
;; (package-initialize)

(require 'use-package)

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package zenburn-theme
  :config
  (progn
    (load-theme 'zenburn t)
    (set-frame-font
      (font-spec :name "Droid Sans Mono" :size 15)
      nil t)))

(use-package recentf
  :custom
  (recentf-max-saved-items 50)
  :bind (("C-o" . recentf))
  :config
  (recentf-mode 1))

(use-package vertico
  :config
  (vertico-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package marginalia
  :custom
  (marginalia-align 'right)
  :config
  (marginalia-mode))

(use-package which-key
  :config
  (which-key-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

;; save/restore last cursor position in edited files
(use-package saveplace
  :custom
  (save-place-file (concat user-emacs-directory "places"))
  :config
  (save-place-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)
         ("C-h C-h" . helpful-at-point)))

;; helper for quickly opening this file
(use-package iqa)

(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :custom
  (projectile-switch-project-action #'projectile-dired)
  :config
  (projectile-mode 1)
  (add-to-list 'projectile-globally-ignored-directories "node_modules"))

(use-package rg
  :config
  (rg-enable-default-bindings))

; easily move between windows
(use-package windmove
  :config
  (windmove-default-keybindings))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package gdb-mi
  :defer t
  :custom
  (gdb-many-windows nil)
  (gdb-show-main t))

(use-package realgud
  :defer t)

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-disabled-checkers '(python-pycompile)))

(use-package compile
  :bind (("<f9>" . compile)))

(use-package org
  :defer t
  :config
  (global-set-key (kbd "C-c l") #'org-store-link)
  (global-set-key (kbd "C-c a") #'org-agenda)
  (global-set-key (kbd "C-c c") #'org-capture)
  :custom
  (org-replace-disputed-keys t)
  (org-capture-templates '(("t" "Task" entry (file+headline "tasks.org" "Tasks") nil)
                            ("i" "Idea" entry (file+headline "ideas.org" "Ideas") nil)
                            ("n" "Note" entry (file+headline "notes.org" "Notes") nil)))
  (org-agenda-files '("~/org"))
  (org-refile-targets '((org-agenda-files . (:level . 1)))))

;; programming modes

(use-package lsp-mode
  :defer t
  :commands lsp
  :custom
  (lsp-enable-snippet nil))

(use-package lsp-ui
  :defer t
  :commands lsp-ui-mode)

(use-package dap-mode
  :defer t
  :custom
  (dap-auto-configure-features '(sessions locals controls tooltip)))

(use-package csound-mode
  :defer t
  :mode "\\.csd\\'"
  :custom
  (csound-repl-sr 48000)
  (csound-repl-ksmps 32)
  (csound-repl-nchnls 2)
  (csound-repl-0dbfs 1))

(use-package tidal-mode
  :defer t
  :mode "\\.tidal\\'")

(use-package c-mode
  :defer t
  :mode "\\.c\\'"
  :hook (c-mode . lsp))

(use-package c++-mode
  :defer t
  :mode ("\\.cc\\'" "\\.cpp\\'" "\\.cxx\\'")
  :hook (c++-mode . lsp))

(use-package clang-format+
  :defer t
  :hook (c-mode-common . clang-format+-mode)
  :custom
  (clang-format+-always-enable t)
  (clang-format-style "LLVM"))

(use-package cmake-mode
  :defer t
  :mode ("\\`CMakeLists\\.txt\\'"))

(use-package nim-mode
  :defer t
  :mode ("\\.nim\\'"
         "\\.nims\\'"
         "\\.nimble\\'"
         "\\`nim.cfg\\'"))

(use-package forth-mode
  :defer t
  :mode ("\\.f\\'" "\\.fth\\'" "\\.fs\\'" "\\.b\\'"))

(use-package janet-mode
  :defer t
  :mode ("\\.janet\\'"))

(use-package extempore-mode
  :defer t
  :mode "\\.xtm\\'"
  :init
  (setq user-extempore-directory "/usr/bin/"))

(use-package scheme-mode
  :defer t
  :mode ("\\.scm\\'" . scheme-mode))

(use-package geiser
  :defer t
  :hook (scheme-mode . geiser-mode)
  :init
  (setq geiser-chicken-binary "chicken-csi")
  (setq geiser-active-implementations '(chicken guile)))

(use-package clojure-mode
  :defer t
  :mode ("\.clj\\'" . clojure-mode))

(use-package cider
  :defer t
  :config
  (setq cider-repl-display-help-banner nil)
  (setq nrepl-sync-request-timeout 600))

(use-package lua-mode
  :defer t
  :mode "\\.lua\\'"
  :interpreter "lua")

(use-package toml-mode
  :defer t
  :mode "\\.toml\\'")

(use-package yaml-mode
  :defer t
  :mode "\\.ya?ml\\'")

(use-package rust-mode
  :defer t
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp))

(use-package cargo
  :defer t
  :hook (rust-mode . cargo-minor-mode))

(use-package flycheck-rust
  :defer t
  :hook (flycheck-mode . flycheck-rust-setup))

(use-package go-mode
  :defer t
  :mode "\\.go\\'"
  :init (let ((go-bin-path (expand-file-name "~/go/bin")))
          (unless (member go-bin-path exec-path)
            (setq exec-path (append exec-path (list go-bin-path)))))
  :hook ((go-mode . lsp)
         (before-save . gofmt-before-save)))

(use-package dap-dlv-go
  :defer t)

(use-package zig-mode
  :defer t
  :mode "\\.zig\\'"
  :hook (zig-mode . lsp))

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

(use-package php-mode
  :defer t
  :mode "\\.php[345]?\\'")

(use-package python-mode
  :defer t
  :mode "\\.py\\'"
  :interpreter "python"
  :custom (python-indent-offset 4))

(use-package blacken
  :defer t
  :hook (python-mode . blacken-mode))

(use-package lisp-mode
  :defer t
  :bind (:map lisp-mode-map
          ("C-c C-z" . rb-mrepl))
  :custom (lisp-indent-offset 2))

(defun rb-mrepl ()
  (interactive)
  (pcase major-mode
    ('clojure-mode
     (command-execute (if (cider-sessions)
                          'cider-switch-to-repl-buffer
                        'cider)))
    ('lisp-mode
     (command-execute (if (sly-current-connection)
                          'sly-mrepl
                        'sly)))
    ('sly-mrepl-mode
     (command-execute 'sly-switch-to-most-recent))
    (_ (message "First switch to a Lisp or Clojure buffer." ))))

(global-set-key (kbd "C-c C-z") 'rb-mrepl)

(defun rb-sly-quit-lisp ()
  (interactive)
  (if (sly-current-connection)
      (command-execute 'sly-quit-lisp)
    (message "No current connection.")))

(use-package sly
  :defer t
  :commands sly
  :config
  (add-hook 'sly-mrepl-mode-hook
            (lambda ()
              (define-key sly-mode-map (kbd "C-c C-z") 'rb-mrepl)))
  :custom
  (inferior-lisp-program
   (seq-find (lambda (binary-name)
               (locate-file binary-name exec-path nil 'executable))
             '("sbcl-rb" "sbcl")))
  :bind
  (:map sly-mode-map
        ("C-c C-q" . rb-sly-quit-lisp)))

(use-package sly-asdf
  :defer t
  :after sly)

(use-package sly-quicklisp
  :defer t
  :after sly)

(use-package lispy-mode
  :hook (emacs-lisp-mode
         lisp-mode
         clojure-mode
         scheme-mode
         extempore-mode
         janet-mode))

(use-package llvm-mode
  ;; did not find this package on MELPA
  :disabled t
  :defer t
  :mode ("\\.ir\\'" "\\.ll\\'"))

(use-package css-mode
  :defer t
  :mode "\\.css\\'"
  :custom (css-indent-offset 2))

(use-package web-mode
  :defer t
  :mode ("\\.[jt]sx\\'")
  :commands web-mode)

(use-package js2-mode
  :defer t
  :mode "\\.js\\'"
  :custom
  (js-indent-level 2)
  (js2-bounce-indent-p nil))

(use-package json-mode
  :defer t
  :mode "\\.json\\'")

(use-package tide
  :defer t
  :after (typescript-mode flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup))))

(use-package sonic-pi
  :if (file-accessible-directory-p "/usr/lib/sonic-pi")
  :defer t
  :init
  (setq sonic-pi-path "/usr/lib/sonic-pi/"))

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
