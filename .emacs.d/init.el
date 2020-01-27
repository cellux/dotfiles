(setq custom-file
  (expand-file-name "settings.el" user-emacs-directory))

(load-file custom-file)

;; enable all commands
(setq disabled-command-function nil)

;; default to utf-8
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

;; show matching parentheses
(show-paren-mode 1)

;; highlight current line
(hl-line-mode t)

;; don't blink the cursor
(blink-cursor-mode 0)

;; no toolbar
(tool-bar-mode 0)

;; disable electric indent
(electric-indent-mode 0)

;; turn on syntax highlighting
(global-font-lock-mode)

;; automatically revert unchanged buffers
;; when the underlying file is changed on disk
(global-auto-revert-mode t)

;; remember recently opened files
(recentf-mode 1)

;; use the mouse wheel
(mouse-wheel-mode t)

;; show both line and column numbers in the mode line
(line-number-mode 1)
(column-number-mode 1)

;; show name of current buffer in frame title
(setq frame-title-format "%f")

;;; packages

(require 'package)

(add-to-list 'package-archives
  (cons "melpa" "https://melpa.org/packages/") t)

(add-to-list 'package-archives
  (cons "elpa" "https://elpa.gnu.org/packages/") t)

(package-initialize)

(require 'use-package)

(use-package zenburn-theme
  :config
  (load-theme 'zenburn t))

(use-package ace-jump-mode
  :bind (("C-c SPC" . ace-jump-mode)))

(use-package ace-mc
  :bind (("C-(" . ace-mc-add-multiple-cursors)
         ("C-M-(" . ace-mc-add-multiple-cursors)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; save/restore last cursor position in edited files
(use-package saveplace
  :config
  (save-place-mode 1))

(use-package helpful
  :bind (("C-h f" . helpful-function)
         ("C-h v" . helpful-variable)))

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

(use-package realgud
  :defer t)

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config (setq company-tooltip-align-annotations t)
          (setq company-minimum-prefix-length 1))

(use-package lsp-mode
  :commands lsp
  :config (require 'lsp-clients))

(use-package lsp-ui)

(use-package c-mode
  :mode "\\.c\\'"
  :hook (c-mode . lsp))

(use-package c++-mode
  :mode ("\\.cc\\'" "\\.cpp\\'" "\\.cxx\\'")
  :hook (c++-mode . lsp))

(use-package forth-mode
  :mode ("\\.f\\'" "\\.fth\\'" "\\.fs\\'" "\\.g\\'"))

(use-package extempore-mode
  :defer t
  :mode "\\.xtm\\'"
  :init
  (setq user-extempore-directory "/usr/bin/"))

(use-package geiser
  :mode ("\\.scm\\'" . geiser-mode))

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
  :hook (python-mode . lsp))

(use-package llvm-mode
  ; did not find this package on MELPA
  :disabled t
  :mode ("\\.ir\\'" "\\.ll\\'"))

(use-package css-mode
  :mode "\\.css\\'")

(use-package web-mode
  :commands web-mode)

(use-package js2-mode
  :mode "\\.js\\'")

(use-package json-mode
  :mode "\\.json\\'")

(use-package sonic-pi
  :defer t)

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
