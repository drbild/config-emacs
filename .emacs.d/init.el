(setq package-enable-at-startup nil)

;; ##############################################################
;; Functions
;; ##############################################################

(eval-and-compile
  (define-inline emacs-path (path)
    (expand-file-name path user-emacs-directory))

  )

;; ##############################################################
;; Environment
;; ##############################################################
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-enable-at-startup nil)

(setq exec-path (append exec-path '("/home/drbild/.asdf/shims")))

(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-refresh-contents)
  (package-install 'diminish))

(unless (package-installed-p 'bind-key)
  (package-refresh-contents)
  (package-install 'bind-key))

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

;; ##############################################################
;; Settings
;; ##############################################################

(setq custom-file (emacs-path "settings.el"))
(load custom-file)

;; ##############################################################
;; Packages
;; ##############################################################
(use-package ace-window
  :bind (("M-o" . 'ace-window)))

(use-package auto-package-update
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 7)
  (auto-package-update-maybe))

(use-package avy
  :pin melpa-stable
  :bind ("C-c j" . avy-goto-char))

(use-package cc-mode
  :mode ("\\.inl\\'" . c++-mode))

(use-package cmake-font-lock
  :hook (cmake-mode . cmake-font-lock-activate))

(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'"))

(use-package company
  :defer 5
  :diminish
  :hook (typescript-mode))

(use-package diminish
  :demand t)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package eglot
  :defer t
  :hook (typescript-mode . eglot-ensure))

(use-package elixir-mode
  :mode ("\\.ex"
         "\\.exs"))

(use-package erlang
  :defer t)

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :custom
  (tab-width 4))

(use-package kconfig-mode
  :mode ("KConfig"
         "Config.in")
  :custom
  (tab-width 4))

(use-package ledger-mode
  :mode ("\\.ledger\\'"))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package nginx-mode
  :defer t)

(use-package prettier
  :config
  (add-hook 'after-init-hook #'global-prettier-mode))

(use-package projectile
  :diminish
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package treemacs
  :bind (("M-0" . 'treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package tree-sitter
  :defer t
  :config
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package typescript-mode
  ;;:after tree-sitter ;; reenable when tsc adds support for arm64 on Mac
  :config
  (define-derived-mode typescript-tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . typescript-tsx-mode)))
  ;;(add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; ##############################################################
;; Store backups and autosaves in central location
;; ##############################################################
;; Save all backups to ~/.emacs.d/backup
(defvar backup-dir "~/.emacs.d/backup/")
(setq backup-directory-alist `(("." . , backup-dir)))

;; Version backups
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Place all autosave files in ~/.emacs.d/autosave
(defvar autosave-dir "~/.emacs.d/autosave/")
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))
