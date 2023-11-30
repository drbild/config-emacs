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
  :hook (typescript-mode)
  :config (setq company-idle-delay 0.2)
          (setq company-minimum-prefix-length 2)
  )

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :bind (("C-x C-f" . counsel-find-file)
         ("C-x C-l" . counsel-find-library)
         ("M-x"     . counsel-M-x))
  )

(use-package counsel-projectile
  :after (counsel projectile)
  :config
  (counsel-projectile-mode 1))

(use-package diminish
  :demand t)

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package eglot
  :commands eglot
  :config (fset #'jsonrpc--log-event #'ignore)
          (setq eglot-events-buffer-size 0)
          (setq eglot-sync-connect nil)
  :custom
  (eglot-autoshutdown t)
  :hook ((typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)))

(use-package elixir-mode
  :mode ("\\.ex"
         "\\.exs"))

(use-package erlang
  :defer t)

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :custom
  (tab-width 4))

(use-package ivy
  :diminish
  :demand t
  :bind (("C-x b" . ivy-switch-buffer)
         ("C-x B" . ivy-switch-buffer-other-window)
         ("M-H" . ivy-resume))
  :config
  (ivy-mode 1))

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
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map)))

(defun my-projectile-project-find-function (dir)
  (let ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(with-eval-after-load 'project
  (add-to-list 'project-find-functions 'my-projectile-project-find-function))

(use-package protobuf-mode
  )

(use-package rego-mode
  )

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
;; Function to insert Patrick-style error tags
;; ##############################################################
(defun insert-error-tag ()
  "Insert an error tag [?????] contain five random uppercase letters."
  (interactive)
  (let ((random-letters ""))
    (dotimes (i 5 random-letters)
      (setq random-letters 
            (concat random-letters 
                    (char-to-string (+ (random 26) ?A)))))
    (insert (concat "[" random-letters "]"))))

(global-set-key (kbd "C-c e") 'insert-error-tag)

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
