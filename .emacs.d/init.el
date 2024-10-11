(setq package-enable-at-startup nil)

;; ##############################################################
;; Functions
;; ##############################################################

(eval-and-compile
  (define-inline emacs-path (path)
    (expand-file-name path user-emacs-directory)))

;; ##############################################################
;; Environment
;; ##############################################################
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("melpa" . "https://melpa.org/packages/"))
      package-enable-at-startup nil)

(setq exec-path (append exec-path '("/opt/homebrew/bin")))
(setenv "PATH" (format "%s:%s" "/opt/homebrew/bin" (getenv "PATH")))

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

;; Remove after Emacs 30 is released
(unless (package-installed-p 'vc-use-package)
  (package-refresh-contents)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(eval-when-compile
  (setq use-package-always-ensure t)
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key)
  (require 'vc-use-package))

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

(use-package add-node-modules-path
  :hook
  (typescript-mode . add-node-modules-path)
  (typescript-ts-mode . add-node-modules-path)
  (js-mode . add-node-modules-path)
  (js-ts-mode .add-node-modules-path)
  :custom
  (add-node-modules-path-command '("echo \"$(npx npm root)/.bin\"")))

(use-package asdf
  :vc (asdf :url "https://github.com/tabfugnic/asdf.el"
            :branch "main")
  :config (asdf-enable))

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
  :hook (typescript-mode go-mode)
  :config (setq company-idle-delay 0.2)
          (setq company-minimum-prefix-length 2))

(use-package company-quickhelp)

(use-package company-quickhelp-terminal
  :if (not window-system)
  :custom
  (company-quickhelp-use-propertized-text nil)
  :config
  (with-eval-after-load 'company-quickhelp
    (company-quickhelp-terminal-mode 1)))

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

(use-package dired
  :ensure nil
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package editorconfig
  :diminish editorconfig-mode
  :config
  (editorconfig-mode 1))

(use-package eglot
  :commands eglot
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l r" . eglot-rename))
  :preface
  (defun my/project-find-go-module (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (if (and (bound-and-true-p eglot-lsp-context) (eq major-mode "go-mode"))
          (cons 'go-module root))))
  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))
  :config
  ;; performance optimizations
  (fset #'jsonrpc--log-event #'ignore)
  (setq eglot-sync-connect nil)
  (plist-put eglot-events-buffer-config :size 0)
  ;; Typescript + React support
  (add-to-list 'eglot-server-programs '((tsx-mode :language-id "typescriptreact") . ("typescript-language-server" "--stdio")))
  ;; Golang support
  ;;(add-hook 'project-find-functions #'my/project-find-go-module)
  :custom
  (eglot-autoshutdown t)
  :hook ((typescript-mode . eglot-ensure)
         (go-mode . eglot-ensure)))

(use-package eldoc
  :ensure nil
  :diminish)

(use-package elixir-mode
  :mode ("\\.ex"
         "\\.exs"))

(use-package erlang
  :defer t)

(use-package flymake-eslint
  :hook
  (eglot-managed-mode
   . (lambda ()
       (when (derived-mode-p 'typescript-mode 'js-mode)
         (when-let* ((root (locate-dominating-file (buffer-file-name) "package.json"))
                     (rc (locate-file ".eslintrc" (list root) '(".js" ".json"))))
           (flymake-eslint-enable))))))

(use-package go-mode
  :hook (before-save . gofmt-before-save)
  :custom
  (tab-width 4))

(use-package json-mode
  :mode (("\\.json\\'" . json-mode)))

(use-package ivy
  :diminish
  :demand t
  :bind (("M-H" . ivy-resume))
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

(use-package perspective
  :bind(("C-x C-b" . persp-list-buffers)
        ("C-x b" . persp-counsel-switch-buffer)
        ("C-x x b" . persp-switch-to-buffer))
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode))

(use-package persp-projectile
  :after projectile)

(use-package popper
  :bind (("C-'" . popper-toggle)
         ("M-'" . popper-cycle))
  :init
  (setq popper-reference-buffers
        '(("\\*Gofmt Errors\\*" . hide)
          "\\*Messages\\*"
          "Output\\*$"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package prettier
  :diminish
  :config
  (add-hook 'after-init-hook #'global-prettier-mode))

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
         ("s-p" . projectile-command-map)
         ("C-c p" . projectile-command-map))
  :init
  (projectile-mode +1)
  :preface
  (defun my/projectile-switch-project-action ()
    "Open the Treemacs window and find a file in the project."
    (projectile-dired)
    (treemacs))
  :config
  (setq projectile-switch-project-action #'my/projectile-switch-project-action))

(use-package protobuf-mode
  )

(use-package rego-mode
  )

(use-package swiper
  :bind (("C-s" . swiper-isearch)
         ("C-r" . swiper-isearch-backwards)))

(use-package treemacs
  :bind (("M-0" . 'treemacs-select-window))
  :init(treemacs-project-follow-mode +1))

(use-package treemacs-projectile
  :after (treemacs projectile))

(use-package treemacs-perspective
  :after (treemacs perspective)
  :config (treemacs-set-scope-type 'Perspectives))

;; (use-package tree-sitter
;;  :config
;;  (global-tree-sitter-mode)
;;  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; (use-package tree-sitter-langs
;;   :after tree-sitter)

;; (use-package typescript-ts-mode
;;   :mode (("\\.ts\\'" . typescript-ts-mode)
;;          ("\\.tsx\\'" . tsx-ts-mode)))

(use-package typescript-mode
  :config
  (define-derived-mode tsx-mode typescript-mode "TSX")
  (add-to-list 'auto-mode-alist `(,(rx ".tsx" eos) . tsx-mode)))

(use-package vterm
  :preface
  (defun my/open-vterm-in-project-root ()
    "Toggle a vterm in the root of the current Projectile project.
If the vterm is visible in the current window, hide
it. Otherwise, switch to it or create a new one."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (buffer-name (format "*vterm: %s*" (projectile-project-name)))
         (current-buffer (current-buffer)))
    (if project-root
        (if (and (get-buffer buffer-name)
                 (eq (get-buffer buffer-name) current-buffer))
            ;; If the vterm buffer is current, bury it (hide it)
            (bury-buffer)
          ;; Switch to or create the vterm buffer
          (progn
            (if (get-buffer buffer-name)
                (switch-to-buffer buffer-name)
              (progn
                (vterm buffer-name)
                (set-process-query-on-exit-flag (get-buffer-process buffer-name) nil)
                (vterm-send-string (format "cd %s" project-root))
                (vterm-send-return)
                (vterm-clear)))))
      (message "Not in a Projectile project."))))
  :bind("C-c v" . 'my/open-vterm-in-project-root)
  :config
  (setq vterm-timer-delay nil))

(use-package whitespace
  :diminish (global-whitespace-mode
             whitespace-mode
             whitespace-newline-mode))

(use-package yaml-mode
  :mode ("\\.yml\\'" "\\.yaml\\'"))

(use-package yasnippet
  :diminish 'yas-minor-mode
  :config (yas-global-mode 1))

(use-package yasnippet-snippets)

;; ##############################################################
;; Function to insert Patrick-style error tags
;; ##############################################################
(defun my/insert-error-tag ()
  "Insert an error tag [?????] contain five random uppercase letters."
  (interactive)
  (let ((random-letters ""))
    (dotimes (i 5 random-letters)
      (setq random-letters
            (concat random-letters
                    (char-to-string (+ (random 26) ?A)))))
    (insert (concat "[" random-letters "]"))))

(global-set-key (kbd "C-c e") 'my/insert-error-tag)

;; ##############################################################
;; Hide VCS info from mode line
;; ##############################################################
(setq-default mode-line-format (delete '(vc-mode vc-mode) mode-line-format))

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
