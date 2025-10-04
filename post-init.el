;;; pacakge -- pre-init.el
;;; commentary:
;;; summary:
;;; code:


;;; ------------- Native Compilation -----------------
;; Native compilation enhances Emacs performance by converting Elisp code into
;; native machine code, resulting in faster execution and improved
;; responsiveness.
;;
;; Ensure adding the following compile-angel code at the very beginning
;; of your `~/.emacs.d/post-init.el` file, before all other packages.
(use-package compile-angel
  :demand t
  :ensure t
  :custom
  ;; Set `compile-angel-verbose` to nil to suppress output from compile-angel.
  ;; Drawback: The minibuffer will not display compile-angel's actions.
  (compile-angel-verbose t)

  :config
  ;; The following directive prevents compile-angel from compiling your init
  ;; files. If you choose to remove this push to `compile-angel-excluded-files'
  ;; and compile your pre/post-init files, ensure you understand the
  ;; implications and thoroughly test your code. For example, if you're using
  ;; the `use-package' macro, you'll need to explicitly add:
  ;; (eval-when-compile (require 'use-package))
  ;; at the top of your init file.
  (push "/init.el" compile-angel-excluded-files)
  (push "/early-init.el" compile-angel-excluded-files)
  (push "/pre-init.el" compile-angel-excluded-files)
  (push "/post-init.el" compile-angel-excluded-files)
  (push "/pre-early-init.el" compile-angel-excluded-files)
  (push "/post-early-init.el" compile-angel-excluded-files)

  ;; A local mode that compiles .el files whenever the user saves them.
  ;; (add-hook 'emacs-lisp-mode-hook #'compile-angel-on-save-local-mode)

  ;; A global mode that compiles .el files prior to loading them via `load' or
  ;; `require'. Additionally, it compiles all packages that were loaded before
  ;; the mode `compile-angel-on-load-mode' was activated.
  (compile-angel-on-load-mode 1))

;;; ------------- Native Compilation -----------------


;;; ------------- flame -----------------
(setq initial-frame-alist
      (append (list
	'(width . 180)
        '(height . 60)
        )
	      initial-frame-alist))
(setq default-frame-alist initial-frame-alist)
;;; ------------- flame -----------------



;;; ------------- font -------------------
;; 英語フォント
(defvar my/font-eng "Ricty Diminished")
;; 日本語フォント
(defvar my/font-jp "Noto Sans CJK JP")

(if (string-match "issei-All-Series" (system-name))
    (progn
      (message "linux settings")
      (set-face-attribute 'default nil
			  :family "Ricty Diminished"
			  :height 110)
      (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Ricty Diminished" :size 14))
      )
  )

(if (string-match "ac211.local" (system-name))
    (progn
          (message "ac211.local settings")
    (set-face-attribute 'default nil
			:family "Ricty Diminished"
			:height 140)
  (set-fontset-font
   nil 'japanese-jisx0208
   (font-spec :family "Hiragino Kaku Gothic ProN" :size 10))
  ;; 英語と日本語の比率を1：2に設定
  (add-to-list 'face-font-rescale-alist
	       '(".*Hiragino Kaku Gothic ProN.*" . 1.2))
      )
  )

(if (string-match "DESKTOP-QFI57MO" (system-name))
    (progn
      (message "wsl settings")
      (set-face-attribute 'default nil
			  :family "Ricty Diminished"
			  :height 110)
      (set-fontset-font t 'japanese-jisx0208 (font-spec :family my/font-jp :size 14))
      (set-fontset-font t 'japanese-jisx0212 (font-spec :family my/font-jp :size 14))
      )
  )
(setq-default line-spacing 0.1) ;; 行間を指定
;;; ------------- font -------------------





;;; ------------- theme -------------------
(use-package ef-themes
  :ensure t
  :config
  (setq ef-themes-mixed-fonts t
        ef-themes-variable-pitch-ui t)
  (load-theme 'ef-melissa-light t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package nerd-icons)

(use-package nerd-icons-dired
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after marginalia
  :config
  (nerd-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

;; 画面の余白
(use-package spacious-padding
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))

  (spacious-padding-mode +1))
;;; ------------- theme -------------------





;;; ------------- tab --------------------
(use-package centaur-tabs
  :ensure t
  :init
  (centaur-tabs-mode t) ;; グローバルにCentaur Tabsを有効にする
  :config
  (centaur-tabs-mode t)
  (defun centaur-tabs-hide-tab (x)
  "Do no to show buffer X in tabs."
  (let ((name (format "%s" x)))
    (or
     ;; Current window is not dedicated window.
     (window-dedicated-p (selected-window))

     ;; Buffer name not match below blacklist.
     (string-prefix-p "*Flycheck" name)
     (string-prefix-p "*Flymake log*" name)
     (string-prefix-p "*Warnings*" name)
     (string-prefix-p "*Messages*" name)
     (string-prefix-p "*lsp" name)
     (string-prefix-p "*pylsp*" name)
     (string-prefix-p "*pylsp::stderr*" name)

     ;; Is not magit buffer.
     (and (string-prefix-p "magit" name)
	  (not (file-name-extension name)))
     )))
  :custom
  ;; (centaur-tabs-style "wave")

  ;; icons
  (centaur-tabs-set-icons t)
  (centaur-tabs-plain-icons t)

  ;; To display an underline over the selected tab:
  (centaur-tabs-set-bar 'under)
  (x-underline-at-descent-line t)

  (centaur-tabs-set-close-button nil)

  ;; Customize the modified marker
  (centaur-tabs-set-modified-marker t)
  (centaur-tabs-modified-marker "*")
  :bind
  ("M-[" . centaur-tabs-backward)
  ("M-]" . centaur-tabs-forward)
  )
;;; ------------- tab --------------------




;;; ------------- dashboard ---------------
(use-package dashboard
  :init
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-display-icons-p t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content t)
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '(
			  (recents   . 5)
			  (projects   . 5)
			  (agenda    . 5)
			  (bookmarks . 5)
			  ;;(error-status . nil)
			  ))
  (setq dashboard-heading-icons '((recents   . "nf-oct-history")
				  (projects  . "nf-oct-rocket")
				  (agenda    . "nf-oct-calendar")
                                  (bookmarks . "nf-oct-bookmark")
                                  (registers . "nf-oct-database")
				  (error-status . "nf-oct-bug")
				  ))
  )
;;; ------------- dashboard ---------------



;;; ------------- others ------------------
;; 対応する括弧を光らせる。
(show-paren-mode 1)
(setq blink-matching-paren nil)

;; 長い行を含むファイルの最適化
(use-package so-long
  :init
  (global-so-long-mode +1))

;; かっこの自動挿入
(electric-pair-mode 1)

;;C-nを押し続けてもページが切り替わることなく一行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;; 行番号表示
(global-display-line-numbers-mode 1) ;; グローバル
;; 絶対行番号（デフォルト）
(setq display-line-numbers-type t)

;; camelCase単位で移動する
(use-package subword
  :init
  (global-subword-mode +1))


;; カーソルの移動を視覚的に分かりやすくしてくれます。beaconよりもシンプルな実装になっています。
(use-package pulsar
  :config
  (pulsar-global-mode +1)
  ;; (pulsar-pulse t)
  )

;; フォントキャッシュの圧縮を抑制（多フォント環境の引っかかり軽減）
(setq inhibit-compacting-font-caches t)

;; 高速で不正確なスクロール
(setq fast-but-imprecise-scrolling t)

;; 字句ハイライト遅延（超巨大バッファで効く）
(setq jit-lock-defer-time 0.05)

;; emacsclient コマンドで高速にファイルが開けます。
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;; パフォーマンスの向上
(setq process-adaptive-read-buffering t)

;; 閉じ括弧を入力しても点滅させない
(setq blink-matching-paren nil)

;; vcのバックエンドをGitのみに変更
(setq vc-handled-backends '(Git))

;; ファイル検索を2回行わないようにする
(setq auto-mode-case-fold nil)

;; 双方向の並び替えを抑制する
(setq-default bidi-display-reordering 'left-to-right)

;; 長い行の双方向スキャン
(setq bidi-inhibit-bpa t)

;; フォーカスされていないウィンドウのカーソルを削除
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; ドメインにpingを送信しない
(setq ffap-machine-p-known 'reject)

;; paste時、regionを削除してpasteする
(delete-selection-mode 1)
;;; ------------- others ------------------



;;; ------------- recentf -----------------
;; Auto-revert in Emacs is a feature that automatically updates the
;; contents of a buffer to reflect changes made to the underlying file
;; on disk.
(use-package autorevert
  :ensure nil
  :commands (auto-revert-mode global-auto-revert-mode)
  :hook
  (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3)
  (auto-revert-remote-files nil)
  (auto-revert-use-notify t)
  (auto-revert-avoid-polling nil)
  (auto-revert-verbose t))


;; Recentf is an Emacs package that maintains a list of recently
;; accessed files, making it easier to reopen files you have worked on
;; recently.
(use-package recentf
  :ensure nil
  :commands (recentf-mode recentf-cleanup)
  :hook
  (after-init . recentf-mode)

  :custom
  (recentf-auto-cleanup (if (daemonp) 300 'never))
  (recentf-exclude
   (list "\\.tar$" "\\.tbz2$" "\\.tbz$" "\\.tgz$" "\\.bz2$"
         "\\.bz$" "\\.gz$" "\\.gzip$" "\\.xz$" "\\.zip$"
         "\\.7z$" "\\.rar$"
         "COMMIT_EDITMSG\\'"
         "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
         "-autoloads\\.el$" "autoload\\.el$" ".recentf" "^/ssh:"))

  :config
  ;; A cleanup depth of -90 ensures that `recentf-cleanup' runs before
  ;; `recentf-save-list', allowing stale entries to be removed before the list
  ;; is saved by `recentf-save-list', which is automatically added to
  ;; `kill-emacs-hook' by `recentf-mode'.
  (add-hook 'kill-emacs-hook #'recentf-cleanup -90)
  (global-set-key "\C-x\ \C-r" 'recentf-open-files)
  )

;; savehist is an Emacs feature that preserves the minibuffer history between
;; sessions. It saves the history of inputs in the minibuffer, such as commands,
;; search strings, and other prompts, to a file. This allows users to retain
;; their minibuffer history across Emacs restarts.
(use-package savehist
  :ensure nil
  :commands (savehist-mode savehist-save)
  :hook
  (after-init . savehist-mode)
  :custom
  (savehist-autosave-interval 600)
  (savehist-additional-variables
   '(kill-ring                        ; clipboard
     register-alist                   ; macros
     mark-ring global-mark-ring       ; marks
     search-ring regexp-search-ring)))

;; save-place-mode enables Emacs to remember the last location within a file
;; upon reopening. This feature is particularly beneficial for resuming work at
;; the precise point where you previously left off.
(use-package saveplace
  :ensure nil
  :commands (save-place-mode save-place-local-mode)
  :hook
  (after-init . save-place-mode)
  :custom
  (save-place-limit 400))

;; Enable `auto-save-mode' to prevent data loss. Use `recover-file' or
;; `recover-session' to restore unsaved changes.
(setq auto-save-default t)

(setq auto-save-interval 300)
(setq auto-save-timeout 30)
;;; ------------- recentf -----------------






;;; ------------- corfu -----------------
;; Corfu enhances in-buffer completion by displaying a compact popup with
;; current candidates, positioned either below or above the point. Candidates
;; can be selected by navigating up or down.
(use-package corfu
  :ensure t
  :commands (corfu-mode global-corfu-mode)

  :hook ((prog-mode . corfu-mode)
         (shell-mode . corfu-mode)
         (eshell-mode . corfu-mode))

  :custom
  ;; Hide commands in M-x which do not apply to the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  ;; Disable Ispell completion function. As an alternative try `cape-dict'.
  (text-mode-ispell-word-completion nil)
  (tab-always-indent 'complete)
  (corfu-auto t)

  ;; popup-mode
  (corfu-popupinfo-mode t)
  (corfu-popupinfo-delay 0.5)
  (corfu-popupinfo-at-point t)

  ;; corfu-echo
  (corfu-echo-delay 0.05)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-preselect-first nil)   ; 無駄な再描画回数を減らす好み設定

  ;; corfuの設定
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  (corfu-auto-delay 0.12) ; Auto-completion delay
  ;; (corfu-auto-completion-delay 0.1) ; Auto-completion delay
  (corfu-quit-at-boundary t) ; Quit completion at word boundary
  (corfu-separator ?\s) ; Separator for candidates
  (corfu-popupinfo-delay 0.5) ; Delay for popup info
  (corfu-scroll-margin 3) ; Scroll margin
  (corfu-min-width 10) ; Minimum width of completion window
  (corfu-max-height 15) ; Maximum height of completion window

  ;; Enable Corfu
  :config
  (global-corfu-mode))

;; Cape, or Completion At Point Extensions, extends the capabilities of
;; in-buffer completion. It integrates with Corfu or the default completion UI,
;; by providing additional backends through completion-at-point-functions.
(use-package cape
  :ensure t
  :commands (cape-dabbrev cape-file cape-elisp-block)
  :bind ("C-c p" . cape-prefix-map)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  :config
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
  (advice-add 'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

  (add-hook 'completion-at-point-functions #'tempel-complete)
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  )
;;; ------------- corfu -----------------




;;; ------------- Vertico、Consult、Embark -----------------
;; Vertico provides a vertical completion interface, making it easier to
;; navigate and select from completion candidates (e.g., when `M-x` is pressed).
(use-package vertico
  ;; (Note: It is recommended to also enable the savehist package.)
  :ensure t
  :config
  (vertico-mode))

;; Vertico leverages Orderless' flexible matching capabilities, allowing users
;; to input multiple patterns separated by spaces, which Orderless then
;; matches in any order against the candidates.
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; Marginalia allows Embark to offer you preconfigured actions in more contexts.
;; In addition to that, Marginalia also enhances Vertico by adding rich
;; annotations to the completion candidates displayed in Vertico's interface.
(use-package marginalia
  :ensure t
  :commands (marginalia-mode marginalia-cycle)
  :hook (after-init . marginalia-mode))

;; Embark integrates with Consult and Vertico to provide context-sensitive
;; actions and quick access to commands based on the current selection, further
;; improving user efficiency and workflow within Emacs. Together, they create a
;; cohesive and powerful environment for managing completions and interactions.
(use-package embark
  ;; Embark is an Emacs package that acts like a context menu, allowing
  ;; users to perform context-sensitive actions on selected items
  ;; directly from the completion interface.
  :ensure t
  :commands (embark-act
             embark-dwim
             embark-export
             embark-collect
             embark-bindings
             embark-prefix-help-command)
  :bind
  (("M-." . embark-act)         ;; pick some comfortable binding
   ("C-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :init
  :after (vertico) ; Load after vertico
  :bind
  ("C-s" . consult-line)  ;; バッファ内をキーワードで検索
  ("C-x b" . consult-buffer)
  ("C-x 4 b" . consult-buffer-other-window)
  ;; ("C-r" . consult-ripgrep) ;; ripgrep がインストールされていれば
  ;; ("C-g C-g" . consult-grep) ;; デフォルトの grep コマンドに consult を適用
  ("M-y" . consult-yank-pop) ;; kill-ring の履歴から選択
  )

;; bufferへの表示をいい感じにしてくれるらしい
(use-package beframe
  :ensure t
  :config
  (defvar consult-buffer-sources)
  (declare-function consult--buffer-state "consult")

  (with-eval-after-load 'consult
    (defface beframe-buffer
      '((t :inherit font-lock-string-face))
      "Face for `consult' framed buffers.")

    (defvar beframe-consult-source
      `( :name     "Frame-specific buffers (current frame)"
         :narrow   ?F
         :category buffer
         :face     beframe-buffer
         :history  beframe-history
         :items    ,#'beframe-buffer-names
         :action   ,#'switch-to-buffer
         :state    ,#'consult--buffer-state))

    (add-to-list 'consult-buffer-sources 'beframe-consult-source))

  (beframe-mode +1)
  )
;; ミニバッファを大きくする
(setq resize-mini-windows t)
(setq mini-window-hscroll t)
(setq mini-window-max-height 0.4)

;; 補完候補の表示数を増やす
(setq completion-cycle-threshold nil)
(setq completion-try-completion nil)
(setq completion-auto-help t)

;; isearch のインクリメンタルサーチをより強力に
(setq search-whitespace-regexp ".*?")
;;; ------------- Vertico、Consult、Embark -----------------



;;; ------------- eglot -----------------
;; Set up the Language Server Protocol (LSP) servers using Eglot.
(use-package eglot
  :init
  (setq eglot-send-changes-idle-time 1.0)
  (setq eglot-extend-to-xref t)
  (setq eglot-events-buffer-size 0) ;; Eglotのログ/イベントバッファは基本オフ
  (setq eglot-report-progress nil) ;; Eglotのログ/イベントバッファは基本オフ
  (setq read-process-output-max (* 3 1024 1024)) ;; プロセス読み取りを広げてスループットUP
  :bind ( :map eglot-mode-map
          ("C-c r" . eglot-rename)
          ("C-c o" . eglot-code-action-organize-imports)
          ("C-c a" . eglot-code-actions)
          ("C-c h" . eldoc)
          ("<f6>" . xref-find-definitions))
  :commands (eglot-ensure
             eglot-rename
             eglot-format-buffer)
  :config
  (with-eval-after-load 'flymake
    (setq flymake-no-changes-timeout 0.5
          flymake-start-on-save-buffer t
          flymake-start-on-flymake-mode t
          flymake-start-on-newline nil))
  ;; language serverを追加する場合はここに追加していく
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp" "--verbose"))) ;;python用
  ;; (add-to-list 'eglot-server-programs '(python-mode . ("pylsp" "-v"))) ;;python用
  (add-to-list 'eglot-server-programs
               '(tsx-ts-mode . ("typescript-language-server" "--stdio" "--log-level" "4"))) ;; tsx-ts-mode
  (add-to-list 'eglot-server-programs
               `(elixir-mode . (,(expand-file-name
                                  (concat user-emacs-directory
                                          ".cache/lsp/elixir-ls-v0.28.0/language_server.sh"))))) ;; elixir
  )
;; consultとeglotを統合するパッケージです。シンボルの検索が行えるようになります。
(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("C-c s" . consult-eglot-symbols)))


;; eglotの拡張
(use-package eglot-x
  :straight (eglot-x :type git :host nil :repo "https://github.com/nemethf/eglot-x.git")
  :after eglot
  :config
  (eglot-x-setup))

;; ミニバッファのeldocをposframeで表示してくれます。
(use-package eldoc-box
  :after eglot
  ;; :init
  ;; :hook
  ;; (eglot-managed-mode-hook . eldoc-box-hover-at-point-mode) ;;Display the documentation of the symbol at point in a temporary childframe
  :config
  (set-face-attribute 'eldoc-box-border nil :background "white")
  ;; (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-mode t)
  (add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)
  )

;; eldocの情報を追加します。
(use-package eglot-signature-eldoc-talkative
  :after eglot
  :config
  (advice-add #'eglot-signature-eldoc-function
              :override #'eglot-signature-eldoc-talkative))


;; emacs-lsp-booster ;; M-x eglot-booster
(use-package eglot-booster
	:straight ( eglot-booster :type git :host nil :repo "https://github.com/jdtsmith/eglot-booster")
	:after eglot
	:config (eglot-booster-mode))

;; emacsの組み込み関数を利用してシンボルをハイライトしてくれます。
(use-package symbol-overlay
  :hook (prog-mode . symbol-overlay-mode))
;;; ------------- eglot -----------------




;;; ---------code --------------------------------------
;; The built-in outline-minor-mode provides structured code folding in modes
;; such as Emacs Lisp and Python, allowing users to collapse and expand sections
;; based on headings or indentation levels. This feature enhances navigation and
;; improves the management of large files with hierarchical structures.
(use-package outline
  :ensure nil
  :commands outline-minor-mode
  :hook
  ((emacs-lisp-mode . outline-minor-mode)
   ;; Use " ▼" instead of the default ellipsis "..." for folded text to make
   ;; folds more visually distinctive and readable.
   (outline-minor-mode
    .
    (lambda()
      (let* ((display-table (or buffer-display-table (make-display-table)))
             (face-offset (* (face-id 'shadow) (ash 1 22)))
             (value (vconcat (mapcar (lambda (c) (+ face-offset c)) " ▼"))))
        (set-display-table-slot display-table 'selective-display value)
        (setq buffer-display-table display-table))))))

;; The outline-indent Emacs package provides a minor mode that enables code
;; folding based on indentation levels.
;;
;; In addition to code folding, *outline-indent* allows:
;; - Moving indented blocks up and down
;; - Indenting/unindenting to adjust indentation levels
;; - Inserting a new line with the same indentation level as the current line
;; - Move backward/forward to the indentation level of the current line
;; - and other features.
(use-package outline-indent
  :ensure t
  :commands outline-indent-minor-mode

  :custom
  (outline-indent-ellipsis " ▼")

  :init
  ;; The minor mode can also be automatically activated for a certain modes.
  (add-hook 'python-mode-hook #'outline-indent-minor-mode)
  (add-hook 'python-ts-mode-hook #'outline-indent-minor-mode)

  (add-hook 'yaml-mode-hook #'outline-indent-minor-mode)
  (add-hook 'yaml-ts-mode-hook #'outline-indent-minor-mode))

;; The stripspace Emacs package provides stripspace-local-mode, a minor mode
;; that automatically removes trailing whitespace and blank lines at the end of
;; the buffer when saving.
(use-package stripspace
  :ensure t
  :commands stripspace-local-mode

  ;; Enable for prog-mode-hook, text-mode-hook, conf-mode-hook
  :hook ((prog-mode . stripspace-local-mode)
         (text-mode . stripspace-local-mode)
         (conf-mode . stripspace-local-mode))

  :custom
  ;; The `stripspace-only-if-initially-clean' option:
  ;; - nil to always delete trailing whitespace.
  ;; - Non-nil to only delete whitespace when the buffer is clean initially.
  ;; (The initial cleanliness check is performed when `stripspace-local-mode'
  ;; is enabled.)
  (stripspace-only-if-initially-clean nil)

  ;; Enabling `stripspace-restore-column' preserves the cursor's column position
  ;; even after stripping spaces. This is useful in scenarios where you add
  ;; extra spaces and then save the file. Although the spaces are removed in the
  ;; saved file, the cursor remains in the same position, ensuring a consistent
  ;; editing experience without affecting cursor placement.
  (stripspace-restore-column t))
;;; -------------------------------------------------------



;;; ----- markdown ----------------------------------------
;; The markdown-mode package provides a major mode for Emacs for syntax
;; highlighting, editing commands, and preview support for Markdown documents.
;; It supports core Markdown syntax as well as extensions like GitHub Flavored
;; Markdown (GFM).
(use-package markdown-mode
  :commands (gfm-mode
             gfm-view-mode
             markdown-mode
             markdown-view-mode)
  :mode (("\\.markdown\\'" . markdown-mode)
         ("\\.md\\'" . markdown-mode)
         ("README\\.md\\'" . gfm-mode))
  :bind
  (:map markdown-mode-map
        ("C-c C-e" . markdown-do)))
;; Automatically generate a table of contents when editing Markdown files
(use-package markdown-toc
  :ensure t
  :commands (markdown-toc-generate-toc
             markdown-toc-generate-or-refresh-toc
             markdown-toc-delete-toc
             markdown-toc--toc-already-present-p)
  :custom
  (markdown-toc-header-toc-title "**Table of Contents**"))
;;; ----- markdown ----------------------------------------




;;; ----- avy ----------------------------------------
(use-package avy
  :ensure t
  :commands (avy-goto-char
             avy-goto-char-2
             avy-next)
  :init
  (global-set-key (kbd "C-'") 'avy-goto-char-2))
;;; ----- avy ----------------------------------------



;;; ----- Emacsヘルプバッファ ---------------------------
;; Helpful is an alternative to the built-in Emacs help that provides much more
;; contextual information.
(use-package helpful
  :ensure t
  :commands (helpful-callable
             helpful-variable
             helpful-key
             helpful-command
             helpful-at-point
             helpful-function)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-function] . helpful-callable)
  ([remap describe-key] . helpful-key)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  :custom
  (helpful-max-buffers 7))
;;; ----- Emacsヘルプバッファ ---------------------------



;;; ----- keybind ---------------------------
;; window移動
(global-set-key (kbd "C-t") 'other-window)

; コメントアウト
;; (define-key global-map "\C-c;" 'comment-region)
(define-key global-map (kbd "C-;") 'comment-region)

; コメント解除 (:はkbdつけない)
(define-key global-map "\C-c:" 'uncomment-region)

(setq cua-enable-cua-keys nil)  ; CUAキーバインドを無効化
;; 上側に大きくスクロール
;; (define-key global-map "\C-o" 'cua-scroll-down)
(define-key global-map "\C-o" 'scroll-down)

;; mac のcommandとoptionを入れ替える
(if (string-match "ac171.local" (system-name))
   (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super))
 )
(if (string-match "ac171" (system-name))
   (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super))
 )
(if (string-match "AC164-3.local" (system-name))
       (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super))
 )
(if (string-match "ac211.local" (system-name))
       (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super))
)
(if (string-match "ifmac.local" (system-name))
       (setq ns-command-modifier (quote meta))
 (setq ns-alternate-modifier (quote super))
 )

;;reload
;; use-packageの場合、M-x eval-defunを使う
(global-set-key [f12] 'eval-buffer)

;; undo
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-z") 'undo)


;; macのpinchを無効化
(global-set-key (kbd "<pinch>") 'ignore)
(global-set-key (kbd "<C-wheel-up>") 'ignore)
(global-set-key (kbd "<C-wheel-down>") 'ignore)


;; WSLではC-\で日英を切り替え!!!
;; C-\ runs the command toggle-input-method

;; wsl用 C-SPCで日英切り替える
;; 切り替わらない場合、terminalでfcitxを起動する
(global-unset-key (kbd "C-\\"))
(defun start-fcitx ()
  (interactive)
  (start-process "start-fcitx" nil "fcitx"))
(defun toggle-ime ()
  "外部IMEのON/OFFを切り替えるコマンドをEmacsから呼び出す。"
  (interactive)
  ;; 以下はfcitx5の場合の例（wslなど）
  (start-process "fcitx-toggle" nil "fcitx-remote" "-t"))
(global-set-key (kbd "C-\\") 'toggle-ime)
;;; ----- keybind ---------------------------



;;; --------- org ---------------------------
;; templateに用いることができるelement
;; https://orgmode.org/manual/Template-elements.html
;; %フォーマットの表記
;; https://orgmode.org/manual/Template-expansion.html
(use-package org
  :init
  (setq org-return-follows-link t  ; Returnキーでリンク先を開く
        org-mouse-1-follows-link t ; マウスクリックでリンク先を開く
        )
  ;; TODOキーワード設定
  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOIN(i)" "WAIT(w)" "|" "DONE(d)")))
  (setq org-todo-keyword-faces
	'(
	  ("WAIT"  . (:foreground "CadetBlue3"      :weight bold))
	  ("TODO"  . (:foreground "LightGoldenrod3" :weight bold))
	  ))
  ;; DONEステータス時の見出しの色を変えない
  (setq org-fontify-done-headline nil)
  (setq work-directory "~/prog/org/")
  :config
  (setq listfile (concat work-directory "list.org"))
  (setq chatfile (concat work-directory "chats.org"))
  (setq ideafile (concat work-directory "idea/idea.org"))

  (defun yy-mm-file (base-dir file-prefix)
    "Generate a file name like 'YYYY-MM-PREFIX.org' in BASE-DIR."
    (let* ((now (current-time))
           (year (format-time-string "%Y" now))
           (month (format-time-string "%m" now))
           (full-dir (expand-file-name base-dir)))
      (unless (file-directory-p full-dir) ;; ディレクトリが存在しない場合は作成
	(make-directory full-dir t))
      ;; ファイル名を生成
      (expand-file-name (format "%s-%s-%s.org" year month file-prefix) full-dir)))

  (defun yy-mm-dd-file (base-dir file-prefix)
    "Generate a file name like 'YYYY-MM-DD-PREFIX.org' in BASE-DIR."
    (let* ((now (current-time))
           (year (format-time-string "%Y" now))
           (month (format-time-string "%m" now))
	   (day (format-time-string "%d" now))
           (full-dir (expand-file-name base-dir)))
      (unless (file-directory-p full-dir) ;; ディレクトリが存在しない場合は作成
	(make-directory full-dir t))
      ;; ファイル名を生成
      (expand-file-name (format "%s-%s-%s-%s.org" year month day file-prefix) full-dir)))

  ;; (setq taskfile (yy-mm-file (concat work-directory "tasks/") "task"))
  ;; (setq laterfile (yy-mm-file (concat work-directory "later/") "later"))
  ;; (setq techfile (yy-mm-dd-file (concat work-directory "tech/") "tech"))

  (setq memofile (yy-mm-dd-file (concat work-directory "memo/") "memo"))
  (setq chatfile (yy-mm-dd-file (concat work-directory "chat/") "chat"))
  (setq fefile (yy-mm-file (concat work-directory "fe/") "fe"))
  (setq matsuo-lab-file (yy-mm-dd-file (concat work-directory "matsuo-lab-llm-compe/") "matsuo-lab-llm-compe"))


  (setq org-capture-templates
	'(
	  ;; タスク
	  ("t" "task" entry (file memofile)
	   "** TODO %? :todo: \n:PROPERTIES:\n:CREATED: %U\n:TAG: task \n:END:\n%i\n%a\n"  :empty-lines 1)
	  ("l" "あとで読む" entry (file memofile)
           "** %? :later: \n:PROPERTIES:\n:CREATED: %U\n:TAG: later \n:END:\n%i\n%a\n"  :empty-lines 1)
	  ("a" "Any Idea" entry (file memofile)
           "** %? :any: \n:PROPERTIES:\n:CREATED: %U\n:TAG: any \n:END:\n%i\n%a\n"  :empty-lines 1)
	  ("i" "Tech memo" entry (file memofile)
           "** %? :tech: \n:PROPERTIES:\n:CREATED: %U\n:TAG: tech \n:END:\n%i\n%a\n"  :empty-lines 1)
	  ;; ("m" "Memo" entry (file+headline memofile "Memo")
          ;;  "* %? :memo: \n  :PROPERTIES:\n  :CREATED: %U\n  :TAG: memo\n  :END:\n  %i\n  %a\n" :empty-lines 1)
	  ("m" "Memo" entry (file memofile)
           "** %? :memo: \n:PROPERTIES:\n:CREATED: %U\n:TAG: memo \n:END:\n%i\n%a\n" :empty-lines 1 :tree-type month)
	  ("e" "emacs" entry (file memofile)
           "** %? :memo: \n:PROPERTIES:\n:CREATED: %U\n:TAG: memo \n:END:\n%i\n%a\n" :empty-lines 1 :tree-type month)
	  ("p" "Pepar" entry (file memofile)
           "** %? :pepar: \n:PROPERTIES:\n:CREATED: %U\n:TAG: pepar \n:END:\n%i\n%a\n" :empty-lines 1 :tree-type month)


	  ;; ("m" "Memo" entry (file+olp+datetree datetreefile)
          ;;  "** %<%m-%d(%a) %H:%M>\n#+filetags: :memo: \n:PROPERTIES:\n:CREATED: %U\n:TAG: :memo: \n:END:\n%?\n%i\n%a\n" :empty-lines 1 :tree-type month)
	  ("s" "matsuo-lab-llm-compe" entry (file matsuo-lab-file)
           "** %? :llm_compe: \n:PROPERTIES:\n:CREATED: %U\n:TAG: llm_compe \n:END:\n%i\n%a\n" :empty-lines 1 :tree-type month)
	  ("c" "chats" entry (file+headline chatfile "Chats")
	   "** %? :chat: \n\n:PROPERTIES:\n:CREATED: %U\n:TAG: chat\n:END:\n%i\n" :empty-lines 1)
	  ("f" "FE memo" entry (file fefile)
           "* %? :fe: \n:PROPERTIES:\n:CREATED: %U\n:TAG: fe \n:END:\n%i\n%a\n"  :empty-lines 1)
	  )
	)

  ;; agendaの設定
  (defun my-list-subdirectories (dir)
    "指定したディレクトリ DIR の直下にあるディレクトリのリストを返します。"
    (let ((files (directory-files dir t nil))) ;; t で絶対パス、nil でソート
      (cl-loop for file in files
               when (and (file-directory-p file)
			 (not (string-equal (file-name-nondirectory file) "."))
			 (not (string-equal (file-name-nondirectory file) "..")))
               collect (concat file "/")
	       )
      ))
  (setq org-agenda-files (my-list-subdirectories work-directory))
  ;;(setq org-agenda-files '("~/prog/org/memo/"))
  ;; (message org-agenda-files)
  (setq org-agenda-custom-commands
	'(
	  ("s" "List entries with memo tag/property" tags "memo")
	  ("p" "Entries with property TAG=memo" tags "+TAG=\"tech\"")
	  )
	)
  )

;; orgの検索用
(defun my/org-date-string (days-offset)
  "Return date string like '2025-07-01' offset by DAYS-OFFSET from today."
  (format-time-string "%Y-%m-%d"
                      (time-add (current-time)
                                (days-to-time days-offset)))
  )

;; プロパティから時刻文字列を取得し、Emacsの内部時刻形式に変換
(defun my/org-parse-created-timestamp ()
  "Parse CREATED property as a time value, or nil if not present or invalid."
  (let ((ts (org-entry-get nil "CREATED")))
    (when ts
      (condition-case nil
          (encode-time (parse-time-string ts))
        (error nil)))))  ;; エラー時は nil を返す

;; 指定した日数前より後かどうかをチェック
(defun my/org-created-after-days-ago-p (days)
  "Return non-nil if the CREATED property is within the last DAYS days."
  (let ((cutoff (time-subtract (current-time) (days-to-time days))))
    (let ((created-time (my/org-parse-created-timestamp)))
      (and created-time
           (time-less-p cutoff created-time)))))

;; 今日作成されたかチェック
(defun my/org-created-today-p ()
  "Return non-nil if CREATED property is today."
  (let* ((created-time (my/org-parse-created-timestamp))
         (now (current-time)))
    (when created-time
      (let ((created-date (decode-time created-time))
            (now-date (decode-time now)))
        (and (= (nth 3 created-date) (nth 3 now-date))   ;; day
             (= (nth 4 created-date) (nth 4 now-date))   ;; month
             (= (nth 5 created-date) (nth 5 now-date))))))) ;; year

(use-package org-ql
  :after org
  :straight (org-ql :type git :host nil :repo "https://github.com/alphapapa/org-ql.git" :tag "v0.8.10")
  :config
  (setq org-ql-views
	'(
	  ("🕓 今日作成したメモ"
           :buffers-files org-agenda-files
	   :query (my/org-created-today-p)
           :title "🕓 今日作成したメモ"
	   :files org-agenda-files
	   )
	  ("🦑 昨日作成したメモ"
           :buffers-files org-agenda-files
	   :query (my/org-created-after-days-ago-p 1)
           :title "🦑 昨日作成したメモ"
	   :files org-agenda-files
	   )
	  ("📅 過去7日間に作成されたエントリ"
	   :buffers-files org-agenda-files
           :title "📅 過去7日間に作成されたエントリ"
	   :query (my/org-created-after-days-ago-p 7)
           :files org-agenda-files
	   )
          ("📝 メモ"
           :buffers-files org-agenda-files
           :query (tags "memo")
           :title "📝 メモ"
	   :narrow nil
	 )
	;; ("今日のタスク"
        ;;  :buffers-files org-agenda-files
        ;;  :query (and (todo)
        ;;              (ts-active :on today)) ; 今日の日付を持つもの
        ;;  :title "今日のタスク一覧"
        ;;  :sort (ts priority todo)
	;;  :narrow nil
	;;  )
        ;; ("今週の予定"
        ;;  :buffers-files org-agenda-files
        ;;  :query (ts-active :from today :to 7)
        ;;  :title "今週の予定"
	;;  :narrow nil
	;;  ) ;; 今日から7日以内
	)
    )
  )

;; アンダースコアを入力しても下付き文字にならないようにする
(setq org-use-sub-superscripts '{}
      org-export-with-sub-superscripts nil)


;; org-indent-mode
;; インデント機能を有効にしています。
(use-package org-indent
  :straight nil
  :ensure nil
  :hook (org-mode . org-indent-mode))

;; org-mode用のtheme
(use-package org-modern
  :straight ( org-modern :type git :host nil :repo "https://github.com/minad/org-modern.git" :tag "1.9")
  :custom
  (org-modern-fold-stars '(("▶" . "▼") ("▷" . "▽") ("▸" . "▾") ("▹" . "▿") ("▸" . "▾")))
  :config
  (setopt
   ;; Edit settings
   org-auto-align-tags nil
   org-tags-column 0
   org-catch-invisible-edits 'show-and-error
   org-special-ctrl-a/e t
   org-insert-heading-respect-content t

   ;; Org styling, hide markup etc.
   org-hide-emphasis-markers t
   org-pretty-entities t

   ;; Agenda styling
   org-agenda-tags-column 0
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "◀── now ─────────────────────────────────────────────────")

  ;; Ellipsis styling
  (setopt org-ellipsis "…")
  (set-face-attribute 'org-ellipsis nil :inherit 'default :box nil)

  (global-org-modern-mode))
;;; --------- org ---------------------------




;;; -----------------------------------------
;; Tree-sitter in Emacs is an incremental parsing system introduced in Emacs 29
;; that provides precise, high-performance syntax highlighting. It supports a
;; broad set of programming languages, including Bash, C, C++, C#, CMake, CSS,
;; Dockerfile, Go, Java, JavaScript, JSON, Python, Rust, TOML, TypeScript, YAML,
;; Elisp, Lua, Markdown, and many others.
(use-package treesit-auto
  :ensure t
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))
;;; -----------------------------------------



;;; -----------------------------------------
(use-package reformatter
  :ensure t
  :config
  (reformatter-define go-format
    :program "goimports")
  (reformatter-define web-format
    :program "prettier"
    :args `("--write" "--stdin-filepath" ,buffer-file-name))
  (reformatter-define python-format
    :program "ruff"
    :args `("format" "--stdin-filename" ,buffer-file-name))
  :hook
  (go-ts-mode . go-format-on-save-mode)
  (typescript-ts-mode . web-format-on-save-mode)
  (tsx-ts-mode . web-format-on-save-mode)
  (json-ts-mode . web-format-on-save-mode)
  (python-ts-mode . python-format-on-save-mode))
;;; -----------------------------------------


;;; --------- python --------------------------------
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
(add-hook 'python-mode-hook #'eglot-ensure)
(add-hook 'python-ts-mode-hook #'eglot-ensure)
;;; -----------------------------------------


;;; --------- typescript --------------------------------
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(add-hook 'typescript-ts-hook #'eglot-ensure)
(add-hook 'tsx-ts-hook #'eglot-ensure)
;;; -----------------------------------------


;;; post-init.el ends here
