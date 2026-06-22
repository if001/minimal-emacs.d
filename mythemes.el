;;; mythemes.el --- -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'cl-lib)

;; nano-emacsのnano-modeline.elを使う場合に、モードラインの右に, branchやlsp、flykmakeの状態を表示するようにする
(with-eval-after-load 'nano
  ;; reload しても元関数を上書き保存しないようにする
  (unless (fboundp 'my/nano-modeline--original-default-mode)
    (defalias 'my/nano-modeline--original-default-mode
      (symbol-function 'nano-modeline-default-mode)))

  (defcustom my/nano-modeline-right-padding 0
    "Number of spaces inserted at the right edge of nano-modeline."
    :type 'integer
    :group 'nano-modeline)

  (defun my/nano-modeline--right-padding ()
    (make-string my/nano-modeline-right-padding ?\s))

  (defun my/nano-modeline--join (&rest parts)
    (string-join (delq nil parts) "/"))

  (defun my/nano-modeline--lsp-on-p ()
    "Return non-nil when lsp-mode or eglot is active."
    (or (bound-and-true-p lsp-mode)
        (and (fboundp 'eglot-managed-p)
             (ignore-errors (eglot-managed-p)))))

  (defun my/nano-modeline--flymake-type-kind (type)
    "Flymake diagnostic TYPE を :error / :warning / :note に正規化する。"
    (let ((category (get type 'flymake-category)))
      (cond
       ((or (eq type :error)
            (eq type 'flymake-error)
            (eq category 'flymake-error))
        :error)

       ((or (eq type :warning)
            (eq type 'flymake-warning)
            (eq category 'flymake-warning))
        :warning)

       ((or (eq type :note)
            (eq type 'flymake-note)
            (eq category 'flymake-note))
        :note)

       (t
        type))))

  (defun my/nano-modeline--flymake-counts ()
    "Flymake の error / warning 数を (ERRORS . WARNINGS) で返す。"
    (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
      (let ((errors 0)
            (warnings 0))
        ;; 引数なしの flymake-diagnostics は current buffer 全体を見る
        (dolist (diag (flymake-diagnostics))
          (pcase (my/nano-modeline--flymake-type-kind
                  (flymake-diagnostic-type diag))
            (:error
             (cl-incf errors))
            (:warning
             (cl-incf warnings))))
        (cons errors warnings))))

  (defun my/nano-modeline--flymake-status ()
    "Return flymake error / warning status string."
    (when-let* ((counts (my/nano-modeline--flymake-counts)))
      (let ((errors (car counts))
            (warnings (cdr counts)))
        (my/nano-modeline--join
         (concat
          (nerd-icons-faicon "nf-fa-times_circle")
          " "
          (propertize (number-to-string errors)
                      'face 'nano-face-header-critical))
         (concat
          (nerd-icons-faicon "nf-fa-warning")
          " "
          (propertize (number-to-string warnings)
                      'face 'nano-face-header-popout))))))

  (defun my/nano-modeline--right ()
    "Return right side string for prog-mode nano-modeline."
    ;; nano-modeline.el 側の nano-mode-name / vc-branch を利用する
    (let* ((mode-name (nano-mode-name))
           (branch (vc-branch))
           (branch-name (when branch
                          (string-remove-prefix "#" branch))))
      (concat
       (my/nano-modeline--join
        ;; major mode
        (propertize mode-name
                    'face 'nano-face-header-default)

        ;; Git branch
        (when branch-name
          (concat
           (nerd-icons-faicon "nf-fa-code_branch")
           (propertize branch-name
                       'face 'nano-face-header-default)))

        ;; LSP / Eglot
        (when (my/nano-modeline--lsp-on-p)
          (concat
           (nerd-icons-faicon "nf-fa-rocket")
           )
          )

        ;; Flymake
        (my/nano-modeline--flymake-status))

       ;; right edge padding
       (my/nano-modeline--right-padding))))

  (defun my/nano-modeline-prog-mode ()
    "My extended nano-modeline for prog-mode."
    (let ((buffer-name (format-mode-line "%b"))
          ;;(position (format-mode-line "%l:%c"))
          )
      (nano-modeline-compose
       (nano-modeline-status)
       buffer-name
       ""
       (my/nano-modeline--right))))

  ;; nano-modeline 側の default mode を override。
  ;; prog-mode のときだけ拡張版を使い、それ以外は元の実装に戻す。
  (defun nano-modeline-default-mode ()
    (if (nano-modeline-prog-mode-p)
        (my/nano-modeline-prog-mode)
      (my/nano-modeline--original-default-mode)))

  ;; flymake / lsp / eglot の状態変化で再描画
  (dolist (hook '(flymake-mode-hook
                  lsp-mode-hook
                  eglot-managed-mode-hook))
    (add-hook hook #'force-mode-line-update)))

(provide 'my-nano-modeline)

;;; mytyemes.el ends here
