;;; pacakge -- myconf.el
;;; commentary:
;;; summary:
;;; code:

;;; ------------- marp -----------------
(defun my/marp-start-server ()
    "Start Marp server in current directory."
    (interactive)
    (let ((default-directory (file-name-directory (buffer-file-name))))
      (start-process-shell-command
       "marp-server"
       "*marp-server*"
       (format "marp --allow-local-files --html --theme /Users/ac211/prog/slide/themes/base.scss --server \"%s\"" default-directory))
      (browse-url "http://localhost:8080/"))
    )

;; brew install pngpaste
(defun my/save-screenshot-from-clipboard (filepath)
  "クリップボードの画像を指定されたファイルパスに保存します。
macOSで 'pngpaste' がインストールされている必要があります。
例: M-x my-save-screenshot-from-clipboard RET ~/image/sample.png"
  (interactive "Mファイルパス: ")
  (if (executable-find "pngpaste")
      (progn
        (shell-command (format "pngpaste %s" (shell-quote-argument filepath)))
        (message "画像を %s に保存しました。" filepath))
    (error "pngpaste が見つかりません。Homebrewでインストールしてください。")))

(defun my/insert-screenshot-markdown ()
  "クリップボードの画像を './image-N.png' として現在のディレクトリに保存し、
カーソル位置にその画像のMarkdownリンクを挿入します。
macOSで 'pngpaste' がインストールされている必要があります。
例: M-x my-insert-screenshot-markdown"
  (interactive)
  (unless (executable-find "pngpaste")
    (error "pngpaste が見つかりません。Homebrewでインストールしてください。"))

  (let* ((base-dir default-directory)
         (file-num 1)
         (file-path nil)
         (relative-path nil))
    ;; 連番のファイル名を生成
    (while (file-exists-p
            (setq file-path
                  (expand-file-name
                   (format "image-%d.png" file-num)
                   base-dir)))
      (setq file-num (1+ file-num)))

    ;; クリップボードの画像を保存
    (shell-command (format "pngpaste %s" (shell-quote-argument file-path)))
    (message "画像を %s に保存しました。" file-path)

    ;; 相対パスを作成
    (setq relative-path (file-relative-name file-path base-dir))

    ;; Markdown形式で挿入
    (insert (format "![](./%s)" relative-path))))
;;; ------------- marp -----------------



;;; myconf.el ends here
