;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-pathに追加するフォルダ
;; 2つ以上フォルダを指定する場合の引数 => (add-to-load-path "elisp" "xxx" "xxx")
;; (add-to-load-path "elisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; MELPA
(defvar my-favorite-package-list
  '(anzu
    atom-one-dark-theme
    auto-complete
	ess
	e2wm-R
    expand-region
    helm
	inlineR
    js2-mode
    multiple-cursors
    rainbow-delimiters
    smartparens
    smartrep
    tabbar
    undo-tree
    yatex
    foreign-regexp)
  "packages to be installed")

(require 'package)

;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

;; MELPA-stableを追加
 (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

;; Marmaladeを追加
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; 初期化
 (package-initialize)

;; 未インストールパッケージをインストール
(unless package-archive-contents (package-refresh-contents))
(dolist (pkg my-favorite-package-list)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UTF-8を使う
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8-unix)
(setq coding-system-for-write 'utf-8-unix)

;; スタートアップメッセージを表示させない
(setq inhibit-startup-message t)

;; 日時を表示
(setq display-time-day-and-date t)  ;; 曜日・月・日
(setq display-time-24hr-format t)   ;; 24時表示
(display-time-mode t)

;; ファイルサイズを表示
;; (size-indication-mode t)

;; カーソル行に下線を表示
(setq hl-line-face 'underline)
(global-hl-line-mode t)

;; スクロールバーを非表示
(set-scroll-bar-mode nil)

;; ビープ音を消す
(set-message-beep 'silent)

;; "yes or no" の選択を "y or n" にする
(fset 'yes-or-no-p 'y-or-n-p)

;; scratchの初期メッセージ消去
(setq initial-scratch-message "")

;; ツールバー非表示
(tool-bar-mode -1)

;; フォント設定
(add-to-list 'default-frame-alist '(font . "Myrica M"))

;; テーマの設定
;; (load-theme 'zenburn' t)
;; CUI用設定を、以下に記述
(if (not window-system) (progn
	(load-theme 'wombat' t)
))
;; GUI用設定を、以下に記述
(if window-system (progn
	(load-theme 'atom-one-dark' t)
))

;; Emacsのカラーテーマ
;; http://code.google.com/p/gnuemacscolorthemetest/
 (when (and (require 'color-theme nil t) (window-system))
   (color-theme-initialize)
   (color-theme-clarity))

;; Ctrl+hでBackSpace
(keyboard-translate ?\C-h ?\C-?)

;; Ctrl+uを無効化(IMEの切り替えに利用)
;; (global-unset-key "\C-u")

;; C-=とC--でフォントサイズ変更
(define-key global-map (kbd "C-=") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(define-key global-map (kbd "C-M-0") 'text-scale-adjust)

;; C-tで次のウィンドウに移動
(define-key global-map (kbd "C-t") 'other-window)

;; M-tで前のウィンドウに移動
(defun prev-window ()
   (interactive)
   (other-window -1))
 (define-key global-map (kbd "M-t") 'prev-window)

;; フルスクリーン化
(global-set-key (kbd "<C-M-return>") 'toggle-frame-fullscreen)

;; ctrl-mause_wheelでフォント拡大・縮小
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Alt+kで1行コピー
;; Alt+Kで1行削除
(defun copy-whole-line (&optional arg)
  "Copy current line."
  (interactive "p")
  (or arg (setq arg 1))
  (if (and (> arg 0) (eobp) (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0) (bobp) (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (unless (eq last-command 'copy-region-as-kill)
    (kill-new "")
    (setq last-command 'copy-region-as-kill))
  (cond ((zerop arg)
         (save-excursion
           (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
           (copy-region-as-kill (point) (progn (end-of-visible-line) (point)))))
        ((< arg 0)
         (save-excursion
           (copy-region-as-kill (point) (progn (end-of-visible-line) (point)))
           (copy-region-as-kill (point)
                                (progn (forward-visible-line (1+ arg))
                                       (unless (bobp) (backward-char))
                                       (point)))))
        (t
         (save-excursion
           (copy-region-as-kill (point) (progn (forward-visible-line 0) (point)))
           (copy-region-as-kill (point)
                                (progn (forward-visible-line arg) (point))))))
  (message (substring (car kill-ring-yank-pointer) 0 -1)))

  (global-set-key (kbd "M-k") 'copy-whole-line)
  (global-set-key (kbd "M-K") 'kill-whole-line)


;; アクティブでないバッファを全て閉じる

(defun my/filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))
(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (let* ((no-kill-buffer-names
          ;; 消さないバッファ名を指定
          (list (buffer-name (current-buffer))
                "*Messages*" "*Compile-Log*" "*Help*"
                "*init log*" "*Ibuffer*" "*scratch*"
                "*MULTI-TERM-DEDICATED*"))
         (interested-buffers
          (my/filter
           '(lambda (buffer)
              (and
               ;; donk kill buffers who has the windows displayed in
               (not (get-buffer-window (buffer-name buffer)))
               ;; dont kill hidden buffers (hidden buffers' name starts with SPACE)
               (not (string-match "^ " (buffer-name buffer)))
               ;; dont kill buffers who have running processes
               (let ((proc (get-buffer-process buffer)))
                 (if proc
                     (equal 'exit
                            (process-status
                             (get-buffer-process buffer)))
                   t))))
           (buffer-list)))
         (buffers-to-kill
          (set-difference interested-buffers
                          (mapcar '(lambda (buffer-name)
                                     (get-buffer buffer-name))
                                  no-kill-buffer-names))))
    (mapc 'kill-buffer buffers-to-kill)))
;; (global-set-key (kbd "C-c C-b C-b") 'kill-other-buffers)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 列数を表示する
(column-number-mode t)

;; 行数を表示する
 (global-linum-mode t)
 (set-face-attribute 'linum nil
;;                  :foreground "#800"
					 :height 1.0)

;; 左側の行表示をフォントサイズ拡大・縮小に合わせて伸縮させる
(defun linum-update-window-scale-fix (win)
  "fix linum for scaled text"
  (set-window-margins win
          (ceiling (* (if (boundp 'text-scale-mode-step)
                  (expt text-scale-mode-step
                    text-scale-mode-amount) 1)
              (if (car (window-margins))
                  (car (window-margins)) 1)
              ))))
(advice-add #'linum-update-window :after #'linum-update-window-scale-fix)


;; 5 桁分の表示領域を確保する
 (setq linum-format "%5d |")

;; モードラインに行番号表示
 (line-number-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; モードラインの割合表示を総行数表示
(defvar my-lines-page-mode t)
(defvar my-mode-line-format)

(when my-lines-page-mode
  (setq my-mode-line-format "%d")
  (if size-indication-mode
      (setq my-mode-line-format (concat my-mode-line-format " of %%I")))
  (cond ((and (eq line-number-mode t) (eq column-number-mode t))
         (setq my-mode-line-format (concat my-mode-line-format " (%%l,%%c)")))
        ((eq line-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " L%%l")))
        ((eq column-number-mode t)
         (setq my-mode-line-format (concat my-mode-line-format " C%%c"))))

  (setq mode-line-position
        '(:eval (format my-mode-line-format
                        (count-lines (point-max) (point-min))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 改行コードを表示する
(setq eol-mnemonic-dos "(CRLF)")
(setq eol-mnemonic-mac "(CR)")
(setq eol-mnemonic-unix "(LF)")

;; スペース、タブなどを可視化する
;; (global-whitespace-mode 1)

;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; タブ幅
(custom-set-variables
;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(anzu-deactivate-region t)
 '(anzu-mode-lighter "")
 '(anzu-search-threshold 1000)
 '(package-selected-packages
   (quote
	(smartrep multiple-cursors expand-region anzu undo-tree helm tabbar smartparens js2-mode auto-complete)))
 '(send-mail-function (quote mailclient-send-it))
 '(tab-width 4))

;; 対応する括弧を光らせる
(show-paren-mode 1)

;; 対応する括弧を強調させる
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; 括弧の自動補完
(require 'smartparens-config)
(smartparens-global-mode t)

;; バックアップファイルを作成させない
(setq make-backup-files nil)

;; 終了時にオートセーブファイルを削除する
(setq delete-auto-save-files t)

;; beep音を消す
(defun my-bell-function ()
  (unless (memq this-command
        '(isearch-abort abort-recursive-edit exit-minibuffer
              keyboard-quit mwheel-scroll down up next-line previous-line
              backward-char forward-char))
    (ding)))
(setq ring-bell-function 'my-bell-function)

;; 1行ずつスクロール
(setq scroll-conservatively 35
      scroll-margin 0
      scroll-step 1)
(setq comint-scroll-show-maximum-output t) ;; shell-mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; タブ化
;; http://www.emacswiki.org/emacs/tabbar.el
;;(require 'cl)

(when (require 'tabbar nil t)
  (tabbar-mode))

;; ボタンをシンプルにする
(setq tabbar-home-button-enabled "")
(setq tabbar-scroll-right-button-enabled "")
(setq tabbar-scroll-left-button-enabled "")
(setq tabbar-scroll-right-button-disabled "")
(setq tabbar-scroll-left-button-disabled "")

;; Ctrl-Tab, Ctrl-Shift-Tab でタブを切り替える
(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))
(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))
(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))
(global-set-key [(control tab)] 'shk-tabbar-next)
(global-set-key [(control shift tab)] 'shk-tabbar-prev)

;; GUIで直接ファイルを開いた場合フレームを作成しない
(add-hook 'before-make-frame-hook
          (lambda ()
            (when (eq tabbar-mode t)
              (switch-to-buffer (buffer-name))
              (delete-this-frame))))

;; 例: 全バッファを一つのグループにしまう
(setq tabbar-buffer-groups-function (lambda () (list "Buffers")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for javascript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; auto-completeの設定
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-quick-help-delay 0.5) ;; 補完候補を0.5秒で表示
(global-auto-complete-mode t)
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ

;; for markdown-mode
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for eww
(setq eww-search-prefix "http://www.google.co.jp/search?q=")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for Ruby
;; (require 'ruby-electric)
;; (add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))
;; (setq ruby-electric-expand-delimiters-list nil)

;; 同名バッファを分かりやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for helm
(require 'helm-config)
(helm-mode 1)
(define-key global-map (kbd "M-x") 'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for undo-tree
(require 'undo-tree)
(global-undo-tree-mode)

;; M-/でredo
(global-set-key (kbd "M-/") 'undo-tree-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for anzu
;; 検索時に全体数と現在の位置を表示
;; 常にanzu-modeを有効化
(global-anzu-mode +1)

;; モードラインにモード名を表示しない
;; リージョン指定で置換を行った後，ハイライトを無効化
;; 全体のマッチ数の上限を1000件に設定

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for twittering-mode
;; (require 'twittering-mode)

;; アイコンを表示
;; (setq twittering-icon-mode t)

;; 未読ツイート件数を表示
;; (twittering-enable-unread-status-notifier)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for multi-cursor, expand-region and smartrep
(require 'expand-region)
(require 'multiple-cursors)
(require 'smartrep)

;; C-,で選択範囲の拡大，C-M-,で選択範囲の縮小
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; C-M-cでmulti-cursorを用いた編集など
(global-set-key (kbd "C-M-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)
(smartrep-define-key global-map "C-."
  '(("C-t"      . 'mc/mark-next-like-this)
    ("n"        . 'mc/mark-next-like-this)
    ("p"        . 'mc/mark-previous-like-this)))

;;矩形選択した位置に連番を挿入(M-iにバインド)
(defvar my-rectangle-seq-format "%d")
(defun my-sequence-rectangle (start end first incr format)
  "Resequence each line of rectangle starting from FIRST.
The numbers are formatted according to the FORMAT string.
Called from a program, takes five args; START, END, FIRST, INCR and FORMAT."
  (interactive
   (progn (barf-if-buffer-read-only)
          (list
           (region-beginning)
           (region-end)
           (if current-prefix-arg
               (prefix-numeric-value current-prefix-arg)
             (string-to-number
              (read-string "Start value: (0) " nil nil "0")))
           (string-to-number
            (read-string "Increment: (1) " nil nil "1"))
           (read-string (concat "Format: (" my-rectangle-seq-format ") ")))))
  (if (= (length format) 0)
      (setq format my-rectangle-seq-format)
    (setq my-rectangle-seq-format format))
  (apply-on-rectangle 'my-string-rectangle-line start end
                      '(lambda ()
                         (insert (format format first))
                         (setq first (+ first incr)))))

(defun my-string-rectangle-line (startcol endcol func)
  (move-to-column startcol t)
  (funcall func))
(provide 'my-sequence-rectangle)
(define-key global-map (kbd "M-i") 'my-sequence-rectangle)

;; ;; cua-mode
;; (cua-mode t)
;; (setq cua-enable-cua-keys nil)

;; ;; cua-modeで選択中にM-Iでデクリメント
;; (defun cua-decr-rectangle (decriment)
;;   "Decrement each line of CUA rectangle by prefix amount."
;;   (interactive "p")
;;   (cua-incr-rectangle (- decriment)))
;; (define-key cua--rectangle-keymap (kbd "M-I") 'cua-decr-rectangle)

;; ;; ;; C-SPCをset-mark-commandに再定義
;; (global-unset-key "\C-SPC")
;; (define-key global-map (kbd "C-SPC") 'set-mark-command)
;; ;; (define-key  (kbd "C-SPC") 'set-mark-command)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rの設定
;; http://y-mattu.hatenablog.com/entry/rstudio_emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ess-site)

;; 拡張子が r, R の場合に R-mode を起動
(add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))

;; R-mode を起動する時に ess-site をロード
(autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)

;; R を起動する時に ess-site をロード
(autoload 'R "ess-site" "start R" t)

;; R-mode, iESS を起動する際に呼び出す初期化関数
(setq ess-loaded-p nil)
(defun ess-load-hook (&optional from-iess-p)
  ;; インデントの幅を 2 にする（デフォルト 2）
  (setq ess-indent-level 2)
  ;; インデントを調整
  (setq ess-arg-function-offset-new-line (list ess-indent-level))
  ;; comment-region のコメントアウトに # を使う（デフォルト##）
  (make-variable-buffer-local 'comment-add)
  (setq comment-add 0)


  ;; 最初に ESS を呼び出した時の処理
  (when (not ess-loaded-p)
    ;; 補完機能を有効にする
    (setq ess-use-auto-complete t)
    ;; helm を使いたいので IDO は邪魔
    (setq ess-use-ido nil)
    ;; キャレットがシンボル上にある場合にもエコーエリアにヘルプを表示する
    (setq ess-eldoc-show-on-symbol t)
    ;; 起動時にワーキングディレクトリを尋ねられないようにする
    (setq ess-ask-for-ess-directory nil)
    ;; # の数によってコメントのインデントの挙動が変わるのを無効にする
    (setq ess-fancy-comments nil)
    (setq ess-loaded-p t)
    (unless from-iess-p
      ;; ウィンドウが 1 つの状態で *.R を開いた場合はウィンドウを横に分割して R を表示する
      (when (one-window-p)
        ;(split-window-below)
		(split-window-right)
        (let ((buf (current-buffer)))
          (ess-switch-to-ESS nil)
          (switch-to-buffer-other-window buf)))
      ;; R を起動する前だと auto-complete-mode が off になるので自前で on にする
      ;; cf. ess.el の ess-load-extras
      (when (and ess-use-auto-complete (require 'auto-complete nil t))
        (add-to-list 'ac-modes 'ess-mode)
        (mapcar (lambda (el) (add-to-list 'ac-trigger-commands el))
                '(ess-smart-comma smart-operator-comma skeleton-pair-insert-maybe))
        ;; auto-complete のソースを追加
        (setq ac-sources '(ac-source-acr
                           ac-source-R
                           ac-source-filename
                           ac-source-yasnippet)))))

  (if from-iess-p
      ;; R のプロセスが他になければウィンドウを分割する
      (if (> (length ess-process-name-list) 0)
          (when (one-window-p)
			;(split-window-horizontally)
			(split-window-vertically)
            (other-window 1)))
    ;; *.R と R のプロセスを結びつける
    ;; これをしておかないと補完などの便利な機能が使えない
    (ess-force-buffer-current "Process to load into: ")))

;; R-mode 起動直後の処理
(add-hook 'R-mode-hook 'ess-load-hook)

;; R 起動直前の処理
(defun ess-pre-run-hooks ()
  (ess-load-hook t))
(add-hook 'ess-pre-run-hook 'ess-pre-run-hooks)

;; auto-complete-acr
;; (require 'auto-complete-acr)

;; e2wm-R
(require 'e2wm-R)

;; e2wm:dp-R-view, e2wm:stop-management を toggleis する
(defun e2wm:toggle-management ()
  (interactive)
  (if (e2wm:pst-get-instance)
      (e2wm:stop-management) (e2wm:dp-R-view)))
(define-key ess-mode-map (kbd "C-,") 'e2wm:toggle-management)

;; ソースコードにインライン画像を埋め込む
(require 'inlineR)
(define-key ess-mode-map "\C-s-i" 'inlineR-insert-tag)
(setq inlineR-re-funcname ".*plot.*\\|.*gg.*") ;; 作図関数追加

;; インライン画像表示の切り替え
(defun uimage-mode-toggle ()
  (interactive)
  (uimage-mode 'toggle))

(global-set-key (kbd "C-s-I") 'uimage-mode-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; YaTeX
;;
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq auto-mode-alist
      (append '(("\\.tex$" . yatex-mode)
                ("\\.ltx$" . yatex-mode)
                ("\\.cls$" . yatex-mode)
                ("\\.sty$" . yatex-mode)
                ("\\.clo$" . yatex-mode)
                ("\\.bbl$" . yatex-mode)) auto-mode-alist))
(setq YaTeX-inhibit-prefix-letter t)
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)
(setq YaTeX-use-LaTeX2e t)
(setq YaTeX-use-AMS-LaTeX t)
(setq YaTeX-dvi2-command-ext-alist
      '(("TeXworks\\|texworks\\|texstudio\\|mupdf\\|SumatraPDF\\|Preview\\|Skim\\|TeXShop\\|evince\\|atril\\|xreader\\|okular\\|zathura\\|qpdfview\\|Firefox\\|firefox\\|chrome\\|chromium\\|MicrosoftEdge\\|microsoft-edge\\|Adobe\\|Acrobat\\|AcroRd32\\|acroread\\|pdfopen\\|xdg-open\\|open\\|start" . ".pdf")))

;(setq tex-command "ptex2pdf -u -l -ot \"-kanji=utf8 -no-guess-input-enc -synctex=1\"")
;(setq tex-command "ptex2pdf -l -od \"-f uptex-noEmbed-04.map -f otf-up-noEmbed.map\"")
;(setq tex-command "ptex2pdf -l -od \"-f uptex-kozuka-pr6n-04.map -f otf-up-kozuka-pr6n.map\"")
(setq tex-command "ptex2pdf -l -od \"-f otf-ipaex.map\"")
;(setq tex-command "ptex2pdf -l -od \"-f otf-ipa.map\"")
;(setq tex-command "lualatex -cmdx -synctex=1")
;(setq tex-command "latexmk")
;(setq tex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/upmendex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
;(setq tex-command "latexmk -e \"$lualatex=q/lualatex -cmdx %O -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/upmendex %O -o %D %S/\" -norc -gg -pdflua")
;(setq bibtex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/upmendex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
(setq bibtex-command "latexmk -e \"$latex=q/platex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/pbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/mendex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
;(setq makeindex-command "latexmk -e \"$latex=q/uplatex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/upbibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/upmendex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
(setq makeindex-command "latexmk -e \"$latex=q/platex %O -kanji=utf8 -no-guess-input-enc -synctex=1 %S/\" -e \"$bibtex=q/bibtex %O %B/\" -e \"$biber=q/biber %O --bblencoding=utf8 -u -U --output_safechars %B/\" -e \"$makeindex=q/mendex %O -o %D %S/\" -e \"$dvipdf=q/dvipdfmx %O -o %D %S/\" -norc -gg -pdfdvi")
(setq dvi2-command "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance")
;(setq dvi2-command "texworks")
;(setq dvi2-command "texstudio --pdf-viewer-only")
(setq tex-pdfview-command "rundll32 shell32,ShellExec_RunDLL SumatraPDF -reuse-instance")
;(setq tex-pdfview-command "texworks")
;(setq tex-pdfview-command "texstudio --pdf-viewer-only")
(setq dviprint-command-format "powershell -Command \"& {$r = Write-Output %s;$p = [System.String]::Concat('\"\"\"',[System.IO.Path]::GetFileNameWithoutExtension($r),'.pdf','\"\"\"');Start-Process AcroRd32 -ArgumentList ($p)}\"")
(defun fwdsumatrapdf-forward-search ()
  (interactive)
  (progn
    (process-kill-without-query
     (start-process
      "fwdsumatrapdf"
      nil
      "fwdsumatrapdf"
      (expand-file-name
       (concat (file-name-sans-extension (or YaTeX-parent-file
                                             (save-excursion
                                               (YaTeX-visit-main t)
                                               buffer-file-name)))
               ".pdf"))
      (buffer-name)
      (number-to-string (save-restriction
                          (widen)
                          (count-lines (point-min) (point))))))))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (define-key YaTeX-mode-map (kbd "C-c f") 'fwdsumatrapdf-forward-search)))
(add-hook 'yatex-mode-hook
          '(lambda ()
             (auto-fill-mode -1)))
;;
;; RefTeX with YaTeX
;;
;(add-hook 'yatex-mode-hook 'turn-on-reftex)
(add-hook 'yatex-mode-hook
          '(lambda ()
             (reftex-mode 1)
             (define-key reftex-mode-map (concat YaTeX-prefix ">") 'YaTeX-comment-region)
             (define-key reftex-mode-map (concat YaTeX-prefix "<") 'YaTeX-uncomment-region)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Windowsの場合
(when (equal system-type 'windows-nt)

(load-file "~/.emacs.d/conf/init_windows.el")
(load-file "~/.emacs.d/local/local.el")

)

;; rubyの正規表現を使う
(require 'foreign-regexp)
(custom-set-variables
 '(foreign-regexp/regexp-type 'ruby)
 '(reb-re-syntax 'foreign-regexp)
)

