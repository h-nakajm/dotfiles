;; superキーの設定
;; 左WindowsキーをSuperに設定
(setq w32-pass-lwindow-to-system nil)
(setq w32-lwindow-modifier 'super)

;; C-s-jで行番号ジャンプ
(define-key global-map (kbd "C-s-j") 'goto-line)

;; アクティブでないバッファを全て閉じる
(global-set-key (kbd "C-s-b k") 'kill-other-buffers)

