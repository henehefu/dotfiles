; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

;; Emacs 23 より前のバージョンを利用している方は
;; user-emacs-directory変数が未定義のため次の設定を追加
(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

;; load-path を追加する関数を定義
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-addsubdirs-to-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "lib")


; フォント設定
(when window-system
 (set-face-attribute 'default nil
                     :family "monaco"
                     :height 180)
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0208
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'japanese-jisx0212
  '("Hiragino Maru Gothic Pro" . "iso10646-1"))
 (set-fontset-font
  (frame-parameter nil 'font)
  'mule-unicode-0100-24ff
  '("monaco" . "iso10646-1"))
 (setq face-font-rescale-alist
      '(("^-apple-hiragino.*" . 1.2)
        (".*osaka-bold.*" . 1.2)
        (".*osaka-medium.*" . 1.2)
        (".*courier-bold-.*-mac-roman" . 1.0)
        (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
        (".*monaco-bold-.*-mac-roman" . 0.9)
        ("-cdac$" . 1.3))))

(setq ring-bell-function 'ignore)

; C-hでBackSpace
(keyboard-translate ?\C-h ?\C-?)

;; スペースで補完
(if (boundp 'minibuffer-local-filename-completion-map)
     (define-key minibuffer-local-filename-completion-map
       " " 'minibuffer-complete-word))
(if (boundp 'minibuffer-local-must-match-filename-map)
    (define-key minibuffer-local-must-match-filename-map
      " " 'minibuffer-complete-word))

(when (>= emacs-major-version 23)
  (require 'linum)
  (global-linum-mode))


(define-key global-map [ns-drag-file] 'ns-find-file)
(setq ns-pop-up-frames nil)

; ctrl-zをundoにする
(global-set-key "\C-z" 'undo)

(global-set-key "\M-g" 'goto-line)
(global-set-key (kbd "C-x g") 'grep)

;; バックアップ・ファイルを作らない
(setq make-backup-files nil)

(setq-default tab-width 4 indent-tabs-mode nil)

(show-paren-mode t)

;; haskell
(load "~/.emacs.d/lib/haskell-mode/haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)

;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;(define-key haskell-mode-map (kbd "C-x C-s") 'haskell-mode-save-buffer)


;; html5
;; (add-to-list 'load-path "~/.emacs.d/lib/html5-el/")

;; (eval-after-load "rng-loc"
;;   '(add-to-list 'rng-schema-locating-files "~/.emacs.d/lib/html5-el/schemas.xml"))

;; (require 'whattf-dt)

(add-to-list 'load-path "~/.emacs.d/lib/zencoding/")
(require 'zencoding-mode)
(add-hook 'sgml-mode-hook 'zencoding-mode) ;; Auto-start on any markup modes

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(scroll-error-top-bottom t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path "~/.emacs.d/lib/")
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))


;;php-mode
(add-to-list 'load-path "~/.emacs.d/lib/php-mode/")
(load-library "php-mode")
(require 'php-mode)


(autoload 'coffee-mode "coffee-mode" "Major mode for editing CoffeeScript." t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))


