; 言語を日本語にする
(set-language-environment 'Japanese)
; 極力UTF-8とする
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(cd "~/Documents")
(column-number-mode t)
(setq-default tab-width 4)

(when (require 'package nil t)
  (add-to-list 'package-archives
               '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (package-initialize))

;; Emacs 23 より前のバージョンを利用している方は
;; user-emacs-directory変数が未定義のため次の設定を追加
(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; load-path を追加する関数を定義
;; なんか動いてなくね？(´・ω・`) 
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-addsubdirs-to-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; 引数のディレクトリとそのサブディレクトリをload-pathに追加
(add-to-load-path "~/.emacs.d/lib/")


; フォント設定
(when window-system
;;; フォントセットを作る
  (let* ((fontset-name "myfonts") ; フォントセットの名前
         (size 18) ; ASCIIフォントのサイズ [9/10/12/14/15/17/19/20/...]
         (asciifont "Menlo") ; ASCIIフォント
         (jpfont "Hiragino Maru Gothic ProN") ; 日本語フォント
         (font (format "%s-%d:weight=normal:slant=normal" asciifont size))
         (fontspec (font-spec :family asciifont))
         (jp-fontspec (font-spec :family jpfont)) 
         (fsn (create-fontset-from-ascii-font font nil fontset-name)))
    (set-fontset-font fsn 'japanese-jisx0213.2004-1 jp-fontspec)
    (set-fontset-font fsn 'japanese-jisx0213-2 jp-fontspec)
    (set-fontset-font fsn 'katakana-jisx0201 jp-fontspec) ; 半角カナ
    (set-fontset-font fsn '(#x0080 . #x024F) fontspec) ; 分音符付きラテン
    (set-fontset-font fsn '(#x0370 . #x03FF) fontspec) ; ギリシャ文字
    )

;;; デフォルトのフレームパラメータでフォントセットを指定
  (add-to-list 'default-frame-alist '(font . "fontset-myfonts"))

;;; フォントサイズの比を設定
  (dolist (elt '(("^-apple-hiragino.*" . 1.2)
                 (".*osaka-bold.*" . 1.2)
                 (".*osaka-medium.*" . 1.2)
                 (".*courier-bold-.*-mac-roman" . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman" . 0.9)))
    (add-to-list 'face-font-rescale-alist elt))
  
;;; デフォルトフェイスにフォントセットを設定
;;; (これは起動時に default-frame-alist に従ったフレームが作成されない現象への対処)
  (set-face-font 'default "fontset-myfonts"))

(setq ring-bell-function 'ignore)

; C-hでBackSpace
;;(keyboard-translate ?\C-h ?\C-?)
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)

(global-set-key (kbd "s-t") 'next-multiframe-window)
(global-set-key (kbd "s-r") 'rename-buffer)

;; align
(global-set-key (kbd "s-l") 'align-regexp)
(global-set-key (kbd "s-;") 'align-colon)
(global-set-key (kbd "s-:") 'align-colon)
(global-set-key (kbd "s-=") 'align-eq)

(defun align-string (BEG END text)
  (align-regexp BEG END (concat "\\(\\s-*\\)" text) 1 1))

(defun align-colon (BEG END)
  (interactive "r")
  (align-string BEG END ":"))

(defun align-eq (BEG END)
  (interactive "r")
  (align-string BEG END "="))

(defun upcase-backword ()
  (interactive)
  (upcase-word -1))
(global-set-key (kbd "M-U") 'upcase-backword)

(defun capitalize-backword ()
  (interactive)
  (capitalize-word -1))
(global-set-key (kbd "M-C") 'capitalize-backword)

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

;; coffee → js用 auto-revert-mode
(setq auto-revert-interval 1)


;;(add-to-list 'load-path "~/.emacs.d/auto-install/") 
;; auto-install
;; (when (require 'auto-install nil t)
;;   ;(auto-install-update-emacswiki-package-name t)
;;   ;(auto-install-compatibility-setup)
;;   )

;; helm でc-hできるようにする
(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'delete-backward-char)))
(eval-after-load 'helm-files
  '(progn
     (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)))

(when (require 'helm-config nil t)
  (global-set-key (kbd "C-c h") 'helm-mini)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  )

(when (require 'open-junk-file nil t)
  (global-set-key (kbd "C-x C-z") 'open-junk-file))

(when (require 'lispxmp nil t)
  (define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp))

(when (require 'paredit nil t)
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'ielm-mode-hook 'enable-paredit-mode))

(when (require 'auto-async-byte-compile nil t)
  (setq auto-async-bte-compile-exclude-files-regexp "/junk/")
  (add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
  (setq eldoc-idle-delay 0.2)
  (setq eldoc-minor-mode-string ""))

(global-set-key "\C-m" 'newline-and-indent)

(find-function-setup-keys)

