;; -*- lexical-binding: t -*-

;;;; custom-vars
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector ["black" "light gray" "dark gray" "light slate gray"])
 '(blink-cursor-mode t)
 '(c-basic-offset 4)
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(compilation-scroll-output t)
 '(css-indent-offset 2)
 '(cua-global-mark-cursor-color "#3fc5b7")
 '(cua-normal-cursor-color "#b9b9b9")
 '(cua-overwrite-cursor-color "#dbb32d")
 '(cua-read-only-cursor-color "#70b433")
 '(dired-auto-revert-buffer t)
 '(dired-dwim-target t)
 '(dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|\\.DS_Store")
 '(dired-omit-verbose nil)
 '(dired-recursive-copies 'always)
 '(dired-recursive-deletes 'always)
 '(fci-rule-color "#383838" t)
 '(frame-brackground-mode 'dark)
 '(highlight-changes-colors '("#eb6eb7" "#a580e2"))
 '(highlight-symbol-colors
   '("#47853c9c20a0" "#2f44456c3cd2" "#50542e552bcb" "#3d5b34a0453f" "#2d9a3d252c44" "#46c034a72c08" "#2ec7343e4551"))
 '(highlight-symbol-foreground-color "#dedede")
 '(highlight-tail-colors
   '(("#252525" . 0)
     ("#428800" . 20)
     ("#009789" . 30)
     ("#0057af" . 50)
     ("#a68000" . 60)
     ("#aa4b11" . 70)
     ("#ac357c" . 85)
     ("#252525" . 100)))
 '(hl-bg-colors
   '("#a68000" "#aa4b11" "#ad1117" "#ac357c" "#6945a5" "#0057af" "#009789" "#428800"))
 '(hl-fg-colors
   '("#181818" "#181818" "#181818" "#181818" "#181818" "#181818" "#181818" "#181818"))
 '(hl-paren-colors '("#3fc5b7" "#dbb32d" "#368aeb" "#a580e2" "#70b433"))
 '(hl-sexp-background-color "#33323e")
 '(ibuffer-deletion-face 'dired-flagged)
 '(ibuffer-marked-face 'dired-marked)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(js-indent-level 2)
 '(ls-lisp-dirs-first t)
 '(ls-lisp-use-insert-directory-program nil)
 '(menu-bar-mode nil)
 '(org-agenda-files nil t)
 '(org-fontify-whole-heading-line t)
 '(outline-minor-mode-prefix "h")
 '(package-archives
   '(("gnu" . "https://elpa.gnu.org/packages/")
     ("orgmode" . "https://orgmode.org/elpa/")
     ("melpa" . "https://melpa.org/packages/")))
 '(package-check-signature nil)
 '(package-native-compile t)
 '(ring-bell-function 'ignore)
 '(sgml-basic-offset 2)
 '(show-paren-mode t)
 '(recentf-exclude '("/tmp" "/ssh:" "\\ido.last" "recentf"))
 '(recentf-keep '(file-remote-p file-readable-p))
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 100)
 '(recentf-mode t)
 '(recentf-save-file "~/.emacs.d/etc/recentf")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#70b433" "#252525" 0.2))
 '(tab-width 4)
 '(term-default-bg-color "#181818")
 '(term-default-fg-color "#b9b9b9")
 '(tls-program '("gnutls-cli --insecure -p %p %h"))
 '(tool-bar-mode nil)
 '(undo-limit 20000000)
 '(undo-strong-limit 40000000)
 '(vc-annotate-background "#2B2B2B")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#BC8383")
     (40 . "#CC9393")
     (60 . "#DFAF8F")
     (80 . "#D0BF8F")
     (100 . "#E0CF9F")
     (120 . "#F0DFAF")
     (140 . "#5F7F5F")
     (160 . "#7F9F7F")
     (180 . "#8FB28F")
     (200 . "#9FC59F")
     (220 . "#AFD8AF")
     (240 . "#BFEBBF")
     (260 . "#93E0E3")
     (280 . "#6CA0A3")
     (300 . "#7CB8BB")
     (320 . "#8CD0D3")
     (340 . "#94BFF3")
     (360 . "#DC8CC3")))
 '(vc-annotate-very-old-color "#DC8CC3")
 '(visible-bell t)
 '(weechat-color-list
   '(unspecified "#272822" "#3C3D37" "#F70057" "#F92672" "#86C30D" "#A6E22E" "#BEB244" "#E6DB74" "#40CAE4" "#66D9EF" "#FB35EA" "#FD5FF0" "#74DBCD" "#A1EFE4" "#F8F8F2" "#F8F8F0"))
 '(window-divider-default-right-width 1)
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#000000" "#a80000" "#005200" "#8b3800" "#0030a6" "#721045" "#005589" "#f3f1f3"])
 '(xterm-color-names-bright
   ["#505050" "#880000" "#4a5700" "#714900" "#223fbf" "#8f0075" "#185870" "#ffffff"])
 '(xterm-mouse-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 150 :family "Dejavu Sans Mono")))))

;;;; system-packages
(eval-after-load 'package
  (progn
    (package-initialize)

    (unless package-archive-contents
      (package-refresh-contents))

    '(dolist (p package-selected-packages)
      (unless (package-installed-p p)
        (progn
          (package-install p))))))

;;;; guix-emacs-packages
(load-file (expand-file-name "~/.guix-home/profile/share/emacs/site-lisp/subdirs.el"))
(guix-emacs-autoload-packages)

;;;; initialization
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq-default ns-use-srgb-colorspace nil)
(setq warning-minimum-level              :error
      confirm-nonexistent-file-or-buffer nil
      ffap-machine-p-known               'reject)

(defun ers/make-path (root-dir &rest path-elements)
  (let ((new-path          (expand-file-name (if (listp root-dir)
                                                 (car root-dir)
                                               root-dir)))
        (new-path-elements (if (listp root-dir)
                               (rest root-dir)
                             path-elements)))
    (dolist (p new-path-elements)
      (setq new-path (concat (file-name-as-directory new-path) p)))
    new-path))

(defun ers/emacs-dir-path (&rest path-elements)
  (apply 'ers/make-path
         (cons user-emacs-directory path-elements)))

(defun ers/emacs-etc-path (&rest path-elements)
  (apply 'ers/emacs-dir-path (cons "etc" path-elements)))

(defun ers/mkdir-p (dir-path)
  (unless (file-exists-p dir-path)
    (make-directory dir-path t)))

(global-unset-key (kbd "C-j"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-t"))

(add-hook 'after-init-hook #'auto-compression-mode)

;;;; utf-8
(defun ers/setup-utf8 ()
  (interactive)
  (prefer-coding-system 'utf-8)
  (setq-default buffer-file-coding-system 'utf-8)
  (setq coding-system-for-write 'utf-8
        coding-system-for-read  'utf-8
        file-name-coding-system 'utf-8
        locale-coding-system    'utf-8)
  (set-language-environment     'utf-8)
  (set-default-coding-systems   'utf-8)
  (set-terminal-coding-system   'utf-8)
  (set-keyboard-coding-system   'utf-8)
  (set-selection-coding-system  'utf-8)
  (set-language-environment     'utf-8))

(add-hook 'after-init-hook #'ers/setup-utf8)

;;;; backups
(setq make-backup-files nil)

;;;; utilities
(fset 'yes-or-no-p 'y-or-n-p)

(defun ers/comment-or-uncomment-line-or-region ()
  "Comment or uncomment the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun ers/do-with-symbol-at-point-bounds (cb-fn)
  "Do something with the bounds of the symbol at point"
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (funcall cb-fn (car bounds) (cdr bounds)))))

(defun ers/indent-region-or-buffer ()
  "Indents an entire buffer using the default intenting scheme."
  (interactive)
  (let ((coords (if (region-active-p)
                    (list (region-beginning) (region-end))
                  (list (point-min) (point-max)))))
    (indent-region (car coords) (car (last coords)))
    (delete-trailing-whitespace (point-min) (point-max))
    (untabify (point-min) (point-max))))

;;;; eshell
(eval-after-load 'eshell
  (progn
    (setq eshell-highlight-prompt       nil
          eshell-history-size           8000
          eshell-path-env               (getenv "PATH")
          eshell-cmpl-cycle-completions nil)

    (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
    (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)
    (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)

    '(defun eshell/up (&optional level)
       "Change directory from one up to a level of folders."
       (let ((path-level (or level 1)))
         (cd (apply 'concat (make-list path-level "../")))))))

;;;; ansi
(eval-after-load 'ansi-color
  (progn
    (autoload 'ansi-color-apply-on-region "ansi-color" "ansi colors" t nil)

    (defun ers/colorize-compilation-buffer ()
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region (point-min)
                                    (point-max))))

    '(add-hook 'compilation-filter-hook #'ers/colorize-compilation-buffer)))

;;;; hippie-expand
(eval-after-load 'hippie-exp
  (progn
    (setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                             try-expand-dabbrev-all-buffers
                                             try-expand-dabbrev-from-kill
                                             try-complete-file-name-partially
                                             try-complete-file-name
                                             try-expand-all-abbrevs
                                             try-expand-list
                                             try-expand-line
                                             try-complete-lisp-symbol-partially
                                             try-complete-lisp-symbol))
    '(global-set-key (kbd "M-/") #'hippie-expand)))

;;;; line-operations
(defun ers/duplicate-line ()
  "Duplicate line."
  (interactive)
  (progn
    (beginning-of-line)
    (insert (thing-at-point 'line))
    (back-to-indentation)))

(defun move-text-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-text-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(global-set-key (kbd "M-P")     #'move-text-line-up)
(global-set-key (kbd "M-N")     #'move-text-line-down)
(global-set-key (kbd "C-c d")   #'ers/duplicate-line)
(global-set-key (kbd "C-x M-o") #'join-line)
(global-set-key (kbd "C-x C-o") #'delete-blank-lines)

;;;; transpose
(defun ers/transpose-windows ()
  "Transpose two windows.  If more or less than two windows are visible, error."
  (interactive)
  (unless (= 2 (count-windows))
    (user-error "Can only transpose exactly 2 windows."))
  (let* ((windows (window-list))
         (w1      (car windows))
         (w2      (nth 1 windows))
         (w1b     (window-buffer w1))
         (w2b     (window-buffer w2)))
    (set-window-buffer w1 w2b)
    (set-window-buffer w2 w1b)))

(global-unset-key (kbd "C-x C-t"))

(global-set-key (kbd "C-x C-t c") #'transpose-chars)
(global-set-key (kbd "C-x C-t l") #'transpose-lines)
(global-set-key (kbd "C-x C-t s") #'transpose-sexps)
(global-set-key (kbd "C-x C-t w") #'ers/transpose-windows)

;;;; things-at-point
(defun ers/cut-symbol-at-point ()
  "Cut the symbol at point."
  (interactive)
  (ers/do-with-symbol-at-point-bounds 'kill-region))

(defun ers/copy-symbol-at-point ()
  "Copy the symbol at point."
  (interactive)
  (progn (setq kill-ring nil) (garbage-collect))
  (ers/do-with-symbol-at-point-bounds 'kill-ring-save))

(defun ers/mark-symbol-at-point ()
  "Mark symbol at point."
  (interactive)
  (ers/do-with-symbol-at-point-bounds #'(lambda (start end)
                                          (goto-char start)
                                          (set-mark-command nil)
                                          (goto-char end))))

(defun ers/align-and-indent ()
  (interactive)
  (progn
    (newline)
    (indent-according-to-mode)))

(global-set-key (kbd "C-h C-w") #'ers/cut-symbol-at-point)
(global-set-key (kbd "C-h M-w") #'ers/copy-symbol-at-point)
(global-set-key (kbd "C-c m-s") #'ers/mark-symbol-at-point)

;;;; general
(global-set-key (kbd "C-c m l") (kbd "C-a C-@ C-e"))
(global-set-key (kbd "C-c m k") (kbd "C-a C-u 1 C-k"))
(global-set-key (kbd "C-x C-r") #'query-replace)
(global-set-key (kbd "C-c /")   #'ers/comment-or-uncomment-line-or-region)

(global-set-key (kbd "C-c \\") #'ers/indent-region-or-buffer)
(global-set-key (kbd "C-c ar") #'align-regexp)
(global-set-key (kbd "RET")    #'ers/align-and-indent)

;;;; window-management
(eval-after-load 'window
  (progn
    (fset 'scroll-other-window-up   #'scroll-other-window-down)
    (global-set-key (kbd "C-M-y")   #'scroll-other-window-up)
    '(global-set-key (kbd "C-M-v")  #'scroll-other-window)))

(eval-after-load 'windmove
  (progn
    (global-set-key (kbd "C-c wn")  #'windmove-up)
    (global-set-key (kbd "C-c ws")  #'windmove-down)
    (global-set-key (kbd "C-c we")  #'windmove-right)
    '(global-set-key (kbd "C-c ww") #'windmove-left)))

;;;; buffers
(defun ers/kill-buffer-no-confirm ()
  "Kill buffer without confirmation."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(defun ers/revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))

(global-set-key (kbd "C-h [")   #'next-buffer)
(global-set-key (kbd "C-h ]")   #'previous-buffer)
(global-set-key (kbd "C-h C-r") #'ers/revert-buffer-no-confirm)
(global-set-key (kbd "C-x M-k") #'ers/kill-buffer-no-confirm)

;;;; dired
(eval-after-load 'dired
  (progn
    (defun ers/dired-setup ()
      (interactive)
      (mapc 'require '(dired-x ls-lisp))
      (dired-hide-details-mode 1)
      (define-key dired-mode-map "." #'dired-up-directory))

    '(add-hook 'dired-mode-hook 'ers/dired-setup)))

;;;; ffap
(global-set-key (kbd "C-c f.") #'ffap)

;;;; prog-modes
(add-hook 'prog-mode-hook #'electric-pair-local-mode)

;;;; registers
(set-register ?i `(file . ,user-init-file))

;;;; movement
(global-set-key (kbd "M-p") (kbd "C-u 10 C-p"))
(global-set-key (kbd "M-n") (kbd "C-u 10 C-n"))

;;;; others
(add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

(global-set-key (kbd "C-x C-m") #'execute-extended-command)
(global-set-key (kbd "M-s l")   #'goto-line)
(global-set-key (kbd "M-s e")   #'eshell)
(global-set-key (kbd "M-s j")   #'avy-goto-char)
(global-set-key (kbd "M-s s")   #'grep-find)
(global-set-key (kbd "M-s o")   #'occur)
(global-set-key (kbd "C-x o")   #'other-window)
(global-set-key (kbd "M-s r")   #'recentf-open-files)
(global-set-key (kbd "M-z")     #'zap-to-char)
(global-set-key (kbd "M-Z")     #'zap-up-to-char)
(global-set-key (kbd "M-\"")    #'xref-find-apropos)
(global-set-key (kbd "M-s i")   #'imenu)

;;;; disabled-commands
(put 'erase-buffer     'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'downcase-region  'disabled nil)
(put 'upcase-region    'disabled nil)

;;;; terminal-mouse-scroll
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") #'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") #'scroll-up-line))

;;;; xclip
(eval-after-load 'xclip
  (progn
    (require 'xclip)
    '(add-hook 'after-init-hook #'xclip-mode)))

;;;; theme
(load-theme 'rimero-dark t)
