;;
;; Jani-Hur's GNU Emacs initialization file
;; ========================================
;;
;; Linux
;; -----
;;
;; ~/.emacs.d/init.el
;;
;; Windows 10+
;; -----------
;;
;; C:\Users\$env:USERNAME\AppData\Roaming\.emacs.d\init.el
;; or
;; $env:USERPROFILE\AppData\Roaming\.emacs.d\init.el
;;

;;
;; How to install packages manually:
;; M-x list-packages
;;

;;
;; MELPA (https://melpa.org/#/getting-started)
;;
;; Prefer stable (https://stable.melpa.org/#/) when possible
;;

(require 'package)

;; Note: There are some problems using the https location with Emacs
;; on Windows. There is currently no know easy fix for this. You can
;; still use MELPA by using the non-SSL location by replacing https
;; with http.
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; https://emacs.stackexchange.com/a/2989
(setq package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melpa"        . 0)))

;; http://irreal.org/blog/?p=3486
;; 1) build-in available in 29.1
;;    https://www.gnu.org/software/tramp/#index-method-docker
;;    https://www.emacswiki.org/emacs/TrampAndDocker
(setq package-pinned-packages
      '((basic-mode    . "melpa-stable")
        ;(d-mode        . "melpa-stable")
        (docker-tramp  . "melpa-stable") ; see 1) above
        (dracula-theme . "melpa-stable")
        (markdown-mode . "melpa-stable")
        (powershell    . "melpa")
        ;(protobuf-mode . "melpa-stable")
        ;(v-mode        . "melpa")
        (web-mode      . "melpa-stable")
        (xonsh-mode    . "melpa")
        (yaml-mode     . "melpa-stable")))

(package-initialize)

;; install all packages
(dolist (elem package-pinned-packages)
  (let ((package (car elem)))
    (unless (package-installed-p package)
      (package-refresh-contents)
      (package-install package))
    )
  )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" default))
 '(package-selected-packages
   '(docker-tramp basic-mode dracula-theme yaml-mode markdown-mode)))

;;
;; Load theme: https://draculatheme.com/emacs
;;

(load-theme 'dracula t)

;;
;; Package setup: basic-mode
;;

(setq basic-auto-number 10)
(setq basic-indent-offset 2)
(setq basic-line-number-cols 4)

;;
;; Package setup: markdown-mode
;;

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Package setup: v-mode
;; https://github.com/damon-kwok/v-mode
;;

; remove verilog mode from the auto mode list first
; https://stackoverflow.com/a/11633097/272735
;; (rassq-delete-all 'verilog-mode auto-mode-alist)
;; (require 'v-mode)
;; (add-to-list 'auto-mode-alist '("\\.v\\'" . v-mode))
;; (define-key v-mode-map (kbd "M-z") 'v-menu)
;; ;(define-key v-mode-map (kbd "<f6>")  'v-menu)
;; (define-key v-mode-map (kbd "C-c C-f") 'v-format-buffer)

;;
;; Package setup: web-mode
;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode)) ; nunjucks template

;;
;; Package setup: whitespace-mode
;;

(add-hook 'whitespace-mode-hook
          (lambda ()
            (setq whitespace-style (remove 'lines whitespace-style))))

;;
;; Package setup: yaml-mode
;;

(require 'yaml-mode)

;;
;; Use % to match various kinds of brackets (from Perl Best Practices by
;; Damian Conway)
;;

(defun match-paren (arg)
  ""
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))))

(global-set-key "%" 'match-paren)

;;
;; Scroll in place
;; (from http://www.emacswiki.org/cgi-bin/wiki/McMahanEmacsMacros)
;;

;; (defun scroll-down-in-place (n)
;;   (interactive "p")
;;   (previous-line n)
;;   (scroll-down n))

;; (defun scroll-up-in-place (n)
;;   (interactive "p")
;;   (next-line n)
;;   (scroll-up n))

;; (global-set-key (kbd "M-down") 'scroll-up-in-place)
;; (global-set-key [M-up] 'scroll-down-in-place)

;; scroll only one line at time and 5 lines with shift
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5)))

;;
;; Never indent with a tab character (good except with Python)
;; (e.g. http://emacsblog.org/2007/09/30/quick-tip-spaces-instead-of-tabs/)
;; to insert a real tab character: C-q C-i
;;

(setq-default indent-tabs-mode nil)

;;
;; Show matching parenthesis (parens).
;;

(show-paren-mode 1)
(setq show-paren-delay 0.5)
(setq show-paren-style "mixed")

;;
;; Remove duplicate lines in a region - keep the first occurence
;; http://emacswiki.org/emacs/DuplicateLines
;;

(defun uniquify-all-lines-region (start end)
  "Find duplicate lines in region START to END keeping first occurrence."
  (interactive "*r")
  (save-excursion
    (let ((end (copy-marker end)))
      (while
          (progn
            (goto-char start)
            (re-search-forward "^\\(.*\\)\n\\(\\(.*\n\\)*\\)\\1\n" end t))
        (replace-match "\\1\n\\2")))))

;;
;; https://www.emacswiki.org/emacs/IncrementNumber
;;

(defun increment-number-at-point ()
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

;;
;; saved keyboard macros
;; https://emacs.stackexchange.com/a/71/31440
;;

(fset 'increment-pom-patch-version
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ("<version>[4~ODODODODODODODODODODODxinsert	nu	c	m	num	s" 0 "%d")) arg)))

;;
;; Global key bindings
;;
;; Tip: use "C-h k" to find out key binding string
;;

(global-set-key (kbd "å") 'dabbrev-expand)
(global-set-key (kbd "M-g") 'goto-line) ; instead of set-face ... prefix
(global-set-key (kbd "<f5>") 'revert-buffer) ; as re-fresh in windows
(global-set-key (kbd "<C-tab>") 'other-window) ; instead of C-x o
(global-set-key (kbd "<backtab>") 'other-window) ; instead of C-x o

; inspired by http://xahlee.org/emacs/
(global-set-key (kbd "M-0") 'delete-window) ; instead of C-x 0
(global-set-key (kbd "M-1") 'delete-other-windows) ; instead of C-x 1
(global-set-key (kbd "M-2") 'split-window-vertically) ; instead of C-x 2
(global-set-key (kbd "M-b") 'switch-to-buffer) ; instead of C-x b
(global-set-key (kbd "M-s") 'save-buffer) ; instead of C-x C-s

;;
;; Global value for automatic line-wrapping.
;;

(setq-default fill-column 78)

;;
;; Turn on column mode so we'll see also column number in the mode line.
;;

(column-number-mode 1)

;;
;; Display time (in 24hr format) in mode line.
;;

(display-time)
(setq display-time-24hr-format t)

;;
;; Turn off unnecessary bars
;;

(menu-bar-mode 0)
; tool and scroll bars are not available in terminal-mode
; TODO: consider display-graphic-p, but this will do for now
(when (fboundp 'tool-bar-mode) (tool-bar-mode 0))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

;;
;; Visible bell instead of audible
;;

(setq visible-bell t)

;;
;; Default to better frame titles
;;

(setq frame-title-format
      (format "%%b - %%I %%n - %s"
              (downcase (car (split-string (system-name) "\\.")))))

;;
;; Saving Emacs Sessions
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;;

;(desktop-save-mode 1)

;;
;; Run server for emacsclients
;; But only if not yet started (https://stackoverflow.com/a/9999774/272735)
;;

(if (and (fboundp 'server-running-p)
         (not (server-running-p)))
    (server-start))

;;
;; *scratch* buffer
;;

;; default mode
;; https://emacs.stackexchange.com/a/53877/31440
(setq initial-major-mode 'fundamental-mode)

;; disable the scratch message
;; https://stackoverflow.com/q/1498258/272735
(setq initial-scratch-message nil)

;;
;; Inhibit the startuo screen
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html#index-inhibit_002dstartup_002dscreen
;;

(setq inhibit-startup-screen t)

;;
;; In some environments lock files are not removed correctly
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Interlocking.html#Interlocking
;;

(setq create-lockfiles nil)

;;
;; preferred coding system
;; https://emacs.stackexchange.com/a/44540/31440
;;

(setq-default buffer-file-coding-system 'utf-8-unix)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
