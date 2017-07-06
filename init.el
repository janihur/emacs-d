;;
;; Jani-Hur's GNU Emacs initialization file
;; ========================================
;;
;; Linux
;; -----
;;
;; ~/.emacs.d/init.el
;;
;; Windows 10
;; ----------
;;
;; C:\Users\<USERNAME>\AppData\Roaming\.emacs.d\init.el
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
(setq package-pinned-packages
      '((markdown-mode . "melpa-stable")
        (plsql         . "melpa") ; not available in stable
        (web-mode      . "melpa-stable")
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
 '(package-selected-packages (quote (yaml-mode markdown-mode plsql))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;
;; Package setup: markdown-mode
;;

(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;
;; Package setup: plsql-mode
;;

(require 'plsql)

(setq plsql-indent 2)

(add-to-list 'auto-mode-alist '("\\.obs\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.obb\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.pks\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.pkb\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.tps\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.tpb\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("\\.pck\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("_PKG\\.sql\\'" . plsql-mode))
(add-to-list 'auto-mode-alist '("_pkg\\.sql\\'" . plsql-mode))

(defun plsql-find-alternate-package-file ()
  "Switch between package specification and body files. (Idea copied from tuareg mode.)"
  (interactive)
  (let ((name (buffer-file-name)))
    (cond
     ((string-match "\\`\\(.*\\)\\.pks\\'" name)
      (find-file (concat (match-string 1 name) ".pkb")))
     ((string-match "\\`\\(.*\\)\\.pkb\\'" name)
      (find-file (concat (match-string 1 name) ".pks")))

     ((string-match "\\`\\(.*\\)\\.obs\\'" name)
      (find-file (concat (match-string 1 name) ".obb")))
     ((string-match "\\`\\(.*\\)\\.obb\\'" name)
      (find-file (concat (match-string 1 name) ".obs")))

     ((string-match "\\`\\(.*\\)\\.tps\\'" name)
      (find-file (concat (match-string 1 name) ".tpb")))
     ((string-match "\\`\\(.*\\)\\.tpb\\'" name)
      (find-file (concat (match-string 1 name) ".tps")))
     )
    )
  )

(add-hook 'plsql-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-a") 'plsql-find-alternate-package-file)))

;;
;; Package setup: web-mode
;;

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.njk\\'" . web-mode)) ; nunjucks template

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
;; Global key bindings
;;
;; Tip: use "C-h k" to find out key binding string
;;

(global-set-key (kbd "å") 'dabbrev-expand)
(global-set-key (kbd "M-g") 'goto-line) ; instead of set-face ... prefix
(global-set-key (kbd "<f5>") 'revert-buffer) ; as re-fresh in windows

; inspired by http://xahlee.org/emacs/
(global-set-key (kbd "M-0") 'delete-window) ; C-x 0
(global-set-key (kbd "M-1") 'delete-other-windows) ; C-x 1
(global-set-key (kbd "M-2") 'split-window-vertically) ; C-x 2
(global-set-key (kbd "M-b") 'switch-to-buffer) ; C-x b
(global-set-key (kbd "M-s") 'save-buffer) ; C-x C-s

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
      (concat "%b - " 
              (downcase (car (split-string system-name "\\.")))
              ))

;;
;; Saving Emacs Sessions
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Saving-Emacs-Sessions.html
;;

;(desktop-save-mode 1)

;;
;; Run server for emacsclients
;;

(server-start)

;;
;; How to change the scratch message
;; http://stackoverflow.com/questions/1498258/how-do-i-change-the-scratch-message-in-emacs
;;

(setq initial-scratch-message "")
