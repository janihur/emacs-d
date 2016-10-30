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
;; OS X
;; ----
;;
;; OS X Emacs is Aquamacs:
;;
;; http://aquamacs.org/
;; http://www.emacswiki.org/emacs/AquamacsEmacs
;; http://www.emacswiki.org/emacs/CustomizeAquamacs
;;
;; Note that Aquamacs ignores the traditional location and uses Apple specific
;; directory:
;;
;; ~/Library/Preferences/Aquamacs Emacs
;;
;; When using Aquamacs load this file by adding a line:
;;
;; (load "~/.emacs.d/init")
;;
;; to a file:
;;
;; ~/Library/Preferences/Aquamacs Emacs/Preferences.el
;;

;;
;;
;;

(package-initialize)
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'plsql)
(setq plsql-indent 2)

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
(global-set-key (kbd "C-\\") 'set-mark-command) ; C-§

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

;(menu-bar-mode 0)
;(tool-bar-mode 0)
;(scroll-bar-mode -1)

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
