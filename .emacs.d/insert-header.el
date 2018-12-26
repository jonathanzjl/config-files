;; Copyright (C) 2018  Zhijin Li

;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:

;;     * Redistributions of source code must retain the above copyright
;; notice, this list of conditions and the following disclaimer.
;;     * Redistributions in binary form must reproduce the above
;; copyright notice, this list of conditions and the following disclaimer
;; in the documentation and/or other materials provided with the
;; distribution.

;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;; A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;; OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;; SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
;; LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; ---------------------------------------------------------------------------
;;
;; File: insert-header.el<config-files> for config-files
;;
;; Created by Zhijin Li
;; E-mail:   <jonathan.zj.lee@gmail.com>
;;
;; Started on  Sun Sep  9 21:05:27 2018 Zhijin Li
;; Last update Wed Dec 26 14:15:31 2018 Zhijin Li
;; ---------------------------------------------------------------------------


(require 'lice)

(setq header-created-by	"Created by "
      header-login	"E-mail:   "
      header-login-beg	"<"
      header-login-mid	"@"
      header-login-end	">"
      header-started	"Started on  "
      header-last	"Last update "
      header-for	" for "
      header-in		" in "
      header-by		" by "
      header-line	" ---------------------------------------------------------------------------")


(if (setq user-nickname (getenv "USER_NICKNAME"))
    t
  (setq user-nickname (user-full-name)))

(setq write-file-hooks (cons 'update-std-header write-file-hooks))

(setq
 c-cmnt-alist    '( (cs . "/*") (cc . " *") (ce . "*/") )
 c++-cmnt-alist  '( (cs . "//") (cc . "//") (ce . "//") )
 java-cmnt-alist '( (cs . "//") (cc . "//") (ce . "//") )
 py-cmnt-alist   '( (cs . "#! /usr/bin/env python3")  (cc . "##")(ce . "##") )
 sh-cmnt-alist   '( (cs . "#! /bin/bash")  (cc . "##") (ce . "##") )
 mk-cmnt-alist   '( (cs . "##") (cc . "##") (ce . "##") )
 tex-cmnt-alist  '( (cs . "%%") (cc . "%%") (ce . "%%") )
 js-cmnt-alist   '( (cs . "//") (cc . "//") (ce . "//") )
 html-cmnt-alist '( (banner . "<!DOCTYPE html>") (cs . "<!--") (cc . "  --") (ce . "-->"))
 css-cmnt-alist  '( (cs . "/*") (cc . " *") (ce . "*/") )
 lisp-cmnt-alist '( (cs . ";;") (cc . ";;") (ce . ";;") )
 fund-cmnt-alist '( (cs . "##") (cc . "##") (ce . "##") )
 )

(setq major-mode-alist
      '(
	(c-mode                    . c-cmnt-alist)
        (c++-mode                  . c++-cmnt-alist)
	(java-mode             	   . java-cmnt-alist)
        (python-mode               . py-cmnt-alist)
        (sh-mode                   . sh-cmnt-alist)
        (makefile-bsdmakefile-mode . mk-cmnt-alist)
        (makefile-automake-mode    . mk-cmnt-alist)
	(latex-mode                . tex-cmnt-alist)
        (js-mode                   . js-cmnt-alist)
	(mhtml-mode                . html-cmnt-alist)
	(css-mode                  . css-cmnt-alist)
        (lisp-mode                 . lisp-cmnt-alist)
        (emacs-lisp-mode           . lisp-cmnt-alist)
        (lisp-interaction-mode     . lisp-cmnt-alist)
        (fundamental-mode          . fund-cmnt-alist)
        ))


(defun std-get (a)
  (interactive)
  (cdr (assoc a (eval (cdr (assoc major-mode major-mode-alist))))))


(defun update-std-header ()
  "Updates std header with last modification time & owner.\n(According to mode)"
  (interactive)
  (save-excursion
    (if (buffer-modified-p)
	(progn
	  (goto-char (point-min))
	  (if (search-forward header-last nil t)
	      (progn
		(delete-region
		 (progn (beginning-of-line) (point))
		 (progn (end-of-line) (point)))
		(insert (concat (std-get 'cc)
                                " "
				header-last
				(current-time-string)
				" "
				user-nickname))
		(message "Last modification header field updated.")))
          )))
  nil)


(defun std-file-header ()
  "Puts a standard header at the beginning of the file.\n(According to mode)"
  (interactive)
  (goto-char (point-min))
  (let ((projname "foo"))
    (setq projname (read-from-minibuffer
		    (format "Type project name (RETURN to quit) : ")))

    (if (eq major-mode 'mhtml-mode)
        (progn
          (insert (std-get 'banner))
          (newline)))

    (if (or (eq major-mode 'c-mode)
            (eq major-mode 'css-mode)
            (eq major-mode 'mhtml-mode)
            (equal (std-get 'cs) (std-get 'cc)))
        ()
      (progn
        (insert (std-get 'cs))
        (newline))
      )

    (call-interactively #'lice)
    (newline)

    (if (or (eq major-mode 'c-mode)
            (eq major-mode 'mhtml-mode)
            (eq major-mode 'css-mode))
        (progn
          (insert (std-get 'cs))
          (newline))
      )

    (insert (concat (std-get 'cc) header-line))
    (newline)
    (insert (std-get 'cc))
    (newline)
    (insert (concat (std-get 'cc)
                    " File: "
		    (buffer-name)
		    header-for
		    projname))
    (newline)
    (insert (std-get 'cc))
    (newline)
    (insert (concat (std-get 'cc) " " header-created-by user-full-name))
    (newline)
    (insert (concat (std-get 'cc)
                    " "
		    header-login
		    header-login-beg
		    user-mail-address
		    header-login-end))
    (newline)
    (insert (std-get 'cc))
    (newline)
    (insert (concat (std-get 'cc)
                    " "
		    header-started
		    (current-time-string)
		    " "
		    user-nickname))
    (newline)
    (insert (concat (std-get 'cc)
                    " "
		    header-last
		    (current-time-string)
		    " "
		    user-nickname))
    (newline)

    (insert (concat (std-get 'cc)
                    header-line))
    (newline)

    (if (equal (std-get 'cc) (std-get 'ce))
        ()
      (insert (std-get 'ce)))

    (newline)
    ))


(defun insert-std-vertical-comments ()
  "Inserts vertical comments (according to mode)."
  (interactive)
  (beginning-of-line)
  (insert (std-get 'cs))
  (newline)
  (let ((ok t)(comment ""))
    (while ok
      (setq comment (read-from-minibuffer
		     (format "Type comment (RETURN to quit) : ")))
      (if (= 0 (length comment))
	  (setq ok nil)
	(progn
	  (insert (concat (std-get 'cc) comment))
	  (newline)))))
  (insert (std-get 'ce))
  (newline))


(defun std-toggle-comment ()
  "Toggles line comment on or off (according to mode)."
  (interactive)
  (save-excursion
    (let (beg end)
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point))
      (save-restriction
	(if (not (equal beg end))
	    (progn
	      (narrow-to-region beg end)
	      (goto-char beg)
	      (if (search-forward (std-get 'cs) end t)
		  (progn
		    (beginning-of-line)
		    (replace-string (std-get 'cs) "")
		    (replace-string (std-get 'ce) ""))
		(progn
		  (beginning-of-line)
		  (insert (std-get 'cs))
		  (end-of-line)
		  (insert (std-get 'ce)))))))))
  (indent-for-tab-command)
  (next-line 1))


;;; Global key-bindings
(global-set-key "h" 'update-std-header)
(global-set-key "" 'std-file-header)

;;;; Generating local keymaps for special modes.

;;; In CPerl mode, C-c C-h is used to do some help.
;;; so it is C-c C-h h
(add-hook 'cperl-mode-hook
	  '(lambda ()
	     (define-key cperl-mode-map ""
	       'comment-region)
	     (define-key cperl-mode-map "h"
	       'std-file-header)))

;; for perl-mode
(add-hook 'perl-mode-hook
	  '(lambda ()
	     (define-key perl-mode-map ""
	       'comment-region)))

;; for all lisp codes
(add-hook 'lisp-interaction-mode-hook
 	  '(lambda ()
 	     (local-set-key  ""
                             'comment-region)))


;; for LaTeX-mode
(add-hook 'tex-mode-hook
	  '(lambda ()
	     (define-key tex-mode-map ""
	       'comment-region)))
