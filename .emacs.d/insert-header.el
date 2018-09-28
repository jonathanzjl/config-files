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
;; Last update Fri Sep 28 23:11:41 2018 Zhijin Li
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
      header-line	" ---------------------------------------------------------------------------"
      domaine-name	"ge.com")

(defun year-string (year &optional (mode 'long))
  "Formats a year number in various ways."
  (when year
    (case mode
      (short (format nil "~A" (mod year 100)))
      (long (format nil "~A" year)))))


(if (setq user-nickname (getenv "USER_NICKNAME"))
    t
  (setq user-nickname (user-full-name)))

(setq write-file-hooks (cons 'update-std-header write-file-hooks))

(setq std-c-alist               '( (cs . "/*") (cc . "**") (ce . "*/") )
      std-autoconf-alist        '( (cs . "##") (cc . "##") (ce . "##") )
      std-css-alist             '( (cs . "/*") (cc . "**") (ce . "*/") )
      std-cpp-alist             '( (cs . "//") (cc . "//") (ce . "//") )
      std-pov-alist             '( (cs . "//") (cc . "//") (ce . "//") )
      std-java-alist            '( (cs . "//") (cc . "//") (ce . "//") )
      std-latex-alist		'( (cs . "%%") (cc . "%%") (ce . "%%") )
      std-lisp-alist            '( (cs . ";;") (cc . ";;") (ce . ";;") )
      std-xdefault-alist	'( (cs . "!!") (cc . "!!") (ce . "!!") )
      std-pascal-alist		'( (cs . "{ ") (cc . "  ") (ce . "}" ) )
      std-makefile-alist        '( (cs . "##") (cc . "##") (ce . "##") )
      std-text-alist		'( (cs . "##") (cc . "##") (ce . "##") )
      std-fundamental-alist     '( (cs . "##") (cc . "##") (ce . "##") )
      std-html-alist		'( (cs . "<!--") (cc . "  --") (ce . "-->"))
      std-nroff-alist		'( (cs . "\\\"") (cc . "\\\" ") (ce . "\\\""))
      std-sscript-alist         '( (cs . "#!/bin/bash")  (cc . "##") (ce . "##") )
      std-perl-alist            '( (cs . "#!/usr/bin/perl -w")  (cc . "##")(ce . "##") )
      std-python-alist          '( (cs . "#! /usr/bin/env python3")  (cc . "##")(ce . "##") )
      std-cperl-alist           '( (cs . "#!/usr/bin/perl -w")  (cc . "##")(ce . "##") ))

(setq std-modes-alist '(("Autoconf"             . std-autoconf-alist)
			("C/l"                	. std-c-alist)
			("CSS"                	. std-c-alist)
			("PoV"                	. std-pov-alist)
                        ("C++/l"              	. std-cpp-alist)
                        ("Lisp"             	. std-lisp-alist)
                        ("Lisp Interaction" 	. std-lisp-alist)
                        ("Emacs-Lisp"       	. std-lisp-alist)
                        ("Fundamental"      	. std-fundamental-alist)
                        ("Shell-script"     	. std-sscript-alist)
                        ("Python"     	        . std-python-alist)
                        ("GNUmakefile"         	. std-makefile-alist)
                        ("Makefile.am"         	. std-makefile-alist)
			("Perl"            	. std-cperl-alist)
			("CPerl"            	. std-cperl-alist)
			("xdefault"         	. std-xdefault-alist)
			("java"             	. std-java-alist)
			("latex"	    	. std-latex-alist)
			("Pascal"	    	. stdp-ascal-alist)
			("Text"	            	. std-text-alist)
			("HTML"	            	. std-html-alist)
			("Nroff"	    	. std-nroff-alist)
			("TeX"			. std-latex-alist)
			("LaTeX"		. std-latex-alist)))

(defun std-get (a)
  (interactive)
  (cdr (assoc a (eval (cdr (assoc mode-name std-modes-alist))))))

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
		(insert-string (concat (std-get 'cc)
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
  (let ((projname "toto")(location "titi"))
    (setq projname (read-from-minibuffer
		    (format "Type project name (RETURN to quit) : ")))
    (setq location (getenv "PWD"))


    (if (equal (std-get 'cs) (std-get 'cc))
        ()
      (progn
        (insert-string (std-get 'cs))
        (newline))
      )

    (call-interactively #'lice)
    (newline)

    (insert-string (concat (std-get 'cc)
        		   header-line))
    (newline)
    (insert-string (std-get 'cc))
    (newline)
    (insert-string (concat (std-get 'cc)
                           " File: "
			   (buffer-name)
			   header-for
			   projname))
    (newline)
    (insert-string (std-get 'cc))
    (newline)
    (insert-string (concat (std-get 'cc) " " header-created-by user-full-name))
    (newline)
    (insert-string (concat (std-get 'cc)
                           " "
			   header-login
			   header-login-beg
			   user-mail-address
			   header-login-end))
    (newline)
    (insert-string (std-get 'cc))
    (newline)
    (insert-string (concat (std-get 'cc)
                           " "
			   header-started
			   (current-time-string)
			   " "
			   user-nickname))
    (newline)
    (insert-string (concat (std-get 'cc)
                           " "
			   header-last
			   (current-time-string)
			   " "
			   user-nickname))
    (newline)

    (insert-string (concat (std-get 'cc)
                           header-line))
    (newline)

    (if (equal (std-get 'cc) (std-get 'ce))
        (nil)
      (insert-string (std-get 'ce)))

    (newline)
    ))

(defun insert-std-vertical-comments ()
  "Inserts vertical comments (according to mode)."
  (interactive)
  (beginning-of-line)
  (insert-string (std-get 'cs))
  (newline)
  (let ((ok t)(comment ""))
    (while ok
      (setq comment (read-from-minibuffer
		     (format "Type comment (RETURN to quit) : ")))
      (if (= 0 (length comment))
	  (setq ok nil)
	(progn
	  (insert-string (concat (std-get 'cc) comment))
	  (newline)))))
  (insert-string (std-get 'ce))
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
		  (insert-string (std-get 'cs))
		  (end-of-line)
		  (insert-string (std-get 'ce)))))))))
  (indent-for-tab-command)
  (next-line 1))

;;; Added by Eole Wednesday May 29 2002,  1:33:55
;;; Extended bindings for this package and for commenting code

(global-set-key "h" 'update-std-header)
(global-set-key "" 'std-file-header)

;;;; Generating local keymaps for exotics modes.

;;; In CPerl mode, C-c C-h is used to do some help.
;;; so it is C-c C-h h
;;; For working, it requires info pages about perl
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

;; for all kind of lisp code
(add-hook 'lisp-interaction-mode-hook
 	  '(lambda ()
 	     (local-set-key  ""
                             'comment-region)))


;; for La(TeX)-mode
(add-hook 'tex-mode-hook
	  '(lambda ()
	     (define-key tex-mode-map ""
	       'comment-region)))
