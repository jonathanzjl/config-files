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
;; Last update Fri Dec 28 18:59:51 2018 Zhijin Li
;; ---------------------------------------------------------------------------


(require 'lice)
(require 'cl-extra)


;; Find extension of current buffer.
(defun match-curr-buffer-ext (ext)
  "If current buffer matches input extension, return"
  "the extension. Otherwise return nil."
  (when (string-match
         (concat "\\." ext "~??$") buffer-file-name)
    ext))


(defun get-curr-buffer-ext (all-ext)
  "Get the matched file extension from the list of all"
  "extensions for current buffer."
  (cl-some #'match-curr-buffer-ext all-ext))


;; All file extensions
(setq ext-list
      '(
        "h"
        "c"
        "hh"
        "hxx"
        "hpp"
        "cc"
        "cxx"
        "cpp"
        "sh"
        "py"
        "js"
        "jsx"
        "html"
        "css"
        "el"
        "emacs"
        "m"
        ))


;; Header banner.
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


;; Get user nick name
(if (setq user-nickname (getenv "USER_NICKNAME")) t
  (setq user-nickname (user-full-name)))


;; Set-up update-when-writting.
(setq write-file-hooks (cons 'update-std-header write-file-hooks))


;; Specification of comment chars for diff modes.
(setq
 c-cmnt-alist    '( (cs . "/*") (cc . " *") (ce . "*/") )
 c++-cmnt-alist  '( (cs . "//") (cc . "//") (ce . "//") )
 java-cmnt-alist '( (cs . "//") (cc . "//") (ce . "//") )
 py-cmnt-alist   '( (cs . "#! /usr/bin/env python3")  (cc . "##")(ce . "##") )
 sh-cmnt-alist   '( (cs . "#! /bin/bash")  (cc . "##") (ce . "##") )
 mk-cmnt-alist   '( (cs . "##") (cc . "##") (ce . "##") )
 tex-cmnt-alist  '( (cs . "%%") (cc . "%%") (ce . "%%") )
 js-cmnt-alist   '( (cs . "//") (cc . "//") (ce . "//") )
 html-cmnt-alist '( (banner . "<!DOCTYPE html>") (cs . "<!--") (cc . "   --") (ce . "-->"))
 css-cmnt-alist  '( (cs . "/*") (cc . " *") (ce . "*/") )
 lisp-cmnt-alist '( (cs . ";;") (cc . ";;") (ce . ";;") )
 octave-cmnt-alist '( (cs . "%") (cc . "%") (ce . "%") )
 fund-cmnt-alist '( (cs . "##") (cc . "##") (ce . "##") )
 )


;; Mode and comment chars mapping.
(setq major-mode-alist
      '(
        ("h"     . c-cmnt-alist)
        ("c"     . c-cmnt-alist)
        ("hh"    . c++-cmnt-alist)
        ("hxx"   . c++-cmnt-alist)
        ("hpp"   . c++-cmnt-alist)
        ("cc"    . c++-cmnt-alist)
        ("cxx"   . c++-cmnt-alist)
        ("cpp"   . c++-cmnt-alist)
        ("java"  . java-cmnt-alist)
        ("sh"    . sh-cmnt-alist)
        ("py"    . py-cmnt-alist)
        ("js"    . js-cmnt-alist)
        ("jsx"   . js-cmnt-alist)
        ("html"  . html-cmnt-alist)
        ("css"   . css-cmnt-alist)
        ("el"    . lisp-cmnt-alist)
        ("emacs" . lisp-cmnt-alist)
        ("m"     . octave-cmnt-alist)
        (nil     . fund-cmnt-alist)
        ))


(defun std-get (a)
  "Get comment character strings."
  (interactive)
  (cdr (assoc a (eval (cdr (assoc
                            (get-curr-buffer-ext ext-list)
                            major-mode-alist))))))


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

    (if (string= (get-curr-buffer-ext ext-list) "html")
        (progn
          (insert (std-get 'banner))
          (newline)))

    (if (or
         (string= (get-curr-buffer-ext ext-list) "c")
         (string= (get-curr-buffer-ext ext-list) "h")
         (string= (get-curr-buffer-ext ext-list) "html")
         (string= (get-curr-buffer-ext ext-list) "css")
         (string= (std-get 'cs) (std-get 'cc)))
        ()
      (progn
        (insert (std-get 'cs))
        (newline))
      )

    (call-interactively #'lice)
    (newline)

    (if (or
         (string= (get-curr-buffer-ext ext-list) "c")
         (string= (get-curr-buffer-ext ext-list) "h")
         (string= (get-curr-buffer-ext ext-list) "html")
         (string= (get-curr-buffer-ext ext-list) "css")
         )
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
