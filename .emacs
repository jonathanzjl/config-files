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
;; File: .emacs<config-files> for config-files
;;
;; Created by Zhijin Li
;; E-mail:   <jonathan.zj.lee@gmail.com>
;;
;; Started on  Sun Sep  9 21:13:06 2018 Zhijin Li
;; Last update Fri Dec 28 18:59:15 2018 Zhijin Li
;; ---------------------------------------------------------------------------

;;(unless window-system
;;  (require 'mouse)
;;  (xterm-mouse-mode t)
;;  (defun track-mouse (e))
;;  (setq mouse-sel-mode t)
;;  )

(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(setq user-full-name "Zhijin Li")
(setq user-mail-address "jonathan.zj.lee@gmail.com")
(setq default-directory "~/")

(set-frame-parameter (selected-frame) 'alpha '(97 . 90))
(add-to-list 'default-frame-alist '(alpha . (97 . 90)))

;; Manage packages thru MELPA.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(load "~/.emacs.d/insert-header.el")
(load "~/.emacs.d/zj-theme.el")
(load "~/.emacs.d/highlight-curr-linum.el")

;; Hilight line and white cursor.
(setq global-hl-line-mode 1)
(setq default-frame-alist '((cursor-color . "white")))

;; Show line-col numbers.
(global-linum-mode t)
(column-number-mode t)
(line-number-mode t)

;; Set page scroll to half.
(defun scroll-half-page-down ()
  "scroll down half the page"
  (interactive)
  (scroll-down-command (/ (window-body-height) 5)))

(defun scroll-half-page-up ()
  "scroll up half the page"
  (interactive)
  (scroll-up-command (/ (window-body-height) 5)))

(global-unset-key (kbd "<prior>"))
(global-unset-key (kbd "<next>"))

(global-set-key (kbd "C-x C-v") 'revert-buffer)

(global-set-key (kbd "<prior>") 'scroll-half-page-down)
(global-set-key (kbd "<next>") 'scroll-half-page-up)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;; Customer defined variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-error-overview-open-after-TeX-run t t)
 '(column-number-mode t)
 '(css-indent-offset 2)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" default)))
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain-compare))
 '(elpy-eldoc-show-current-function nil)
 '(helm-display-header-line nil)
 '(helm-display-source-at-screen-top t)
 '(helm-ff-file-name-history-use-recentf t)
 '(helm-mode t)
 '(helm-move-to-line-cycle-in-source t)
 '(helm-split-window-inside-p t)
 '(indent-guide-global-mode t)
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(js2-highlight-level 3)
 '(markdown-command "pandoc" t)
 '(package-selected-packages
   (quote
    (js2-mode indent-guide yasnippet modern-cpp-font-lock markdown-mode transpose-mark pdf-tools auto-complete smart-mode-line-powerline-theme auctex helm-projectile helm projectile move-text multiple-cursors)))
 '(preview-TeX-style-dir "/home/de329445/.emacs.d/elpa/auctex-11.90.0/latex" t)
 '(reftex-toc-keep-other-windows t)
 '(reftex-toc-split-windows-fraction 0.5)
 '(safe-local-variable-values (quote ((TeX-master . t))))
 '(show-paren-mode t)
 '(tooltip-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "monaco" :slant normal :weight normal :height 130 :width normal))))
 '(ediff-current-diff-B ((t (:background "sienna"))))
 '(ediff-even-diff-A ((t (:background "dark gray" :foreground "black"))))
 '(ediff-even-diff-Ancestor ((t (:background "Grey" :foreground "Black"))))
 '(ediff-even-diff-B ((t (:background "Grey" :foreground "Black"))))
 '(ediff-fine-diff-B ((t (:background "dark green"))))
 '(ediff-odd-diff-A ((t (:background "Grey" :foreground "Black"))))
 '(ediff-odd-diff-C ((t (:background "Grey" :foreground "Black"))))
 '(font-lock-constant-face ((t (:foreground "yellow1"))))
 '(font-lock-function-name-face ((t (:foreground "green yellow"))))
 '(font-lock-keyword-face ((t (:foreground "DeepSkyBlue1"))))
 '(font-lock-variable-name-face ((t (:foreground "plum"))))
 '(helm-selection ((t (:background "purple" :foreground "white" :weight bold))))
 '(helm-source-header ((t (:background "steel blue" :foreground "white" :weight normal :height 1.0 :width normal :family "Courier New"))))
 '(indent-guide-face ((t (:foreground "LightGreen" :slant normal))))
 '(js2-error ((t (:foreground "salmon"))))
 '(js2-external-variable ((t (:foreground "DarkSlateGray3"))))
 '(js2-function-call ((t (:inherit default :foreground "cornflower blue"))))
 '(js2-object-property ((t (:inherit default :foreground "steel blue"))))
 '(js2-object-property-access ((t (:inherit js2-object-property :foreground "LightGoldenrod2"))))
 '(linum ((t (:background "gray25" :foreground "white smoke" :width extra-expanded))))
 '(linum-current-line ((t (:inherit linum :background "black" :foreground "lawn green" :underline nil :weight extra-bold))))
 '(sml/col-number ((t (:inherit sml/global :background "black"))))
 '(sml/filename ((t (:inherit sml/global :background "grey22" :foreground "yellow green"))))
 '(sml/folder ((t (:inherit sml/global :background "grey22" :foreground "light green" :weight normal))))
 '(sml/global ((t (:background "black" :foreground "white smoke" :inverse-video nil))))
 '(sml/line-number ((t (:inherit sml/global :background "black" :foreground "lawn green" :weight normal))))
 '(sml/modes ((t (:inherit sml/global :background "grey40" :foreground "deep sky blue"))))
 '(sml/position-percentage ((t (:inherit sml/prefix :background "grey40" :foreground "lawn green" :underline t :weight normal))))
 '(sml/read-only ((t (:inherit sml/not-modified :foreground "deep sky blue"))))
 '(sml/vc ((t (:inherit sml/git :background "grey40" :foreground "yellow" :underline nil :weight normal))))
 '(sml/vc-edited ((t (:inherit sml/prefix :background "grey40" :foreground "tomato" :slant italic :weight normal))))
 '(term-color-blue ((t (:background "blue2" :foreground "DodgerBlue1"))))
 '(term-color-green ((t (:background "green3" :foreground "PaleGreen"))))
 '(term-color-magenta ((t (:background "magenta3" :foreground "violet"))))
 '(term-color-red ((t (:background "red3" :foreground "LightCoral"))))
 '(term-color-yellow ((t (:background "yellow3" :foreground "khaki"))))
 '(web-mode-html-attr-name-face ((t (:foreground "plum"))))
 '(web-mode-html-tag-face ((t (:foreground "steel blue"))))
 '(web-mode-type-face ((t (:foreground "yellow")))))


;; Disable menu bar & tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; Auto-complete
;; (require 'auto-complete)
;; (global-auto-complete-mode t)
;; (ac-config-default)

;; Use pandoc for markdown mode preview
(setq markdown-command "pandoc")

;; Use smart-mode-line.
(setq powerline-arrow-shape 'curve)
(setq powerline-default-separator-dir '(right . left))

(setq sml/theme 'powerline)
(sml/setup)

;; Mini-buffer prompt color
(set-face-foreground 'minibuffer-prompt "light green")

;; Auto line-wrap in org-mode.
(add-hook 'org-mode-hook (lambda () (setq truncate-lines nil)))

;; Use space as indent unit.
(setq-default indent-tabs-mode nil)

;; Disable up/low case convt warning.
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Delete trailing spaces when saved.
(add-hook 'c-mode-hook        (lambda () (setq show-trailing-whitespace t)))
(add-hook 'c++-mode-hook      (lambda () (setq show-trailing-whitespace t)))
(add-hook 'python-mode-hook   (lambda () (setq show-trailing-whitespace t)))
(add-hook 'text-mode-hook     (lambda () (setq show-trailing-whitespace t)))
(add-hook 'markdown-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'tex-mode-hook      (lambda () (setq show-trailing-whitespace t)))
(add-hook 'LaTeX-mode-hook    (lambda () (setq show-trailing-whitespace t)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Using web-mode for JSX, HTML & templating engines.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

(defun web-mode-indent-hook ()
  "Change web-mode indent to 2."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(defun web-mode-key-hook ()
  "Activate std-file-header command in web-mode"
  "Create shortcut for content setting."
  (local-unset-key (kbd "C-c C-h"))
  (local-set-key (kbd "C-c C-h") 'std-file-header)
  (local-set-key (kbd "C-c C-c") 'web-mode-set-content-type)
  )

(add-hook 'web-mode-hook 'web-mode-indent-hook)
(add-hook 'web-mode-hook 'web-mode-key-hook)

;; Autofill in log-file modes.
(add-hook 'change-log-mode-hook 'turn-on-auto-fill)
(add-hook 'markdown-mode-hook 'turn-on-auto-fill)

;; Switch buffers.
(global-set-key (kbd "C-)") 'next-buffer)
(global-set-key (kbd "C-(") 'previous-buffer)

;; Disable startup screen.
(setq inhibit-startup-message t)

;; Make yes = y, no = n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Override text on selected.
(pending-delete-mode t)

;; Auto save-place.
(setq save-place-file "~/.emacs.d/saved-places")

;; Default mode set to text-mode.
(setq default-major-mode 'text-mode)

;; Change python indentation size to 2.
(add-hook 'python-mode-hook
          (lambda () (setq python-indent 2)))

;; Python indent highlight
(require 'indent-guide)

;; Set javascript indent level to 2.
(setq js-indent-level 2)

;; Elpy mode
(elpy-enable)

;; Use Jedi as elpy backend.
(setq elpy-rpc-backend "jedi")
(setq elpy-rpc-python-command "python3")
(setq python-shell-interpreter "ipython3"
      python-shell-interpreter-args "--simple-prompt --pprint")

;; Disable the annoying elpy key-bindings for crtl navigation.
(define-key elpy-mode-map (kbd "<C-up>") nil)
(define-key elpy-mode-map (kbd "<C-down>") nil)
(define-key elpy-mode-map (kbd "<C-left>") nil)
(define-key elpy-mode-map (kbd "<C-right>") nil)

;; Add file extension modes.
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hh\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.hxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cxx\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; Modern C++ font-lock.
(add-hook 'c++-mode-hook #'modern-c++-font-lock-mode)

;; Correct some C++ for/if intro indentation.
(defun zhijin/cc-correct-indent ()
  (c-set-offset 'substatement-open 0)
  (c-set-offset 'brace-list-open 0)
  (c-set-offset 'brace-list-intro 2))
(add-hook 'c-mode-hook 'zhijin/cc-correct-indent)
(add-hook 'c++-mode-hook 'zhijin/cc-correct-indent)

;; Change scrolling speed.
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Match parenthesis. But no cursor jummps.
(show-paren-mode t)
(setq show-paren-delay 0)
(setq show-paren-style 'parenthesis)

;; Transpose mark.
(global-unset-key (kbd "C-M-t"))
(global-set-key (kbd "C-M-t") 'transpose-mark-region)

;; Show buffer name.
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Set home and end key.
(global-set-key [home] 'beginning-of-buffer)
(global-set-key [end] 'end-of-buffer)

;; For remote terminal mode.
(global-set-key (kbd "C-?") 'help-command)
(global-set-key (kbd "M-?") 'mark-paragraph)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Set timestamp.
(setq time-stamp-active t)
(setq time-stamp-warn-inactive t)
(setq time-stamp-format "%:y-%02m-%02d %3a %02H:%02M:%02S Zhijin")

;; Comment/uncomment region.
(global-set-key (kbd "C-c u") 'comment-or-uncomment-region)

;; Multiple cursors.
(require 'multiple-cursors)
;; (global-unset-key (kbd "C-x C-a"))
(global-set-key (kbd "C-f") 'mc/mark-next-like-this)
(global-set-key (kbd "C-b") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-x C-a") 'mc/mark-all-like-this)
(global-unset-key [C-down-mouse-1])
(global-set-key [C-down-mouse-1] 'mc/add-cursor-on-click)

;; More intellegent GC.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))
(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Setup LOCAL_DIR.
(setenv "LOCAL_DIR" "/Users/jonathanlee/local")

;; Join lines from the top.
(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))
(global-unset-key (kbd "M-^"))
(global-set-key (kbd "M-^") 'top-join-line)

;; Scroll up and down to change font size.
(defun font-big ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (+ (face-attribute 'default :height) 10)))
(defun font-small ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (- (face-attribute 'default :height) 10)))
(global-set-key (kbd "<C-mouse-4>") 'font-big)
(global-set-key (kbd "<C-mouse-5>") 'font-small)

;; Show parentheses matching in buffer echo area when out of screen.
(defadvice show-paren-function
    (after show-matching-paren-offscreen activate)
  "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
  (interactive)
  (let* ((cb (char-before (point)))
         (matching-text (and cb
                             (char-equal (char-syntax cb) ?\) )
                             (blink-matching-open))))
    (when matching-text (message matching-text))))

;; Yank & Indent.
(dolist (command '(yank yank-pop))
  (eval
   `(defadvice ,command (after indent-region activate)
      (and (not current-prefix-arg)
           (member major-mode
                   '(emacs-lisp-mode
                     lisp-mode
                     clojure-mode    scheme-mode
                     haskell-mode    ruby-mode
                     rspec-mode      python-mode
                     c-mode          c++-mode
                     objc-mode       latex-mode
                     plain-tex-mode)
                   )
           (let ((mark-even-if-inactive transient-mark-mode))
             (indent-region (region-beginning) (region-end) nil))))))

;; Helm
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x b") 'helm-buffers-list)

(set-face-attribute 'helm-selection nil
                    :background "purple"
                    :foreground "white")

(setq helm-split-window-in-side-p t)
(setq helm-move-to-line-cycle-in-source t)
(setq helm-ff-file-name-history-use-recentf t)

;; Projectile
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p r") 'projectile-replace-regexp)

(setq projectile-completion-system 'helm)
(setq projectile-switch-project-action 'helm-projectile)
(helm-projectile-on)

(add-to-list 'projectile-other-file-alist '("hh" "hxx")) ;; switch from hh -> hxx
(add-to-list 'projectile-other-file-alist '("hxx" "hh")) ;; switch from hxx -> hh
(add-to-list 'projectile-other-file-alist '("hpp" "cpp")) ;; switch from hpp -> cpp
(add-to-list 'projectile-other-file-alist '("cpp" "hpp")) ;; switch from hpp -> cpp
(add-to-list 'projectile-other-file-alist '("h" "c")) ;; switch from h -> c
(add-to-list 'projectile-other-file-alist '("c" "h")) ;; switch from c -> h

;; Save and compile w/ F5.
(defun zhijin/compile ()
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive) (save-some-buffers t) (compile compile-command))

(global-set-key [f5] 'zhijin/compile)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Latex
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Helm-bibtex
(autoload 'helm-bibtex "helm-bibtex" "" t)

(setq reftex-default-bibliography
      '("~/your_bib1.bib"
        "~/your_bib2.bib"))

;; For AUCtex
(setq TeX-PDF-mode t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; Use Skim as viewer, enable source <-> PDF sync
;; make latexmk available via C-c C-c
;; Note: SyncTeX is setup via ~/.latexmkrc (see below)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (push
                              '("latexmk" "latexmk -pdf %s" TeX-run-TeX nil t
                                :help "Run latexmk on file")
                              TeX-command-list)))
(add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))

;; use Skim as default pdf viewer
;; Skim's displayline is used for forward search (from .tex to .pdf)
;; option -b highlights the current line; option -g opens Skim in the background
(setq TeX-view-program-selection '((output-pdf "PDF Viewer")))
(setq TeX-view-program-list
      '(("PDF Viewer"
         "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq reftex-extra-bindings t)

(setq LaTeX-includegraphics-read-file
      'LaTeX-includegraphics-read-file-relative)

;; Always show compilation warnings & bad-boxes.
(setq TeX-debug-warnings t
      TeX-debug-bad-boxes t)
(setq TeX-error-overview-open-after-TeX-run t)

(setq reftex-plug-into-AUCTeX t)
(setq reftex-plug-into-auctex t)

;; Disable default varying text size.
(setq font-latex-fontify-script nil)
(setq font-latex-fontify-sectioning 'color)
(setq reftex-use-multiple-selection-buffers t)

(setq flyspell-issue-message-flag nil)

(add-hook 'LaTeX-mode-hook
          (lambda () (setq flyspell-mode t)))

;; Forward / backward search with Okular.
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

;; Use outline-minor-mode
;; change key binding prefix to \C-o
(add-hook 'LaTeX-mode-hook #'outline-minor-mode)

(global-unset-key "\C-o")
(add-hook 'outline-minor-mode-hook
          (lambda () (local-set-key "\C-o" outline-mode-prefix-map)))

(add-hook 'LaTeX-mode-hook 'turn-on-auto-fill)
(add-hook 'latex-mode-hook 'turn-on-auto-fill)

;; Magit setup
(global-set-key (kbd "C-x g") 'magit-status)

;; YASnippet
;; (require 'yasnippet)
;; (yas-global-mode 1)

;; (setq yas/root-directory "~/.emacs.d/snippets")
;; (yas/load-directory yas/root-directory)

(define-key yas-minor-mode-map [(control return)] 'yas-expand)
(define-key yas-minor-mode-map [(tab)]            nil)
(define-key yas-minor-mode-map (kbd "TAB")        nil)
(define-key yas-minor-mode-map (kbd "<tab>")      nil)

;; Move text.
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line)
      (when (or (< arg 0) (not (eobp)))
        (transpose-lines arg))
      (forward-line -1)))))

(defun move-text-down (arg)
  "Move current line or region arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move current line or region arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key [\M-down] 'move-text-down)
(global-set-key [\M-up] 'move-text-up)
