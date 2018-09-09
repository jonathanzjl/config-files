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
;; File: zj-theme.el for config-files
;;
;; Created by Zhijin Li
;; E-mail:   <jonathan.zj.lee@gmail.com>
;;
;; Started on  Sun Sep  9 21:07:19 2018 Zhijin Li
;; Last update Sun Sep  9 21:08:06 2018 Zhijin Li
;; ---------------------------------------------------------------------------


(deftheme zj
  "An Emacs theme. Based on lush, customized by jonathan.zj.lee@gmail.com.")

(let* ((zj/background "#202020")
       (zj/foreground "#E0E0E0")
       (zj/turquoise  "#2AA198")
       (zj/orange     "#FF951B")
       (zj/pink       "#FF88FF")
       (zj/yellow     "#FFE329")
       (zj/green      "#61CE3C")
       (zj/light-blue "#82A6DF")
       (zj/dark-blue  "#253B76")
       (zj/light-red  "#FA583F")
       (zj/hl-line    "#333333"))

  (custom-theme-set-faces
   `zj
   `(border-glyph                        ((t (nil))))
   `(default                             ((t (:foreground , zj/foreground :background ,zj/background))))
   `(buffers-tab                         ((t (:foreground , zj/foreground :background ,zj/background))))
   `(font-lock-builtin-face              ((t (:foreground, zj/foreground))))
   `(font-lock-comment-face              ((t (:foreground, "LightSkyBlue4" ))))
   `(font-lock-constant-face             ((t (:foreground, "green3"))))
   `(font-lock-doc-string-face           ((t (:foreground, "LightYellow4"))))
   `(font-lock-function-name-face        ((t (:foreground, "yellow"))))
   `(font-lock-keyword-face              ((t (:foreground, "SteelBlue1"))))
   `(font-lock-preprocessor-face         ((t (:foreground, "SkyBlue4"))))
   `(font-lock-reference-face            ((t (:foreground, "SlateBlue"))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground, "#E9C062"))))
   `(font-lock-regexp-grouping-construct ((t (:foreground, "red"))))
   `(font-lock-string-face               ((t (:foreground, "salmon"))))
   `(font-lock-type-face                 ((t (:foreground, "DarkCyan"))))
   `(font-lock-variable-name-face        ((t (:foreground, "brown1"))))
   `(font-lock-warning-face              ((t (:foreground, "Pink"))))
   `(gui-element                         ((t (:foreground, "black"    :background "#D4D0C8"))))
   `(mode-line                           ((t (:foreground, "#F0F0F0"  :background "#444444" :box nil))))
   `(mode-line-highlight                 ((t (:foreground, "pink" :box nil))))
   `(hl-line                             ((t (:background, zj/hl-line))))
   `(text-cursor                         ((t (:foreground, "black"    :background "yellow"))))
   `(zmacs-region                        ((t (:foreground, "ble"      :background "snow"))))
   `(region                              ((t (:foreground, "white" :background, "SkyBlue4"))))
   `(highlight                           ((t (:background, "#222222"))))
   `(highline-face                       ((t (:background, "SeaGreen"))))
   `(italic                              ((t (nil))))
   `(left-margin                         ((t (nil))))
   `(toolbar                             ((t (nil))))
   `(underline                           ((nil (:underline nil))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory
                (file-name-directory
                 load-file-name))))

;;;###autoload
(defun zj-theme()
  "Load zj-theme."
  (interactive)
  (load-theme 'zj t))

(provide-theme 'zj)
