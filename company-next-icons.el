;;; company-next-icons.el --- Company front-end  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-next
;; Keywords: company, front-end

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; A company front-end

;;; Code:

(require 'dash)

(defvar company-next-icons-unknown
  'fa_question_circle
  "Icon used when on unknown item.
See `company-next-icons-functions'.")

(defvar company-next-icons~elisp
  '((fa_tag :face font-lock-function-name-face)
    (fa_cog :face font-lock-variable-name-face)
    (fa_cube :face font-lock-constant-face)
    (md_color_lens :face font-lock-doc-face))
  "List of icons to use with Emacs Lisp candidates.
It has the form:
(FUNCTION VALUE FEATURE FACE).
See `company-next-icons-functions'.")

(defvar company-next-icons~yasnippet
  'fa_bookmark
  "Icon to use with yasnippet candidates.
See `company-next-icons-functions'.")

(defvar company-next-icons~lsp
  '((1 . fa_text_height) ;; Text
    (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
    (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
    (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
    (5 . (fa_cog :face font-lock-variable-name-face)) ;; Field
    (6 . (fa_cog :face font-lock-variable-name-face)) ;; Variable
    (7 . (fa_cube :face font-lock-type-face)) ;; Class
    (8 . (fa_cube :face font-lock-type-face)) ;; Interface
    (9 . (fa_cube :face font-lock-type-face)) ;; Module
    (10 . (fa_cog :face font-lock-variable-name-face)) ;; Property
    (11 . md_settings_system_daydream) ;; Unit
    (12 . (fa_cog :face font-lock-variable-name-face)) ;; Value
    (13 . (md_storage :face font-lock-type-face)) ;; Enum
    (14 . (md_closed_caption :face font-lock-keyword-face)) ;; Keyword
    (15 . md_closed_caption) ;; Snippet
    (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
    (17 . fa_file_text_o) ;; File
    (18 . md_refresh) ;; Reference
    (19 . fa_folder_open) ;; Folder
    (20 . (md_closed_caption :face font-lock-type-face)) ;; EnumMember
    (21 . (fa_square :face font-lock-constant-face)) ;; Constant
    (22 . (fa_cube :face font-lock-type-face)) ;; Struct
    (23 . fa_calendar) ;; Event
    (24 . fa_square_o) ;; Operator
    (25 . fa_arrows)) ;; TypeParameter
  "List of Icons to use with LSP candidates.

Elements are of the form:
(KIND . ICON)

Where KIND correspond to a number, the CompletionItemKind from the LSP [1]

ICON can be a symbol, a list or a string.
- SYMBOL:  It is the name of the icon.
- LIST:    The list is then `apply' to `icons-in-terminal' function.
           Example: '(fa_icon :face some-face :foreground \"red\")
- STRING:  A simple string which is inserted, should be of length 1


[1] https://github.com/Microsoft/language-server-protocol/blob/gh-pages/specification.md#completion-request-leftwards_arrow_with_hook

See `company-next-icons-functions'.")

(defun company-next-icons~lsp (candidate)
  (-when-let* ((lsp-item (get-text-property 0 'lsp-completion-item candidate))
               (kind (gethash "kind" lsp-item)))
    (alist-get kind company-next-icons~lsp)))

(defun company-next-icons~elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let* ((sym (intern candidate))
           (item (cond ((fboundp sym) 0)
                       ((boundp sym) 1)
                       ((featurep sym) 2)
                       ((facep sym) 3))))
      (when item
        (nth item company-next-icons~elisp)))))

(defun company-next-icons~yasnippet (candidate)
  (when (get-text-property 0 'yas-annotation candidate)
    company-next-icons~yasnippet))

;; (defun company-next~get-icon (candidate)
;;   (or (-when-let* ((_ (fboundp 'icons-in-terminal))
;;                    (lsp-item (get-text-property 0 'lsp-completion-item candidate))
;;                    (kind (gethash "kind" lsp-item)))
;;         (icons-in-terminal
;;          (pcase kind
;;            (1 'fa_text_height) ;; Text
;;            (2 'fa_tags) ;; Method
;;            ((or 3 4) 'fa_tag) ;; Method, Function, Constructor
;;            ((or 5 6 10 12) 'fa_cog) ;; Field, Variable, Property, Value
;;            ((or 7 8 9 22) 'mfizz_aws) ;; Class, Interface, Module, Struct
;;            (11 'md_settings_system_daydream) ;; Unit
;;            (13 'md_storage) ;; Enum
;;            ((or 14 15 20) 'md_closed_caption) ;; Enum, Keyword, EnumMember
;;            (16 'md_color_lens) ;; Color
;;            (17 'fa_file_text_o) ;; File
;;            (18 'md_refresh) ;; Reference
;;            (19 'fa_folder_open) ;; Folder
;;            (21 'fa_square) ;; Constant
;;            (23 'fa_calendar) ;; Event
;;            (24 'fa_square_o) ;; Operator
;;            (25 'fa_arrows) ;; TypeParameter
;;            )))
;;       (when (derived-mode-p 'emacs-lisp-mode)
;;         (icons-in-terminal
;;          (let ((sym (intern candidate)))
;;            (cond ((fboundp sym) 'fa_tag)
;;                  ((boundp sym) 'fa_cog)
;;                  ((featurep sym) 'fa_folder_open)
;;                  ((facep sym) 'md_color_lens)
;;                  (t 'fa_question_circle)))))
;;       (when (and (fboundp 'icons-in-terminal)
;;                  (get-text-property 0 'yas-annotation candidate))
;;         (icons-in-terminal 'fa_bookmark))
;;       (when (fboundp 'icons-in-terminal)
;;         (icons-in-terminal 'fa_question_circle))))

(provide 'company-next-icons)
