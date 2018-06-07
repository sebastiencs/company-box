;;; company-box-icons.el --- Company front-end  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-box

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
;; Icons for company-box

;;; Code:

(require 'dash)

(eval-when-compile
  (require 'find-func)
  (defconst company-box-icons-dir
    (->> (find-library-name "company-box")
         (file-name-directory)
         (expand-file-name "images")
         (file-name-as-directory)))
  (defun company-box-icons-image (file)
    `(image :type png
            :file ,(concat company-box-icons-dir file)
            :ascent center)))

(defvar company-box-icons-unknown
  (eval-when-compile
    (company-box-icons-image "Namespace.png"))
  "Icon used on an unknown item.
See `company-box-icons-functions' for the ICON format.")

(defvar company-box-icons-elisp
  (eval-when-compile
    `(,(company-box-icons-image "Method.png")
      ,(company-box-icons-image "Field.png")
      ,(company-box-icons-image "Class.png")
      ,(company-box-icons-image "ColorPalette.png")))
  "List of icons to use with Emacs Lisp candidates.
The list has the form:
(FUNCTION VALUE FEATURE FACE).
See `company-box-icons-functions' for each ICON format.")

(defvar company-box-icons-yasnippet
  (eval-when-compile
    (company-box-icons-image "Template.png"))
  "Icon to use with yasnippet candidates.
See `company-box-icons-functions' for the ICON format.")

(defvar company-box-icons-lsp
  (eval-when-compile
    `((1 . ,(company-box-icons-image "String.png")) ;; Text
      (2 . ,(company-box-icons-image "Method.png")) ;; Method
      (3 . ,(company-box-icons-image "Method.png")) ;; Function
      (4 . ,(company-box-icons-image "Method.png")) ;; Constructor
      (5 . ,(company-box-icons-image "Field.png")) ;; Field
      (6 . ,(company-box-icons-image "Field.png")) ;; Variable
      (7 . ,(company-box-icons-image "Class.png")) ;; Class
      (8 . ,(company-box-icons-image "Interface.png")) ;; Interface
      (9 . ,(company-box-icons-image "Namespace.png")) ;; Module
      (10 . ,(company-box-icons-image "Property.png")) ;; Property
      (11 . ,(company-box-icons-image "Misc.png")) ;; Unit
      (12 . ,(company-box-icons-image "EnumItem.png")) ;; Value
      (13 . ,(company-box-icons-image "Enumerator.png")) ;; Enum
      (14 . ,(company-box-icons-image "Keyword.png")) ;; Keyword
      (15 . ,(company-box-icons-image "String.png")) ;; Snippet
      (16 . ,(company-box-icons-image "ColorPalette.png")) ;; Color
      (17 . ,(company-box-icons-image "Document.png")) ;; File
      (18 . ,(company-box-icons-image "Misc.png")) ;; Reference
      (19 . ,(company-box-icons-image "Folder.png")) ;; Folder
      (20 . ,(company-box-icons-image "EnumItem.png")) ;; EnumMember
      (21 . ,(company-box-icons-image "Constant.png")) ;; Constant
      (22 . ,(company-box-icons-image "Class.png")) ;; Struct
      (23 . ,(company-box-icons-image "Event.png")) ;; Event
      (24 . ,(company-box-icons-image "Misc.png")) ;; Operator
      (25 . ,(company-box-icons-image "Class.png")))) ;; TypeParameter
  "List of Icons to use with LSP candidates.

Each element have the form:
(KIND . ICON)

Where KIND correspond to a number, the CompletionItemKind from the LSP [1]

See `company-box-icons-functions' for the ICON format.

[1] https://github.com/Microsoft/language-server-protocol/blob/gh-pages/\
specification.md#completion-request-leftwards_arrow_with_hook.")

(defvar company-box-icons-acphp
  (eval-when-compile
    `(,(company-box-icons-image "Interface.png") ;; Trait
      ,(company-box-icons-image "Class.png")     ;; Class
      ,(company-box-icons-image "Method.png")    ;; Method
      ,(company-box-icons-image "Method.png")    ;; Function
      ,(company-box-icons-image "Property.png")  ;; Property
      ,(company-box-icons-image "Constant.png")  ;; Constant
      ,(company-box-icons-image "Field.png")     ;; Member/Variable
      ,(company-box-icons-image "Interface.png") ;; Interface
      ,(company-box-icons-image "Namespace.png") ;; Namespace
      ,(company-box-icons-image "Template.png")  ;; Usetrait
      ,(company-box-icons-image "Misc.png")))    ;; Fallback
  "List of icons to use with PHP candidates.

  The meaning corresponds to the position in this list:
  (TRAIT CLASS METHOD FUNCTION PROPERTY CONSTANT VARIABLE INTERFACE NAMESPACE USETRAIT FALLBACK)
  
See `company-box-icons-functions' for each ICON format.")

(defun company-box-icons--lsp (candidate)
  (-when-let* ((lsp-item (get-text-property 0 'lsp-completion-item candidate))
               (kind (gethash "kind" lsp-item)))
    (alist-get kind company-box-icons-lsp)))

(defun company-box-icons--elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let* ((sym (intern candidate))
           (item (cond ((fboundp sym) 0)
                       ((boundp sym) 1)
                       ((featurep sym) 2)
                       ((facep sym) 3))))
      (when item
        (nth item company-box-icons-elisp)))))

(defun company-box-icons--yasnippet (candidate)
  (when (get-text-property 0 'yas-annotation candidate)
    company-box-icons-yasnippet))

(defun company-box-icons--acphp (candidate)
  (when (derived-mode-p 'php-mode)
    (let* ((type-tag (get-text-property 0 'ac-php-tag-type candidate))
           (item (cond ((equal "t" type-tag) 0)
                       ((equal "c" type-tag) 1)
                       ((equal "m" type-tag) 2)
                       ((equal "f" type-tag) 3)
                       ((equal "p" type-tag) 4)
                       ((equal "d" type-tag) 5)
                       ((equal "v" type-tag) 6)
                       ((equal "i" type-tag) 7)
                       ((equal "n" type-tag) 8)
                       ((equal "T" type-tag) 9)
                       (t 10))))
      (nth item company-box-icons-acphp))))

(provide 'company-box-icons)
;;; company-box-icons.el ends here
