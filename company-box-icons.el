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

(defvar company-box-icons-images
  (eval-when-compile
    `((Unknown . ,(company-box-icons-image "Namespace.png"))
      (Text . ,(company-box-icons-image "String.png"))
      (Method . ,(company-box-icons-image "Method.png"))
      (Function . ,(company-box-icons-image "Method.png"))
      (Constructor . ,(company-box-icons-image "Method.png"))
      (Field . ,(company-box-icons-image "Field.png"))
      (Variable . ,(company-box-icons-image "Field.png"))
      (Class . ,(company-box-icons-image "Class.png"))
      (Interface . ,(company-box-icons-image "Interface.png"))
      (Module . ,(company-box-icons-image "Namespace.png"))
      (Property . ,(company-box-icons-image "Property.png"))
      (Unit . ,(company-box-icons-image "Misc.png"))
      (Value . ,(company-box-icons-image "EnumItem.png"))
      (Enum . ,(company-box-icons-image "Enumerator.png"))
      (Keyword . ,(company-box-icons-image "Keyword.png"))
      (Snippet . ,(company-box-icons-image "String.png"))
      (Color . ,(company-box-icons-image "ColorPalette.png"))
      (File . ,(company-box-icons-image "Document.png"))
      (Reference . ,(company-box-icons-image "Misc.png"))
      (Folder . ,(company-box-icons-image "Folder.png"))
      (EnumMember . ,(company-box-icons-image "EnumItem.png"))
      (Constant . ,(company-box-icons-image "Constant.png"))
      (Struct . ,(company-box-icons-image "Class.png"))
      (Event . ,(company-box-icons-image "Event.png"))
      (Operator . ,(company-box-icons-image "Misc.png"))
      (TypeParameter . ,(company-box-icons-image "Class.png"))
      (Template . ,(company-box-icons-image "Template.png")))))

(when (require 'all-the-icons nil t)
  ;; TODO: fix the rest
  (defvar company-box-icons-all-the-icons
    (eval-when-compile
      `((Unknown . ,(all-the-icons-faicon "cog"))
        (Text . ,(all-the-icons-octicon "file-text"))
        (Method . ,(all-the-icons-faicon "cube"))
        (Function . ,(all-the-icons-faicon "cube"))
        (Constructor . ,(all-the-icons-faicon "cube"))
        (Field . ,(all-the-icons-faicon "cog"))
        (Variable . ,(all-the-icons-faicon "cog"))
        (Class . ,(all-the-icons-faicon "cogs"))
        ;; (Interface . ,(company-box-icons-image "Interface.png"))
        (Module . ,(all-the-icons-alltheicon "less"))
        (Property . ,(all-the-icons-faicon "wrench"))
        ;; (Unit . ,(company-box-icons-image "Misc.png"))
        ;; (Value . ,(company-box-icons-image "EnumItem.png"))
        (Enum . ,(all-the-icons-material "content_copy"))
        ;; (Keyword . ,(company-box-icons-image "Keyword.png"))
        (Snippet . ,(all-the-icons-material "content_paste"))
        (Color . ,(all-the-icons-material "palette"))
        (File . ,(all-the-icons-faicon "file"))
        ;; (Reference . ,(company-box-icons-image "Misc.png"))
        (Folder . ,(all-the-icons-faicon "folder"))
        ;; (EnumMember . ,(company-box-icons-image "EnumItem.png"))
        ;; (Constant . ,(company-box-icons-image "Constant.png"))
        (Struct . ,(all-the-icons-faicon "cogs"))
        (Event . ,(all-the-icons-faicon "bolt"))
        ;; (Operator . ,(company-box-icons-image "Misc.png"))
        (TypeParameter . ,(all-the-icons-faicon "cogs"))
        ;; (Template . ,(company-box-icons-image "Template.png"))
        ))))

;; TODO: defcustom
(defvar company-box-icons-alist company-box-icons-images
  "Alist with (Type . icon) pairs, see `company-box-icons-functions' for the ICON format.")

(defconst company-box-icons--lsp-alist
  '((1 . Text)
    (2 . Method)
    (3 . Function)
    (4 . Constructor)
    (5 . Field)
    (6 . Variable)
    (7 . Class)
    (8 . Interface)
    (9 . Property)
    (10 . Module)
    (11 . Unit)
    (12 . Value)
    (13 . Enum)
    (14 . Keyword)
    (15 . Snippet)
    (16 . Color)
    (17 . File)
    (18 . Reference)
    (19 . Folder)
    (20 . EnumMember)
    (21 . Constant)
    (22 . Struct)
    (23 . Event)
    (24 . Operator)
    (25 . TypeParameter))
  "List of Icons to use with LSP candidates.

Each element have the form:
(KIND . ICON-TYPE)

Where KIND correspond to a number, the CompletionItemKind from the LSP [1]

See `company-box-icons-images' or `company-box-icons-all-the-icons' for the ICON-TYPEs

[1] https://github.com/Microsoft/language-server-protocol/blob/gh-pages/\
specification.md#completion-request-leftwards_arrow_with_hook.")

(defun company-box-icons--lsp (candidate)
  (-when-let* ((lsp-item (get-text-property 0 'lsp-completion-item candidate))
               (kind-num (gethash "kind" lsp-item)))
    (alist-get kind-num company-box-icons--lsp-alist)))

(defconst company-box-icons--php-alist
  '(("t" . Interface)
    ("c" . Class)
    ("m" . Method)
    ("f" . Function)
    ("p" . Property)
    ("d" . Constant)
    ("v" . Variable)
    ("i" . Interface)
    ("n" . Module)
    ("T" . Template))
  "List of icon types to use with PHP candidates.")

(defun company-box-icons--acphp (candidate)
  (when (derived-mode-p 'php-mode)
    (-> (get-text-property 0 'ac-php-tag-type candidate)
        (alist-get company-box-icons--php-alist))))

(defun company-box-icons--elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((sym (intern candidate)))
      ;; we even can move it to (predicate .  kind) alist
      (cond ((fboundp sym) 'Function)
            ((featurep sym) 'Module)
            ((facep sym) 'Color)
            ((boundp sym) 'Variable)
            (t . nil)))))

(defun company-box-icons--yasnippet (candidate)
  (when (get-text-property 0 'yas-annotation candidate)
    'Template))

(provide 'company-box-icons)
;;; company-box-icons.el ends here
