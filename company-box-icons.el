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
  (require 'subr-x)
  (defconst company-box-icons-dir
    (->> (find-library-name "company-box")
         (file-name-directory)
         (expand-file-name "images")
         (file-name-as-directory)))
  (defconst company-box--have-imagemagick (image-type-available-p 'imagemagick)
    "Emacs might not be compiled with imagemagick.")
  (defun company-box-icons-image (file)
    (let* ((extension (intern (upcase (or (file-name-extension file) ""))))
           (use-magick (and company-box--have-imagemagick
                            (not (member extension imagemagick-types-inhibit))
                            (member extension imagemagick-enabled-types))))
      `(image :type ,(if use-magick 'imagemagick 'png)
              :file ,(concat company-box-icons-dir file)
              :ascent center))))

(defvar company-box-icons-icons-in-terminal
  '((Unknown fa_question_circle)
    (Text . fa_text_height) ;; Text
    (Method . (fa_tags :face font-lock-function-name-face)) ;; Method
    (Function . (fa_tag :face font-lock-function-name-face)) ;; Function
    (Constructor . (fa_tag :face font-lock-function-name-face)) ;; Constructor
    (Field . (fa_cog :foreground "#FF9800")) ;; Field
    (Variable . (fa_cog :foreground "#FF9800")) ;; Variable
    (Class . (fa_cube :foreground "#7C4DFF")) ;; Class
    (Interface . (fa_cube :foreground "#7C4DFF")) ;; Interface
    (Module . (fa_cube :foreground "#7C4DFF")) ;; Module
    (Property . (fa_cog :foreground "#FF9800")) ;; Property
    (Unit . md_settings_system_daydream) ;; Unit
    (Value . (fa_cog :foreground "#FF9800")) ;; Value
    (Enum . (md_storage :face font-lock-type-face)) ;; Enum
    (Keyword . (md_closed_caption :foreground "#009688")) ;; Keyword
    (Snippet . md_closed_caption) ;; Snippet
    (Color . (md_color_lens :face font-lock-doc-face)) ;; Color
    (File . fa_file_text_o) ;; File
    (Reference . md_refresh) ;; Reference
    (Folder . fa_folder_open) ;; Folder
    (EnumMember . (md_closed_caption :foreground "#009688")) ;; EnumMember
    (Constant . (fa_square :face font-lock-constant-face)) ;; Constant
    (Struct . (fa_cube :face font-lock-type-face)) ;; Struct
    (Event . fa_calendar) ;; Event
    (Operator . fa_square_o) ;; Operator
    (TypeParameter . fa_arrows)
    (Template . fa_bookmark)) ;; TypeParameter
  )

(defvar company-box-icons-eclipse
  (eval-when-compile
    `((Unknown . ,(company-box-icons-image "Namespace.png"))
      (Text . ,(company-box-icons-image "eclipse/text.png"))
      (Method . ,(company-box-icons-image "eclipse/method.png"))
      (Function . ,(company-box-icons-image "eclipse/function.png"))
      (Constructor . ,(company-box-icons-image "eclipse/constructor.png"))
      (Field . ,(company-box-icons-image "eclipse/field.png"))
      (Variable . ,(company-box-icons-image "eclipse/variable.png"))
      (Class . ,(company-box-icons-image "eclipse/class.png"))
      (Interface . ,(company-box-icons-image "eclipse/interface.png"))
      (Module . ,(company-box-icons-image "eclipse/module.png"))
      (Property . ,(company-box-icons-image "eclipse/property.png"))
      (Unit . ,(company-box-icons-image "eclipse/unit.png"))
      (Value . ,(company-box-icons-image "eclipse/value.png"))
      (Enum . ,(company-box-icons-image "eclipse/enum.png"))
      (Keyword . ,(company-box-icons-image "eclipse/keyword.png"))
      (Snippet . ,(company-box-icons-image "eclipse/snippet.png"))
      (Color . ,(company-box-icons-image "eclipse/color.png"))
      (File . ,(company-box-icons-image "eclipse/file.png"))
      (Reference . ,(company-box-icons-image "eclipse/reference.png"))
      (Folder . ,(company-box-icons-image "eclipse/folder.png"))
      (EnumMember . ,(company-box-icons-image "eclipse/enummember.png"))
      (Constant . ,(company-box-icons-image "eclipse/constant.png"))
      (Struct . ,(company-box-icons-image "eclipse/struct.png"))
      (Event . ,(company-box-icons-image "eclipse/event.png"))
      (Operator . ,(company-box-icons-image "eclipse/operator.png"))
      (TypeParameter . ,(company-box-icons-image "eclipse/typeparameter.png"))
      (Template . ,(company-box-icons-image "eclipse/template.png")))))

(defvar company-box-icons-netbeans
  (eval-when-compile
    `((Unknown . ,(company-box-icons-image "Namespace.png"))
      (Text . ,(company-box-icons-image "netbeans/text.png"))
      (Method . ,(company-box-icons-image "netbeans/method.png"))
      (Function . ,(company-box-icons-image "netbeans/function.png"))
      (Constructor . ,(company-box-icons-image "netbeans/constructor.png"))
      (Field . ,(company-box-icons-image "netbeans/field.png"))
      (Variable . ,(company-box-icons-image "netbeans/variable.gif"))
      (Class . ,(company-box-icons-image "netbeans/class.png"))
      (Interface . ,(company-box-icons-image "netbeans/interface.png"))
      (Module . ,(company-box-icons-image "netbeans/module.png"))
      (Property . ,(company-box-icons-image "netbeans/property.png"))
      (Unit . ,(company-box-icons-image "netbeans/unit.png"))
      (Value . ,(company-box-icons-image "netbeans/value.png"))
      (Enum . ,(company-box-icons-image "netbeans/enum.png"))
      (Keyword . ,(company-box-icons-image "netbeans/keyword.png"))
      (Snippet . ,(company-box-icons-image "netbeans/snippet.png"))
      (Color . ,(company-box-icons-image "netbeans/color.png"))
      (File . ,(company-box-icons-image "netbeans/file.png"))
      (Reference . ,(company-box-icons-image "netbeans/reference.png"))
      (Folder . ,(company-box-icons-image "netbeans/folder.png"))
      (EnumMember . ,(company-box-icons-image "netbeans/enummember.png"))
      (Constant . ,(company-box-icons-image "netbeans/constant.png"))
      (Struct . ,(company-box-icons-image "netbeans/struct.png"))
      (Event . ,(company-box-icons-image "netbeans/event.png"))
      (Operator . ,(company-box-icons-image "netbeans/operator.png"))
      (TypeParameter . ,(company-box-icons-image "netbeans/typeparameter.png"))
      (Template . ,(company-box-icons-image "netbeans/template.png")))))

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

(defvar company-box-icons-idea
  (eval-when-compile
    `((Unknown . ,(company-box-icons-image "idea/package.png"))
      ;; (Text . ,(company-box-icons-image "idea/misc.png"))
      (Method . ,(company-box-icons-image "idea/method.png"))
      (Function . ,(company-box-icons-image "idea/method.png"))
      (Constructor . ,(company-box-icons-image "idea/method.png"))
      (Field . ,(company-box-icons-image "idea/field.png"))
      (Variable . ,(company-box-icons-image "idea/field.png"))
      (Class . ,(company-box-icons-image "idea/class.png"))
      (Interface . ,(company-box-icons-image "idea/interface.png"))
      (Module . ,(company-box-icons-image "idea/package.png"))
      (Property . ,(company-box-icons-image "idea/property.png"))
      ;; (Unit . ,(company-box-icons-image "idea/misc.png"))
      (Value . ,(company-box-icons-image "idea/field.png"))
      (Enum . ,(company-box-icons-image "idea/enum.png"))
      ;; (Keyword . ,(company-box-icons-image "idea/misc.png"))
      (Snippet . ,(company-box-icons-image "idea/snippet.png"))
      ;; (Color . ,(company-box-icons-image "idea/misc.png"))
      (File . ,(company-box-icons-image "idea/ppFile.png"))
      (Reference . ,(company-box-icons-image "idea/misc.png"))
      (Folder . ,(company-box-icons-image "idea/ppFile.png"))
      (EnumMember . ,(company-box-icons-image "idea/enum.png"))
      (Constant . ,(company-box-icons-image "idea/field.png"))
      (Struct . ,(company-box-icons-image "idea/class.png"))
      (Event . ,(company-box-icons-image "Event.png"))
      ;; (Operator . ,(company-box-icons-image "Misc.png"))
      (TypeParameter . ,(company-box-icons-image "Class.png"))
      (Template . ,(company-box-icons-image "Template.png")))))

(declare-function all-the-icons-faicon "ext:all-the-icons")
(declare-function all-the-icons-material "ext:all-the-icons")
(declare-function all-the-icons-octicon "ext:all-the-icons")

(when (require 'all-the-icons nil t)
  (defvar company-box-icons-all-the-icons
    `((Unknown . ,(all-the-icons-faicon "code"))
      (Text . ,(all-the-icons-material "text_format"))
      (Method . ,(all-the-icons-faicon "cube"))
      (Function . ,(all-the-icons-faicon "cube"))
      (Constructor . ,(all-the-icons-faicon "cube"))
      (Field . ,(all-the-icons-faicon "tag"))
      (Variable . ,(all-the-icons-faicon "tag"))
      (Class . ,(all-the-icons-faicon "cogs"))
      (Interface . ,(all-the-icons-faicon "italic"))
      (Module . ,(all-the-icons-faicon "code"))
      (Property . ,(all-the-icons-faicon "wrench"))
      (Unit . ,(all-the-icons-material "streetview"))
      (Value . ,(all-the-icons-faicon "tag"))
      (Enum . ,(all-the-icons-material "library_books"))
      (Keyword . ,(all-the-icons-material "vpn_key"))
      (Snippet . ,(all-the-icons-faicon "expand"))
      (Color . ,(all-the-icons-material "palette"))
      (File . ,(all-the-icons-faicon "file"))
      (Reference . ,(all-the-icons-material "streetview"))
      (Folder . ,(all-the-icons-faicon "folder-open"))
      (EnumMember . ,(all-the-icons-faicon "book"))
      (Constant . ,(all-the-icons-faicon "bars"))
      (Struct . ,(all-the-icons-faicon "cogs"))
      (Event . ,(all-the-icons-faicon "bolt"))
      (Operator . ,(all-the-icons-material "streetview"))
      (TypeParameter . ,(all-the-icons-faicon "cogs"))
      (Template . ,(all-the-icons-material "settings_ethernet")))))

(defcustom company-box-icons-alist 'company-box-icons-images
  "Rendering method for icons.

- Images
- idea, from idea editors
- netbeans, from netbeans editor
- eclipse, from eclipse editor
- all-the-icons [1]
- icons-in-terminal [2]

Changing colors of icons only works with `all-the-icons' and
`icons-in-terminal'.  Colors with others methods are taken from
the image.

[1] https://github.com/domtronn/all-the-icons.el
[2] https://github.com/sebastiencs/icons-in-terminal
."
  :type '(choice (const :tag "images" company-box-icons-images)
                 (const :tag "idea" company-box-icons-idea)
                 (const :tag "netbeans" company-box-icons-netbeans)
                 (const :tag "eclipse" company-box-icons-eclipse)
                 (const :tag "all-the-icons" company-box-icons-all-the-icons)
                 (const :tag "icons-in-terminal" company-box-icons-icons-in-terminal))
  :group 'company-box)

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

(defconst company-box-icons--eglot-alist
  company-box-icons--lsp-alist)

(defun company-box-icons--lsp (candidate)
  (-when-let* ((lsp-item (get-text-property 0 'lsp-completion-item candidate))
               (kind-num (and (fboundp 'lsp-get) (lsp-get lsp-item :kind))))
    (alist-get kind-num company-box-icons--lsp-alist)))

(defun company-box-icons--eglot (candidate)
  (-when-let* ((eglot-item (get-text-property 0 'eglot--lsp-item candidate))
               (kind-num (plist-get eglot-item :kind)))
    (alist-get kind-num company-box-icons--eglot-alist)))

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

(defconst company-box-icons--cider-alist
  '(("special-form" . Operator)
    ("function" . Function)
    ("method" . Method)
    ("macro" . Template)
    ("class" . Class)
    ("import" . Module)
    ("field" . Field)
    ("keyword" . Keyword)
    ("local" . Variable)
    ("var" . Variable)
    ("namespace" . Unit)
    ("protocol" . Interface)
    ("protocol-function" . Property)
    ("record" . Constructor)
    ("static-field" . Field)
    ("static-method" . Method)
    ("type" . TypeParameter))
  "List of icon types to use with CIDER candidates.
Check out `cider-completion-annotations-alist'.")

(defun company-box-icons--cider (candidate)
  (when (or (derived-mode-p 'clojure-mode) (derived-mode-p 'cider-repl-mode))
    (-> (get-text-property 0 'type candidate)
        (alist-get company-box-icons--cider-alist nil nil 'equal))))

(defun company-box-icons--elisp (candidate)
  (when (derived-mode-p 'emacs-lisp-mode)
    (let ((sym (intern candidate)))
      ;; we even can move it to (predicate .  kind) alist
      (cond ((fboundp sym) 'Function)
            ((featurep sym) 'Module)
            ((facep sym) 'Color)
            ((boundp sym) 'Variable)
            (t 'Unknown)))))

(defun company-box-icons--yasnippet (candidate)
  (when (get-text-property 0 'yas-annotation candidate)
    'Template))

(defun company-box-icons-resize (size &optional icons-images)
  "Set icons size in pixels."
  (interactive "nIcon size in pixels: ")
  (mapc (lambda (icon)
          (-> icon
              (plist-put :height size)
              (plist-put :width size)))
        (or icons-images company-box-icons-images)))

(provide 'company-box-icons)
;;; company-box-icons.el ends here
