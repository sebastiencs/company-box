;;; company-box.el --- Company front-end with icons  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Sebastien Chapuis

;; Author: Sebastien Chapuis <sebastien@chapu.is>
;; URL: https://github.com/sebastiencs/company-box
;; Keywords: company, completion, front-end, convenience
;; Package-Requires: ((emacs "26.0.91") (dash "2.19.0") (company "0.9.6") (frame-local "0.0.1"))
;; Version: 0.0.1

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
;;
;; Differences with the built-in front-end:
;;
;; - Different colors for differents backends.
;; - Icons associated to functions/variables/.. and their backends
;; - Display candidate's documentation (support quickhelp-string)
;; - Not limited by the current window size, buffer's text properties, ..
;;   (it's better than you might think)
;;
;; This package requires Emacs 26.
;; Also, not compatible with Emacs in a tty.
;;
;;
;; Installation:
;;
;; With use-package:
;; (use-package company-box
;;   :hook (company-mode . company-box-mode))
;; Or:
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)
;;
;; To customize:
;; M-x customize-group [RET] company-box [RET]
;;
;;
;; For more informations, see the homepage:
;; https://github.com/sebastiencs/company-box

;;; Code:

(require 'subr-x)
(require 'dash)
(require 'dash)
(require 'company)
(require 'company-box-icons)
(require 'company-box-doc)
(require 'frame-local)

(defgroup company-box nil
  "Front-end for Company."
  :prefix "company-box-"
  :group 'company)

(define-obsolete-face-alias 'company-box-annotation 'company-tooltip-annotation nil)
(define-obsolete-face-alias 'company-box-selection 'company-tooltip-selection nil)
(define-obsolete-face-alias 'company-box-background 'company-tooltip nil)
(define-obsolete-face-alias 'company-box-candidate 'company-tooltip nil)
(define-obsolete-face-alias 'company-box-numbers 'company-tooltip nil)
(make-obsolete-variable 'company-box-max-candidates nil "")
(make-obsolete-variable 'company-box-tooltip-minimum-width 'company-tooltip-minimum-width nil "")
(make-obsolete-variable 'company-box-tooltip-maximum-width 'company-tooltip-maximum-width nil "")

(defface company-box-candidate
  '((((background light)) :foreground "black")
    (t :foreground "white"))
  "Face used to color candidates."
  :group 'company-box)

(defface company-box-annotation
  '((t :inherit company-tooltip-annotation))
  "Face used to color annotations."
  :group 'company-box)

(defface company-box-selection
  '((t :inherit company-tooltip-selection :extend t))
  "Face used to color the selected candidate."
  :group 'company-box)

(defface company-box-background
  '((t :inherit company-tooltip))
  "Face used for frame's background.
Only the 'background' color is used in this face."
  :group 'company-box)

(defface company-box-scrollbar
  '((t :inherit company-tooltip-selection))
  "Face used for the scrollbar.
Only the 'background' color is used in this face."
  :group 'company-box)

(defface company-box-numbers
  '((t :inherit company-box-candidate))
  "Face used for numbers when `company-show-quick-access' is used."
  :group 'company-box)

(defcustom company-box-color-icon t
  "Whether or not to color icons.
Note that icons from images cannot be colored."
  :type 'boolean
  :group 'company-box)

(defcustom company-box-enable-icon t
  "Whether or not to display icons."
  :type 'boolean
  :group 'company-box)

(defcustom company-box-max-candidates 100
  "Maximum number of candidates.
A big number might slowndown the rendering.
To change the number of _visible_ chandidates, see `company-tooltip-limit'"
  :type 'integer
  :group 'company-box)

(defcustom company-box-tooltip-minimum-width 60
  "`company-box' minimum width."
  :type 'integer
  :group 'company-box)

(defcustom company-box-tooltip-maximum-width 260
  "`company-box' maximum width."
  :type 'integer
  :group 'company-box)

(defcustom company-box-show-single-candidate 'always
  "Whether or not to display the candidate if there is only one.
`when-no-other-frontend' will display the candidate if no other front ends are
detected."
  :type '(choice (const :tag "when-no-other-frontend" when-no-other-frontend)
                 (const :tag "never" never)
                 (const :tag "always" always))
  :group 'company-box)

(defcustom company-box-icons-functions
  '(company-box-icons--yasnippet company-box-icons--lsp company-box-icons--eglot company-box-icons--elisp company-box-icons--acphp company-box-icons--cider)
  "Functions to call on each candidate that should return an icon.
The functions takes 1 parameter, the completion candidate.

It should return an ICON or nil.
An ICON can be either a SYMBOL, an IMAGE, a LIST, a STRING:

- SYMBOL:  It is the name of the icon (from `company-box--icons-in-terminal').
- IMAGE:   An image descriptor [1]
           Example: '(image :type png :file \"/path/to/image.png\")
- LIST:    The list is then `apply' to `company-box--icons-in-terminal' function.
           Example: '(fa_icon :face some-face :foreground \"red\")
- STRING:  A simple string which is inserted, should be of length 1

If a function returns nil, it call the next function in the list.
If all functions returns nil, `company-box-icons-unknown' is used.

[1] https://www.gnu.org/software/emacs/manual/html_node/elisp/Image-Descriptors.html"
  :type 'list
  :group 'company-box)

(defcustom company-box-scrollbar t
  "Whether to draw the custom scrollbar or use default scrollbar.

- t means uses the custom scrollbar
- 'inherit uses same scrollbar than the current frame
- 'left or 'right puts default scrollbars to the left or right
- nil means draw no scrollbar."
  :type '(choice (const :tag "Custom scrollbar" t)
                 (const :tag "Inherit scrollbar" inherit)
                 (const :tag "Default scrollbar on left" left)
                 (const :tag "Default scrollbar on right" right)
                 (const :tag "No scrollbar" nil))
  :group 'company-box)

(defcustom company-box-frame-behavior 'default
  "Change frame position behavior."
  :type '(choice (const :tag "Default" default)
                 (const :tag "Follow point as you type" point))
  :group 'company-box)

(defcustom company-box-icon-right-margin 0
  "Set the space between the icon and the candidate text. It can be an integer
or a float number. For example, set `1' to add a space thats width is equal to a
character (see `frame-char-width'), set `0.5' to get half width of a character."
  :type 'number
  :group 'company-box)

(make-obsolete-variable 'company-box-highlight-prefix nil nil)

(defcustom company-box-highlight-prefix nil
  "[OBSOLETE] Highlight the prefix instead of common.
Faces used are `company-tooltip-common' and `company-tooltip-common-selection'
for both cases."
  :type 'boolean
  :safe #'booleanp
  :group 'company-box)

(defvar company-box-backends-colors
  '((company-yasnippet . (:all "lime green" :selected (:background "lime green" :foreground "black"))))
  "List of colors to use for specific backends.

Each element has the form (BACKEND . COLOR)

BACKEND is the backend's symbol for which the color applies
COLOR can be a LIST or a STRING:

- LIST:    A property list with the following keys:
                `:candidate'  : Color to use on the candidate
                `:annotation' : Color to use on the annotation
                `:icon'       : Color to use on the icon. Does nothing if the
                                icon is an image.
                `:all'        : Replace (:candidate X :annotation X :icon X)
           For those 4 attributes, values can be a face, a plist
           or a string (a color)
                `:selected'   : Color to use when the candidate is selected.
           It can be a plist or a face, not a string.
           It needs to define the background and foreground colors

- STRING:  A color string which is used everywhere
           (similar to (:all \"red\"))

Examples:

'((company-yasnippet . (:candidate \"yellow\" :annotation some-face))
  (company-elisp . (:icon \"yellow\" :selected (:background \"orange\"
                                              :foreground \"black\")))
  (company-dabbrev . \"purple\"))")


(defvar company-box-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
    (horizontal-scroll-bars . nil)
    (left-fringe . 0)
    (right-fringe . 0)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (line-spacing . 0)
    ;; (unsplittable . nil)
    (undecorated . t)
    (top . -1)
    (visibility . nil)
    (mouse-wheel-frame . nil)
    (no-other-frame . t)
    (cursor-type . nil)
    (drag-internal-border . t)
    (left-fringe . 0)
    (right-fringe . 0)
    (tab-bar-lines . 0)
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defvar company-box-debug-scrollbar nil)

(defvar-local company-box--ov nil)
(defvar-local company-box--ov-common nil)
(defvar-local company-box--max 0)
(defvar-local company-box--with-icons-p nil)
(defvar-local company-box--x nil)
(defvar-local company-box--top nil)
(defvar-local company-box--space nil)
(defvar-local company-box--height nil)
(defvar-local company-box--scrollbar-window nil)
(defvar-local company-box--parent-buffer nil)
(defvar-local company-box--chunk-size 0)

(defconst company-box--numbers
  (let ((vec (make-vector 20 nil)))
    (dotimes (index 20)
      (aset vec index
            (concat
             (string-trim (funcall company-quick-access-hint-function (mod index 10)))
             (and (> index 10) " "))))
    vec))

(defvar company-box-selection-hook nil
  "Hook run when the selection changed.")


(defalias 'company-box--icons-in-terminal
  (if (require 'icons-in-terminal nil t)
      'icons-in-terminal
    (lambda (&rest _) " ")))

(defun company-box--get-frame (&optional frame)
  "Return the company-box child frame on FRAME."
  (let ((frame (frame-local-getq company-box-frame frame)))
    (and (frame-live-p frame) frame)))

(defsubst company-box--set-frame (frame)
  "Set the frame symbol ‘company-box-frame’ to FRAME."
  (frame-local-setq company-box-frame frame))

(defun company-box--get-id nil
  (or (frame-local-getq company-box-buffer-id)
      (frame-local-setq company-box-buffer-id (or (frame-parameter nil 'window-id)
                                                  (frame-parameter nil 'name)))))

(defun company-box--get-buffer (&optional suffix)
  "Construct the buffer name, it should be unique for each frame."
  (get-buffer-create
   (concat " *company-box-" (company-box--get-id) suffix "*")))

(defun company-box--with-icons-p nil
  (let ((spaces (+ (- (current-column) (string-width company-prefix))
                   (/ (or (car (nth 2 (posn-at-point (line-beginning-position)))) 0)
                      (frame-char-width))
                   (car (window-edges nil t)))))
    (setq company-box--space spaces)
    (and company-box-enable-icon
         (> spaces 1))))

(defun company-box--make-scrollbar-parameter nil
  (cond ((eq company-box-scrollbar 'inherit) (frame-parameter nil 'vertical-scroll-bars))
        ((eq company-box-scrollbar 'left) 'left)
        ((eq company-box-scrollbar 'right) 'right)))

(defun company-box--make-frame (&optional buf)
  (let* ((after-make-frame-functions nil)
         (display-buffer-alist nil)
         (before-make-frame-hook nil)
         (buffer (or buf (company-box--get-buffer)))
         (params (append company-box-frame-parameters
                         `((vertical-scroll-bars . ,(company-box--make-scrollbar-parameter))
                           (default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (inhibit-double-buffering . t)
                           (background-color . ,(face-background 'company-tooltip nil t)))))
         (window (display-buffer-in-child-frame buffer `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (frame-local-setq company-box-buffer buffer frame)
    (set-frame-parameter frame 'desktop-dont-save t)
    (unless buf
      (frame-local-setq company-box-window window))
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    (set-frame-parameter frame 'name "")
    frame))

(defun company-box--get-ov nil
  (or company-box--ov
      (setq company-box--ov (make-overlay 1 1))))

(defun company-box--get-ov-common nil
  (or company-box--ov-common
      (setq company-box--ov-common (make-overlay 1 1))))

(defun company-box--extract-background (color)
  "COLOR can be a string, face or plist."
  `(:background
    ,(or (and (stringp color) color)
         (and (facep color) (face-background color nil t))
         (let ((back (plist-get color :background)))
           (if (facep back) (face-background back nil t) back)))))

(defun company-box--update-image (&optional color)
  "Change the image background color, because the overlay doesn't apply on it.
The function either restore the original image or apply the COLOR.
It doesn't nothing if a font icon is used."
  (-when-let* ((bol (line-beginning-position))
               (point (text-property-any bol (min (+ bol 2) (point-max)) 'company-box-image t))
               (image (get-text-property point 'display-origin))
               (new-image (append image (and color (company-box--extract-background color)))))
    (put-text-property point (1+ point) 'display new-image)))

(defvar-local company-box--numbers-pos nil)

(defun company-box--remove-numbers (&optional side)
  (let ((side (or side (if (eq company-show-quick-access 'left) 'left-margin 'right-margin)))
        (max (point-max)))
    (--each company-box--numbers-pos
      (and (< it max)
           (get-text-property it 'company-box--number-pos)
           (put-text-property it (1+ it) 'display `((margin ,side) "  "))))
    (setq company-box--numbers-pos nil)))

(defun company-box--update-numbers (start)
  (let ((side (if (eq company-show-quick-access 'left) 'left-margin 'right-margin))
        (offset (if (eq company-show-quick-access 'left) 0 10))
        (inhibit-redisplay t)
        (inhibit-modification-hooks t))
    (company-box--remove-numbers side)
    (dotimes (index 10)
      (-some--> start
        (if (get-text-property it 'company-box--number-pos)
            it
          (next-single-property-change it 'company-box--number-pos))
        (progn
          (push it company-box--numbers-pos)
          (setq start (1+ it)))
        (put-text-property (1- it) it 'display `((margin ,side) ,(aref company-box--numbers (+ index offset))))))))

(defun company-box--maybe-move-number (start)
  (when company-show-quick-access
    (company-box--update-numbers start)))

(defvar-local company-box--last-scroll 0)
(defvar-local company-box--last-start nil)

(defun company-box--handle-scroll (_win new-start)
  (setq company-box--last-start new-start)
  (when company-box--x
    (when (>= (abs (- company-box--last-scroll (or company-selection 0)))
              company-box--chunk-size)
      (company-box--ensure-full-window-is-rendered new-start))
    (setq company-box--last-scroll (or company-selection 0))
    (company-box--maybe-move-number new-start)
    (company-box--set-width new-start)))

(defun company-box--move-overlay-no-selection nil
  (goto-char 1)
  (move-overlay (company-box--get-ov) 1 1)
  (move-overlay (company-box--get-ov-common) 1 1))

(defun company-box--end-of-common (start eol)
  (while (let ((face (get-text-property start 'face)))
           (and (or (eq face 'company-tooltip-common)
                    (and (listp face)
                         (memq 'company-tooltip-common face)))
                (not (eq start eol))))
    (setq start (next-single-property-change start 'face nil eol)))
  start)

(defun company-box--move-overlays (selection &optional new-point)
  (if (null selection)
      (company-box--move-overlay-no-selection)
    (company-box--update-image)
    (goto-char (if new-point new-point (company-box--point-at-line selection)))
    (let* ((bol (line-beginning-position))
           (eol (line-beginning-position 2))
           (inhibit-modification-hooks t)
           (start-common (next-single-property-change bol 'company-box--candidate-string nil eol))
           (end-common (company-box--end-of-common start-common eol)))
      (move-overlay (company-box--get-ov) bol eol)
      (move-overlay (company-box--get-ov-common) start-common end-common))
    (let ((color (or (get-text-property (point) 'company-box--color)
                     'company-tooltip-selection))
          (inhibit-modification-hooks t))
      (overlay-put (company-box--get-ov) 'face color)
      (overlay-put (company-box--get-ov-common) 'face 'company-tooltip-common-selection)
      (company-box--update-image color))))


(defun company-box--get-candidates-between (start end)
  (let ((index 0)
        (vector (make-vector (max (- end start) 1) nil)))
    (while (< start end)
      (-when-let* ((candidate (get-text-property start 'company-box-candidate)))
        (aset vector index candidate)
        (setq index (1+ index)))
      (setq start (1+ start)))
    ;; Return nil when the vector is empty
    (and (aref vector 0) vector)))

(defvar-local company-box--first-render nil)

(defun company-box--get-start (point height)
  (previous-single-property-change
   (1+ point) 'company-box--rendered nil (max 1 (- point height))))

(defun company-box--get-end (point height)
  (next-single-property-change
   point 'company-box--rendered nil (min (point-max) (+ point height))))

(defun company-box--render-lines (point &optional no-remove-numbers)
  (when-let* ((height (1+ company-box--chunk-size))
              (start (company-box--get-start point height))
              (end (company-box--get-end point height))
              (candidates (company-box--get-candidates-between start end))
              (inhibit-modification-hooks t)
              (inhibit-redisplay t))
    (unless no-remove-numbers
      (company-box--remove-numbers))
    (save-excursion
      (delete-region start end)
      (goto-char start)
      (insert
       (with-current-buffer company-box--parent-buffer
         (--> candidates
              (mapcar (-compose 'company-box--make-line 'company-box--make-candidate) it)
              (mapconcat 'identity it "\n")))
       "\n")
      (put-text-property start (point) 'company-box--rendered t))))

(defun company-box--render-buffer (string on-update)
  (let ((buffer (current-buffer))
        (inhibit-modification-hooks t)
        (candidates-length company-candidates-length)
        (show-numbers company-show-quick-access)
        (with-icons-p company-box--with-icons-p)
        (window-configuration-change-hook nil)
        (buffer-list-update-hook nil))
    (with-current-buffer (company-box--get-buffer)
      (erase-buffer)
      (insert string)
      (put-text-property (point-min) (point-max) 'company-box--rendered nil)
      (setq company-box--first-render t
            company-candidates-length candidates-length
            company-show-quick-access show-numbers
            company-box--with-icons-p with-icons-p)
      (unless on-update
        (setq mode-line-format nil
              header-line-format nil
              display-line-numbers nil
              truncate-lines t
              show-trailing-whitespace nil
              company-box--parent-buffer buffer
              company-box--first-render t
              cursor-in-non-selected-windows nil)
        (setq-local scroll-step 1)
        (setq-local scroll-conservatively 100000)
        (setq-local scroll-margin 0)
        (setq-local bidi-display-reordering nil)
        (setq-local redisplay--inhibit-bidi t)
        ;; (setq-local next-screen-context-lines 0)
        (setq-local scroll-preserve-screen-position t)
        (setq-local fontification-functions nil)
        (setq-local window-scroll-functions '(company-box--handle-scroll))
        ;; (setq-local pre-redisplay-function '(company-box--handle-state-changed))
        ;; (setq-local pre-redisplay-functions '(company-box--handle-state-changed))
        ;;(setq-local window-state-change-functions '(company-box--handle-state-changed))
        ;;(setq-local window-state-change-hook '(company-box--handle-state-changed))
        ;; (setq-local company-box--chunk-size (or 10 (frame-height) 50))
        ;; (jit-lock-mode 1)
        (add-hook 'window-configuration-change-hook 'company-box--prevent-changes t t)
        ))))

(defvar-local company-box--bottom nil)

(defun company-box--point-bottom nil
  (or company-box--bottom
      (setq company-box--bottom
            (let* ((win (let ((tmp nil))
                          (while (window-in-direction 'below tmp)
                            (setq tmp (window-in-direction 'below tmp)))
                          tmp)))
              (+ (or (nth 2 (window-line-height 'mode-line win))
                     (- (frame-pixel-height) (* (frame-char-height) 3)))
                 (or (and win (nth 1 (window-edges win t nil t)))
                     0))))))

(defvar-local company-box--prefix-pos nil)
(defvar-local company-box--edges nil)

(defun company-box--prefix-pos nil
  (if (eq company-box-frame-behavior 'point)
      (nth 2 (posn-at-point (point)))
    (or company-box--prefix-pos
        (setq company-box--prefix-pos
              (nth 2 (posn-at-point (- (point) (length company-prefix))))))))

(defun company-box--edges nil
  (or company-box--edges
      (setq company-box--edges (window-edges nil t nil t))))

(defun company-box--compute-frame-position (frame)
  (-let* ((window-configuration-change-hook nil)
          ((left top _right _bottom) (company-box--edges))
          (window-tab-line-height (if (fboundp 'window-tab-line-height)
                                      (window-tab-line-height)
                                    0))
          (top (+ top window-tab-line-height))
          (char-height (frame-char-height frame))
          (char-width (frame-char-width frame))
          (height (* (min company-candidates-length company-tooltip-limit) char-height))
          (space-numbers (if (eq company-show-quick-access 'left) char-width 0))
          (frame-resize-pixelwise t)
          (mode-line-y (company-box--point-bottom))
          ((p-x . p-y) (company-box--prefix-pos))
          (p-y-abs (+ top p-y))
          (y (or (and (> p-y-abs (/ mode-line-y 2))
                      (<= (- mode-line-y p-y) (+ char-height height))
                      (> (- p-y-abs height) 0)
                      (- p-y height))
                 (+ p-y char-height)))
          (height (or (and (> y p-y)
                           (> height (- mode-line-y y))
                           (- mode-line-y y))
                      height))
          (height (- height (mod height char-height)))
          (scrollbar-width (if (eq company-box-scrollbar 'left) (frame-scroll-bar-width frame) 0))
          (x (if (eq company-box-frame-behavior 'point)
                 p-x
               (if company-box--with-icons-p
                   (- p-x (* char-width (+ company-box-icon-right-margin (if (= company-box--space 2) 2 3))) space-numbers scrollbar-width)
                 (- p-x (if (= company-box--space 0) 0 char-width) space-numbers scrollbar-width)))))
    (setq company-box--x (max (+ x left) 0)
          company-box--top (+ y top)
          company-box--height height
          company-box--chunk-size (/ height char-height))
    (with-current-buffer (company-box--get-buffer)
      (setq company-box--x (max (+ x left) 0)
            company-box--top (+ y top)
            company-box--height height
            company-box--chunk-size (/ height char-height)))))

(defun company-box--update-frame-position (frame)
  (-let (((new-x . width) (company-box--set-width nil t))
         (window-configuration-change-hook nil)
         (buffer-list-update-hook nil)
         (inhibit-redisplay t))
    (frame-local-setq company-box-window-origin (selected-window) frame)
    (frame-local-setq company-box-buffer-origin (current-buffer) frame)
    (modify-frame-parameters
     frame
     `((width . (text-pixels . ,width))
       (height . (text-pixels . ,company-box--height))
       (user-size . t)
       (left . (+ ,(round (or new-x company-box--x))))
       (top . (+ ,company-box--top))
       (user-position . t)
       (right-fringe . 0)
       (left-fringe . 0)))))

(defun company-box--display (string on-update)
  "Display the completions."
  (company-box--render-buffer string on-update)
  (unless (company-box--get-frame)
    (company-box--set-frame (company-box--make-frame)))
  (company-box--compute-frame-position (company-box--get-frame))
  (company-box--move-selection t)
  (company-box--update-frame-position (company-box--get-frame))
  (unless (frame-visible-p (company-box--get-frame))
    (make-frame-visible (company-box--get-frame)))
  (company-box--update-scrollbar (company-box--get-frame) t)
  (with-current-buffer (company-box--get-buffer)
    (company-box--maybe-move-number (or company-box--last-start 1))))

(defun company-box--get-kind (candidate)
  (let ((list company-box-icons-functions)
        kind)
    (while (and (null kind) list)
      (setq kind (funcall (car list) candidate))
      (pop list))
    (or kind 'Unknown)))

(defun company-box--get-icon (icon)
  (cond ((listp icon)
         (cond ((eq 'image (car icon))
                (unless (plist-get icon :height)
                  (setq icon (append icon `(:height ,(round (* (frame-char-height) 0.90))))))
                (propertize " " 'display icon 'company-box-image t
                            'display-origin icon))
               ((and company-box-color-icon icon)
                (apply 'company-box--icons-in-terminal icon))
               (t (company-box--icons-in-terminal (or (car icon) 'fa_question_circle)))))
        ((symbolp icon)
         (company-box--icons-in-terminal (or icon 'fa_question_circle)))
        (t icon)))

(defun company-box--add-icon (candidate)
  (-let* ((icon (alist-get (company-box--get-kind candidate)
                           (symbol-value company-box-icons-alist)))
          (is-image (and (listp icon) (eq 'image (car icon))))
          (icon-string (company-box--get-icon icon))
          (display-props (unless is-image (get-text-property 0 'display icon-string))))
    (concat
     (cond (is-image icon-string)
           (display-props
            ;; The string already has a display prop, add height to it
            (--> (if (listp (car display-props))
                     (append display-props '((height 0.95)))
                   (append `(,display-props) '((height 0.95))))
                 (put-text-property 0 (length icon-string) 'display it icon-string))
            icon-string)
           (t
            ;; Make sure the string is not bigger than other text.
            ;; It causes invalid computation of the frame size, ..
            (put-text-property 0 (length icon-string) 'display '((height 0.95)) icon-string)
            icon-string))
     (propertize " " 'display `(space :align-to (+ company-box-icon-right-margin
                                                   left-fringe
                                                   ,(if (> company-box--space 2) 3 2)))))))

(defun company-box--get-color (backend)
  (alist-get backend company-box-backends-colors))

(defun company-box--resolve-color (color key)
  (or (and (stringp color) color)
      (and (listp color) (or (plist-get color key) (plist-get color :all)))))

(defun company-box--resolve-colors (color)
  (when color
    (list
     (company-box--resolve-color color :candidate)
     (company-box--resolve-color color :annotation)
     (company-box--resolve-color color :icon)
     (let ((color (company-box--resolve-color color :selected)))
       (unless (stringp color)
         color)))))

(defun company-box--apply-color (string color)
  (when color
    (add-face-text-property 0 (length string)
                            (if (stringp color) (list :foreground color) color)
                            nil string))
  string)

(defun company-box--candidate-string (candidate length-candidate)
  (let* ((company-tooltip-align-annotations nil)
         (company-tooltip-margin 0)
         (company-candidates nil)
         (string (-> (company--clean-string candidate)
                     (company-fill-propertize nil length-candidate nil nil nil))))
    (add-text-properties 0 (length string) '(company-box--candidate-string t) string)
    string))

(defun company-box--make-number-prop nil
  (let ((side (if (eq company-show-quick-access 'left) 'left-margin 'right-margin)))
    (propertize " " 'company-box--number-pos t 'display `((margin ,side) "  "))))

(defun company-box--make-line (candidate)
  (-let* (((candidate annotation len-c len-a backend) candidate)
          (color (company-box--get-color backend))
          ((c-color a-color i-color s-color) (company-box--resolve-colors color))
          (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
          (candidate-string (company-box--candidate-string candidate len-c))
          (align-string (when annotation
                          (concat " " (and company-tooltip-align-annotations
                                           (propertize " " 'display `(space :align-to (- right-margin ,len-a 1)))))))
          (space company-box--space)
          (icon-p company-box-enable-icon)
          (annotation-string (and annotation (propertize annotation 'face 'company-tooltip-annotation)))
          (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                          (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                        (company-box--apply-color icon-string i-color)
                        (when company-show-quick-access
                          (company-box--make-number-prop))
                        (company-box--apply-color candidate-string c-color)
                        align-string
                        (company-box--apply-color annotation-string a-color)
                        ;; Trick to make sure the selection face goes until the end, even without :extend
                        (propertize " " 'display `(space :align-to right-fringe))))
          (len (length line)))
    (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                     'company-box--color s-color
                                     'mouse-face 'company-tooltip-mouse)
                         line)
    line))

(defun company-box--backend (candidate)
  (or (get-text-property 0 'company-backend candidate)
      (and (symbolp company-backend) company-backend)
      (--first (and it (not (keywordp it))) company-backend)))

(defun company-box--make-candidate (candidate)
  (let* ((annotation (-some->> (company-call-backend 'annotation candidate)
                       (replace-regexp-in-string "[ \t\n\r]+" " ")
                       (string-trim)))
         (len-candidate (string-width candidate))
         (len-annotation (if annotation (string-width annotation) 0))
         (len-total (+ len-candidate len-annotation))
         (backend (company-box--backend candidate)))
    (when (> len-total company-box--max)
      (setq company-box--max len-total))
    (list candidate
          annotation
          len-candidate
          len-annotation
          backend)))

(defvar-local company-box--parent-start nil)

(defun company-box-show (&optional on-update)
  (unless on-update
    (setq company-box--parent-start (window-start))
    (add-hook 'window-scroll-functions 'company-box--handle-scroll-parent nil t))
  (company-box--save)
  (setq company-box--max 0
        company-box--with-icons-p (company-box--with-icons-p))
  (let ((string (make-string company-candidates-length 10)))
    (--each-indexed company-candidates
      (put-text-property it-index (1+ it-index) 'company-box-candidate it string))
    (company-box--display string on-update)))

(defvar company-box-hide-hook nil)

(defun company-box-hide nil
  (setq company-box--bottom nil
        company-box--x nil
        company-box--prefix-pos nil
        company-box--last-start nil
        company-box--edges nil)
  (-some-> (company-box--get-frame)
    (make-frame-invisible))
  (with-current-buffer (company-box--get-buffer)
    (setq company-box--last-start nil))
  (remove-hook 'window-scroll-functions 'company-box--handle-scroll-parent t)
  (run-hook-with-args 'company-box-hide-hook (or (frame-parent) (selected-frame))))

(defun company-box--calc-len (buffer start end char-width)
  (let ((max 0))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((len (or (get-text-property (point) 'company-box--len) 0)))
            (when (> len max)
              (setq max len)))
          (forward-line))))
    (* (+ max (if company-box--with-icons-p 6 2) (if company-show-quick-access 2 0))
       char-width)))

(defun company-box--get-start-end-for-width (win win-start)
  (let ((height company-box--chunk-size)
        (selection (or company-selection 0))
        (box-buffer (window-buffer win)))
    (if win-start
        (cons win-start (with-current-buffer box-buffer
                          (company-box--point-at-line height win-start)))
      ;; When window-start is not known, we take the points (selection - height)
      ;; and (selection + height)
      (with-current-buffer box-buffer
        (let ((start (company-box--point-at-line (- selection height))))
          (cons start (company-box--point-at-line height start)))))))

(defun company-box--set-width (&optional win-start value-only)
  (-let* ((inhibit-redisplay t)
          (frame (company-box--get-frame (frame-parent)))
          (window (frame-local-getq company-box-window (frame-parent)))
          (char-width (frame-char-width frame))
          ((start . end) (company-box--get-start-end-for-width window win-start))
          (width (+ (company-box--calc-len (window-buffer window) start end char-width)
                    (if (and (eq company-box-scrollbar t) (company-box--scrollbar-p frame)) (* 2 char-width) 0)
                    char-width))
          (width (max (min width
                           (* company-tooltip-maximum-width char-width))
                      (* company-tooltip-minimum-width char-width)))
          (diff (abs (- (frame-pixel-width frame) width)))
          (frame-width (frame-pixel-width (frame-parent)))
          (new-x (and (> (+ width company-box--x) frame-width)
                      (max 0 (- frame-width width char-width)))))
    (when company-box-debug-scrollbar
      (message "[RESIZE] NEW-WIDTH=%s NEW-PIXEL=%s OLD=%s OLD-PIXEL=%s"
               (/ width char-width) width (frame-parameter frame 'width) (frame-pixel-width frame)))
    (or (and value-only (cons new-x width))
        (and (> diff 2)
             (modify-frame-parameters
              frame
              `((width . (text-pixels . ,width))
                (left . (+ ,(round (or new-x company-box--x))))))))))

(defun company-box--percent (a b)
  (/ (float a) (float b)))

(defun company-box--scrollbar-p (frame)
  (/= 1 (company-box--percent
         company-box--height
         (* company-candidates-length
            (frame-char-height frame)))))

(defun company-box--scrollbar-prevent-changes (&rest _)
  (when company-box-debug-scrollbar
    (message "[CHANGES] CURRENT-BUFFER=%s MIN-WIDTH=%s SAFE-MIN-WIDTH=%s MIN-SIZE=%s MIN-SIZE-IGNORE=%s"
             (current-buffer) window-min-width window-safe-min-width
             (window-min-size nil t) (window-min-size nil t t)))
  (let ((window-min-width 2)
        (window-safe-min-width 2)
        (ignore-window-parameters t)
        (current-size (window-size nil t)))
    (when company-box-debug-scrollbar
      (message "[CHANGES] MIN CURRENT-SIZE=%s WIN-MIN-SIZE=%s WIN-PARAMS=%s FRAME-PARAMS=%s HOOKS=%s"
               current-size (window-min-size nil t) (window-parameters) (frame-parameters (company-box--get-frame))
               window-configuration-change-hook))
    (unless (= current-size 2)
      (minimize-window))))

(defun company-box--update-scrollbar-buffer (height-blank height-scrollbar percent buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (setq header-line-format nil
          mode-line-format nil
          show-trailing-whitespace nil
          cursor-in-non-selected-windows nil)
    (setq-local window-min-width 2)
    (setq-local window-safe-min-width 2)
    (unless (zerop height-blank)
      (insert (propertize " " 'display `(space :align-to right-fringe :height ,height-blank))
              (propertize "\n" 'face (list :height 1))))
    (setq height-scrollbar (if (= percent 1)
                               ;; Due to float/int casting in the emacs code, there might 1 or 2
                               ;; remainings pixels
                               (+ height-scrollbar 10)
                             height-scrollbar))
    (insert (propertize " " 'face (list :background (face-background 'company-box-scrollbar nil t))
                        'display `(space :align-to right-fringe :height ,height-scrollbar)))
    (add-hook 'window-configuration-change-hook 'company-box--scrollbar-prevent-changes t t)
    (current-buffer)))

(defun company-box--update-scrollbar (frame &optional first)
  (when (eq company-box-scrollbar t)
    (let* ((selection (or company-selection 0))
           (buffer (company-box--get-buffer "-scrollbar"))
           (h-frame company-box--height)
           (n-elements company-candidates-length)
           (percent (company-box--percent selection (1- n-elements)))
           (percent-display (company-box--percent h-frame (* n-elements (frame-char-height frame))))
           (scrollbar-pixels (* h-frame percent-display))
           (height-scrollbar (/ scrollbar-pixels (frame-char-height frame)))
           (blank-pixels (* (- h-frame scrollbar-pixels) percent))
           (height-blank (/ blank-pixels (frame-char-height frame)))
           (inhibit-redisplay t)
           (inhibit-eval-during-redisplay t)
           (window-configuration-change-hook nil)
           (window-scroll-functions nil))
      (when company-box-debug-scrollbar
        (message "[SCROLL] SELECTION=%s BUFFER=%s H-FRAME=%s N-ELEMENTS=%s PERCENT=%s PERCENT-DISPLAY=%s SCROLLBAR-PIXEL=%s HEIGHT=SCROLLBAR=%s BLANK-PIXELS=%s HEIGHT-BLANK=%s FRAME-CHAR-HEIGHT=%s FRAME-CHAR-HEIGHT-NO-FRAME=%s FRAME=%s MUL=%s"
                 selection buffer h-frame n-elements percent percent-display scrollbar-pixels height-scrollbar blank-pixels height-blank (frame-char-height frame) (frame-char-height) frame (* n-elements (frame-char-height frame))))
      (cond
       ((and first (= percent-display 1) (window-live-p company-box--scrollbar-window))
        (delete-window company-box--scrollbar-window))
       ((window-live-p company-box--scrollbar-window)
        (company-box--update-scrollbar-buffer height-blank height-scrollbar percent buffer))
       ((/= percent-display 1)
        (setq
         company-box--scrollbar-window
         (with-selected-frame (company-box--get-frame)
           (let* ((window-min-width 2)
                  (window-safe-min-width 2)
                  (window-configuration-change-hook nil)
                  (display-buffer-alist nil)
                  (window-scroll-functions nil))
             (display-buffer-in-side-window
              (company-box--update-scrollbar-buffer height-blank height-scrollbar percent buffer)
              '((side . right) (window-width . 2))))))
        (frame-local-setq company-box-scrollbar (window-buffer company-box--scrollbar-window) frame))))))

;; ;; (message "selection: %s len: %s PERCENT: %s PERCENTS-DISPLAY: %s SIZE-FRAME: %s HEIGHT-S: %s HEIGHT-B: %s h-frame: %s sum: %s"
;; ;;          selection n-elements percent percent-display height height-scrollbar height-blank height (+ height-scrollbar height-blank))
;; ;; (message "HEIGHT-S-1: %s HEIGHT-B-1: %s sum: %s" scrollbar-pixels blank-pixels (+ height-scrollbar-1 height-blank-1))

(defun company-box--point-at-line (&optional line start)
  (save-excursion
    (goto-char (or start 1))
    (forward-line (or line company-selection 0))
    (point)))

(defun company-box--move-selection (&optional first-render)
  (let ((selection company-selection)
        (candidates-length company-candidates-length)
        (inhibit-redisplay t)
        (inhibit-modification-hooks t)
        (buffer-list-update-hook nil)
        (window-configuration-change-hook nil))
    (with-selected-window (get-buffer-window (company-box--get-buffer) t)
      (setq company-selection selection)
      (let ((new-point (company-box--point-at-line selection))
            (buffer-list-update-hook nil)
            (window-configuration-change-hook nil))
        (cond ((and (> new-point 1) (null (get-text-property (1- new-point) 'company-box--rendered)))
               ;; When going backward, render lines not yet visible
               ;; This avoid to render the lines when it's already visible
               ;; causing window-start to jump
               (company-box--render-lines (1- new-point))
               (company-box--move-overlays selection))
              ((get-text-property new-point 'company-box--rendered)
               ;; Line is already rendered, just move overlays
               (company-box--move-overlays selection new-point))
              (t
               ;; Line is not rendered at point
               (company-box--render-lines new-point)
               (company-box--move-overlays selection))))
      (when (equal selection (1- candidates-length))
        ;; Ensure window doesn't go past last candidate
        (--> (- company-box--chunk-size)
             (company-box--point-at-line it (point-max))
             (set-window-start nil it))))
    (unless first-render
      (company-box--update-scrollbar (company-box--get-frame) first-render))
    (run-with-idle-timer 0 nil (lambda nil (run-hook-with-args 'company-box-selection-hook selection
                                                               (or (frame-parent) (selected-frame)))))))

(defun company-box--prevent-changes (&rest _)
  (set-window-margins
   nil
   (if (eq company-show-quick-access 'left) 1 0)
   (if (eq company-show-quick-access 't) 2 0)))

(defun company-box--handle-window-changes (&optional on-idle)
  (-when-let* ((frame (company-box--get-frame)))
    (and (frame-live-p frame)
         (frame-visible-p frame)
         (or (not (eq (selected-window) (frame-local-getq company-box-window-origin frame)))
             (not (eq (window-buffer) (frame-local-getq company-box-buffer-origin frame))))
         (if on-idle (company-box-hide)
           ;; Handle when this function (in `buffer-list-update-hook') has been
           ;; triggered by a function that select only temporary another window/buffer.
           ;; So we are sure to not be in a false positive case.
           ;; See the docstring of `select-window'
           (run-with-idle-timer 0 nil (lambda nil (company-box--handle-window-changes t)))))))

(defun company-box--hide-single-candidate nil
  (or (eq company-box-show-single-candidate 'never)
      (and (eq company-box-show-single-candidate 'when-no-other-frontend)
           (cdr company-frontends))))

(defvar-local company-box--state nil)

(defun company-box--save nil
  (setq company-box--state
        (list company-prefix
              company-common
              company-search-string
              company-candidates-length
              ;; company-candidates
              )))

(defun company-box--update nil
  (-let* (((prefix common search length) company-box--state)
          (frame (company-box--get-frame))
          (window-configuration-change-hook nil)
          (frame-visible (and (frame-live-p frame) (frame-visible-p frame))))
    (if (and frame-visible
             (equal search company-search-string)
             (equal length company-candidates-length)
             (string= company-prefix prefix)
             (string= company-common common))
        (company-box--move-selection)
      (company-box-show frame-visible))))

(defun company-box--handle-scroll-parent (win new-start)
  (when (and (eq (frame-local-getq company-box-window-origin (company-box--get-frame)) win)
             (not (equal company-box--parent-start new-start)))
    (company-box--on-start-change)
    (setq company-box--parent-start new-start)))

(defvar company-mouse-event)
(defun company-box--select-mouse ()
  "Select the candidate from `company-mouse-event'."
  (let ((posn (event-end company-mouse-event)))
    (when (eq (company-box--get-buffer) (window-buffer (posn-window posn)))
      (setq company-selection
            (with-current-buffer (company-box--get-buffer)
              (1- (line-number-at-pos (posn-point posn)))))
      (company-box--move-selection)
      ;; success
      t)))

(defun company-box-frontend (command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  ;; (message "\nCOMMMAND: %s last=%s this=%s" command last-command this-command)
  ;; (message "prefix: %s" company-prefix)
  ;; (message "candidates: %s" company-candidates)
  ;; (message "common: %s" company-common)
  ;; (message "selection: %s" company-selection)
  ;; (message "point: %s" company-point)
  ;; (message "search-string: %s" company-search-string)
  ;;(message "last-command: %s" last-command)
  (cond
   ((eq command 'hide)
    (company-box-hide))
   ((and (equal company-candidates-length 1)
         (company-box--hide-single-candidate))
    (company-box-hide))
   ((eq command 'show)
    (company-box-show))
   ((eq command 'update)
    (company-box--update))
   ((eq command 'select-mouse)
    (company-box--select-mouse))
   ;; ((eq command 'post-command)
   ;;  (company-box--post-command))
   ))

(defun company-box--ensure-full-window-is-rendered (&optional start)
  (let ((window-configuration-change-hook nil)
        (buffer-list-update-hook nil))
    (with-selected-window (get-buffer-window (company-box--get-buffer) t)
      (let* ((start (or start (window-start)))
             (line-end company-box--chunk-size)
             (end (company-box--point-at-line line-end start))
             (nlines (- end start)))
        (dotimes (index nlines)
          (unless (get-text-property (- end (1+ index)) 'company-box--rendered)
            (company-box--render-lines (- end (1+ index)) t)))))))

(defun company-box--on-start-change nil
  (setq company-box--prefix-pos nil
        company-box--edges nil)
  (let ((frame (company-box--get-frame))
        (inhibit-redisplay t)
        (inhibit-modification-hooks t)
        (window-scroll-functions nil))
    (when (and (frame-live-p frame) (frame-visible-p frame))
      (company-box--compute-frame-position frame)
      (company-box--ensure-full-window-is-rendered)
      (company-box--update-frame-position frame))))

(defun company-box--delete-frame ()
  "Delete the child frame if it exists."
  (-when-let (frame (company-box--get-frame))
    (and (frame-live-p frame)
         (delete-frame frame))
    (company-box--set-frame nil)))

(defun company-box--kill-delay (buffer)
  (run-with-idle-timer
   0 nil (lambda nil
           (when (buffer-live-p buffer)
             (kill-buffer buffer)))))

(defun company-box--kill-buffer (frame)
  (company-box--kill-delay (frame-local-getq company-box-buffer frame))
  (company-box--kill-delay (frame-local-getq company-box-scrollbar frame)))

(defun company-box--is-box-buffer (&optional buffer)
  (or (and buffer (eq buffer (frame-local-getq company-box--dimmer-parent (frame-parent))))
      (string-prefix-p " *company-box" (buffer-name (or buffer (window-buffer))))))

(defun company-box--dimmer-show (&rest _)
  (frame-local-setq company-box--dimmer-parent (window-buffer)))

(defun company-box--dimmer-hide (&rest _)
  (frame-local-setq company-box--dimmer-parent nil))

(defun company-box--handle-theme-change (&rest _)
  ;; Deleting frames will force to rebuild them from scratch
  ;; and use the correct new colors
  (company-box-doc--delete-frame)
  (company-box--delete-frame))

(defun company-box--tweak-external-packages nil
  (with-eval-after-load 'dimmer
    (when (boundp 'dimmer-prevent-dimming-predicates)
      (add-to-list
       'dimmer-prevent-dimming-predicates
       'company-box--is-box-buffer))
    (when (boundp 'dimmer-buffer-exclusion-predicates)
      (add-to-list
       'dimmer-buffer-exclusion-predicates
       'company-box--is-box-buffer))
    (advice-add 'load-theme :before 'company-box--handle-theme-change)
    (advice-add 'company-box-show :before 'company-box--dimmer-show)
    (advice-add 'company-box-hide :before 'company-box--dimmer-hide))
  (with-eval-after-load 'golden-ratio
    (when (boundp 'golden-ratio-exclude-buffer-regexp)
      (add-to-list 'golden-ratio-exclude-buffer-regexp " *company-box"))))

(defun company-box--set-mode (&optional frame)
  (cond
   ((and (bound-and-true-p company-box-mode) (not (display-graphic-p frame)))
    (company-box-mode -1))
   ((bound-and-true-p company-box-mode)
    (company-box--tweak-external-packages)
    (remove-hook 'after-make-frame-functions 'company-box--set-mode t)
    (add-hook 'delete-frame-functions 'company-box--kill-buffer)
    (add-hook 'buffer-list-update-hook 'company-box--handle-window-changes t)
    (make-local-variable 'company-frontends)
    (setq company-frontends (->> (delq 'company-pseudo-tooltip-frontend company-frontends)
                                 (delq 'company-pseudo-tooltip-unless-just-one-frontend)))
    (add-to-list 'company-frontends 'company-box-frontend)
    (unless (assq 'company-box-frame frameset-filter-alist)
      (push '(company-box-doc-frame . :never) frameset-filter-alist)
      (push '(company-box-frame . :never) frameset-filter-alist)))
   ((memq 'company-box-frontend company-frontends)
    (setq company-frontends (delq 'company-box-frontend  company-frontends))
    (add-to-list 'company-frontends 'company-pseudo-tooltip-unless-just-one-frontend))))

(add-hook 'company-box-selection-hook 'company-box-doc)
(add-hook 'company-box-hide-hook 'company-box-doc--hide)

;;;###autoload
(define-minor-mode company-box-mode
  "Company-box minor mode."
  :group 'company-box
  :lighter " company-box"
  ;; With emacs daemon and:
  ;; `(add-hook 'company-mode-hook 'company-box-mode)'
  ;; `company-box-mode' is called too early to know if we are in a GUI
  (if (and (daemonp)
           (not (frame-parameter nil 'client))
           company-box-mode)
      (add-hook 'after-make-frame-functions 'company-box--set-mode t t)
    (company-box--set-mode)))

(provide 'company-box)
;;; company-box.el ends here
