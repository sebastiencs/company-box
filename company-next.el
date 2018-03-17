;;; company-next.el --- Company front-end  -*- lexical-binding: t -*-

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
(require 'company)
(require 'company-next-icons)

(defgroup company-next nil
  "Front-end for Company."
  :prefix "company-next-"
  :group 'company)

(defface company-next-candidate
  '((t :foreground "white"))
  "Face used to color candidates."
  :group 'company-next)

(defface company-next-annotation
  '((t :inherit company-tooltip-annotation))
  "Face used to color annotations."
  :group 'company-next)

(defface company-next-selection
  '((t :inherit company-tooltip-selection))
  "Face used to color the selected candidate."
  :group 'company-next)

(defface company-next-background
  '((t :background "#2B303B"))
  "Face used for frame's background.
Only the 'background' color is used in this face."
  :group 'company-next)

(defface company-next-scrollbar
  '((t :inherit company-next-selection))
  "Face used for the scrollbar.
Only the 'background' color is used in this face."
  :group 'company-next)

(defcustom company-next-align-annotations company-tooltip-align-annotations
  "When non-nil, align annotations to the right border."
  :type 'boolean
  :group 'company-next)

(defcustom company-next-color-icon t
  "Whether or not to color icons."
  :type 'boolean
  :group 'company-next)

(defcustom company-next-enable-icon t
  "Whether or not to display icons."
  :type 'boolean
  :group 'company-next)

(defcustom company-next-limit company-tooltip-limit
  "Maximum number of candidates in the frame."
  :type 'integer
  :group 'company-next)

(defcustom company-next-minimum-width 40
  "Minimum width of the completion frame, in numbers of characters."
  :type 'integer
  :group 'company-next)

(defcustom company-next-icons-functions
  '(company-next-icons~lsp company-next-icons~elisp company-next-icons~yasnippet)
  "Functions to call on each candidate that should return an icon.
The functions takes 1 parameter, the completion candidate.

It should return either a SYMBOL, a LIST, a STRING, or nil:

- SYMBOL:  It is the name of the icon.
- LIST:    The list is then `apply' to `icons-in-terminal' function.
           Example: '(fa_icon :face some-face :foreground \"red\")
- STRING:  A simple string which is inserted, should be of length 1

If a function returns nil, it call the next function in the list.
If all functions returns nil, `company-next-icons-unknown' is used."
  :type 'list
  :group 'company-next)


(defvar company-next-backends-color
  '((company-yasnippet . (:all "lime green" :selected (:background "lime green" :foreground "black"))))
  "List of colors to use for specific backends.

Each element has the form (BACKEND . COLOR)

BACKEND is the backend's symbol for which the color applies
COLOR can be a LIST or a STRING:

- LIST:    A property list with the following keys:
                `:candidate'  : Color to use on the candidate
                `:annotation' : Color to use on the annotation
                `:icon'       : Color to use on the icon
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


(defvar company-next-frame-parameters
  '((left . -1)
    (no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width  . 0)
    (width  . 0)
    (min-height  . 0)
    (height  . 0)
    (internal-border-width . 1)
    (vertical-scroll-bars . nil)
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
    (no-special-glyphs . t))
  "Frame parameters used to create the frame.")

(defvar-local company-next~ov nil)
(defvar-local company-next~max 0)
(defvar-local company-next~with-icons-p nil)
(defvar-local company-next~x nil)
(defvar-local company-next~space nil)
(defvar-local company-next~start nil)
(defvar-local company-next~height nil)
(defvar-local company-next~scrollbar-window nil)

(defmacro company-next~get-frame nil
  "Return the child frame."
  `(frame-parameter nil 'company-next-frame))

(defmacro company-next~set-frame (frame)
  "Set the frame parameter ‘company-next-frame’ to FRAME."
  `(set-frame-parameter nil 'company-next-frame ,frame))

(defun company-next~get-buffer (&optional suffix)
  "Construct the buffer name, it should be unique for each frame."
  (get-buffer-create
   (concat " *company-next-"
           (or (frame-parameter nil 'window-id)
               (frame-parameter nil 'name))
           suffix
           "*")))

(defun company-next~with-icons-p nil
  (let ((spaces (+ (- (current-column) (string-width company-prefix))
                   (/ (or (car (nth 2 (posn-at-point (line-beginning-position)))) 0)
                      (frame-char-width))
                   (car (window-edges nil t)))))
    (setq company-next~space spaces)
    (and company-next-enable-icon
         (fboundp 'icons-in-terminal)
         (> spaces 1))))

(defun company-next~make-frame nil
  (let* ((after-make-frame-functions nil)
         (before-make-frame-hook nil)
         (buffer (company-next~get-buffer))
         (params (append company-next-frame-parameters
                         `((default-minibuffer-frame . ,(selected-frame))
                           (minibuffer . ,(minibuffer-window))
                           (background-color . ,(face-background 'company-next-background nil t)))))
         (window (display-buffer-in-child-frame buffer `((child-frame-parameters . ,params))))
         (frame (window-frame window)))
    (set-frame-parameter nil 'company-next-buffer buffer)
    (set-frame-parameter nil 'company-next-window window)
    (set-window-dedicated-p window t)
    (redirect-frame-focus frame (frame-parent frame))
    frame))

(defun company-next~get-ov nil
  (or company-next~ov
      (setq company-next~ov (make-overlay 1 1))))

(defun company-next~update-line (selection)
  (goto-char 1)
  (forward-line selection)
  (move-overlay (company-next~get-ov)
                (line-beginning-position)
                (line-beginning-position 2))
  (-if-let* ((color (get-text-property (point) 'company-next~color)))
      (overlay-put (company-next~get-ov) 'face color)
    (overlay-put (company-next~get-ov) 'face 'company-next-selection)))

(defun company-next~render-buffer (string)
  (let ((selection company-selection))
    (with-current-buffer (company-next~get-buffer)
      (erase-buffer)
      (insert string "\n")
      (setq mode-line-format nil
            truncate-lines t)
      (setq-local scroll-step 1)
      (setq-local scroll-conservatively 10000)
      (setq-local scroll-margin  0)
      (setq-local scroll-preserve-screen-position t)
      (company-next~update-line selection))))

(defun company-next~point-bottom nil
  (let* ((win (let ((tmp nil))
                (while (window-in-direction 'below tmp)
                  (setq tmp (window-in-direction 'below tmp)))
                tmp)))
    (+ (or (nth 2 (or (window-line-height 'mode-line win)
                      (and (redisplay t) (window-line-height 'mode-line win))))
           0)
       (or (and win (nth 1 (window-edges win t nil t))) 0))))

(defun company-next~set-frame-position (frame)
  (-let* (((left top right _bottom) (window-edges nil t nil t))
          (window (frame-parameter nil 'company-next-window))
          ((width . height) (window-text-pixel-size window nil nil 10000 10000))
          (frame-resize-pixelwise t)
          (point (- (point) (length company-prefix)))
          (mode-line-y (company-next~point-bottom))
          ((p-x . p-y) (nth 2 (posn-at-point point)))
          (char-height (frame-char-height frame))
          (char-width (frame-char-width frame))
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
          (x (if company-next~with-icons-p
                 (- p-x (* char-width (if (= company-next~space 2) 2 3)))
               (- p-x (if (= company-next~space 0) 0 char-width)))))
    ;; Debug
    ;; (message "X+LEFT: %s P-X: %s X: %s LEFT: %s space: %s with-icon: %s LESS: %s"
    ;;          (+ x left) p-x x left company-next~space company-next~with-icons-p (+ (* char-width 3) (/ char-width 2)))
    (setq company-next~x (+ x left))
    (setq company-next~start (window-start))
    (setq company-next~height height)
    (set-frame-size frame (company-next~update-width t (/ height char-height))
                    height t)
    (set-frame-position frame (max (+ x left) 0) (+ y top))
    (with-selected-frame frame (set-fringe-style 0))))

(defun company-next~display (string)
  "Display the completions."
  (company-next~render-buffer string)
  (unless (company-next~get-frame)
    (company-next~set-frame (company-next~make-frame)))
  (company-next~set-frame-position (company-next~get-frame))
  (unless (frame-visible-p (company-next~get-frame))
    (make-frame-visible (company-next~get-frame)))
  (company-next~update-scrollbar (company-next~get-frame) t))

(defun company-next~get-icon (candidate)
  (let ((list company-next-icons-functions)
        icon)
    (while (and (null icon) list)
      (setq icon (funcall (car list) candidate))
      (pop list))
    (setq icon (or icon company-next-icons-unknown))
    (cond
     ((listp icon)
      (if company-next-color-icon
          (apply 'icons-in-terminal icon)
        (icons-in-terminal (car icon))))
     ((symbolp icon)
      (icons-in-terminal icon))
     (t icon))))

(defun company-next~add-icon (candidate)
  (concat
   (company-next~get-icon candidate)
   (propertize " " 'display `(space :align-to (+ left-fringe ,(if (> company-next~space 2) 3 2))))))

(defun company-next~get-color (backend)
  (alist-get backend company-next-backends-color))

(defun company-next~resolve-color (color key)
  (or (and (stringp color) color)
      (and (listp color) (or (plist-get color key) (plist-get color :all)))))

(defun company-next~resolve-colors (color)
  (when color
    (list
     (company-next~resolve-color color :candidate)
     (company-next~resolve-color color :annotation)
     (company-next~resolve-color color :icon)
     (let ((color (company-next~resolve-color color :selected)))
       (unless (stringp color)
         color)))))

(defun company-next~apply-color (string color)
  (when color
    (add-face-text-property 0 (length string)
                            (if (stringp color) (list :foreground color) color)
                            nil string))
  string)

(defun company-next~make-line (candidate)
  (-let* (((candidate annotation len-c len-a backend) candidate)
          (color (company-next~get-color backend))
          ((c-color a-color i-color s-color) (company-next~resolve-colors color))
          (icon-string (and company-next~with-icons-p (company-next~add-icon candidate)))
          (candidate-string (propertize candidate 'face 'company-next-candidate))
          (align-string (when annotation
                          (if company-next-align-annotations
                              (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))
                            " ")))
          (space company-next~space)
          (icon-p company-next-enable-icon)
          (annotation-string (and annotation (propertize annotation 'face 'company-next-annotation)))
          (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                          (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                        (company-next~apply-color icon-string i-color)
                        (company-next~apply-color candidate-string c-color)
                        align-string
                        (company-next~apply-color annotation-string a-color)))
          (len (length line)))
    (add-text-properties 0 len (list 'company-next~len (+ len-c len-a)
                                     'company-next~color s-color)
                         line)
    line))

(defun company-next~backend (candidate)
  (or (get-text-property 0 'company-backend candidate)
      (--first (and it (not (keywordp it))) company-backend)))

(defun company-next~make-candidate (candidate)
  (let* ((annotation (-some->> (company-call-backend 'annotation candidate)
                               (replace-regexp-in-string "[ \t\n\r]+" " ")))
         (len-candidate (string-width candidate))
         (len-annotation (if annotation (string-width annotation) 0))
         (len-total (+ len-candidate len-annotation))
         (backend (company-next~backend candidate)))
    (when (> len-total company-next~max)
      (setq company-next~max len-total))
    (list candidate
          annotation
          len-candidate
          len-annotation
          backend)))

(defun company-next-show nil
  (setq company-next~max 0
        company-next~with-icons-p (company-next~with-icons-p))
  (--> (-take company-next-limit company-candidates)
       (mapcar (-compose 'company-next~make-line 'company-next~make-candidate) it)
       (mapconcat 'identity it "\n")
       (company-next~display it)))

(defun company-next-hide nil
  (-some-> (company-next~get-frame)
           (make-frame-invisible)))

(defun company-next~calc-len (buffer start end char-width)
  (let ((max 0))
    (with-current-buffer buffer
      (save-excursion
        (goto-char start)
        (while (< (point) end)
          (let ((len (or (get-text-property (point) 'company-next~len) 0)))
            (when (> len max)
              (setq max len)))
          (forward-line))))
    (* (+ max (if company-next~with-icons-p 6 2))
       char-width)))

(defun company-next~update-width (&optional no-update height)
  (unless no-update
    (redisplay))
  (-let* ((frame (company-next~get-frame))
          (window (frame-parameter nil 'company-next-window))
          (start (window-start window))
          (char-width (frame-char-width frame))
          (end (or (and height (with-current-buffer (window-buffer window)
                                 (save-excursion
                                   (goto-char start)
                                   (forward-line height)
                                   (point))))
                   (window-end window)))
          (max-width (- (frame-pixel-width) company-next~x) char-width)
          (width (+ (if company-next-align-annotations
                        ;; With align-annotations, `window-text-pixel-size' doesn't return
                        ;; good values because of the display properties in the buffer
                        ;; More specifically, because of the spaces specifications
                        (company-next~calc-len (window-buffer window) start end char-width)
                      (car (window-text-pixel-size window start end 10000 10000)))
                    (if (company-next~scrollbar-p frame) (* 2 char-width) 0)
                    char-width))
          (width (max (min width max-width)
                      (* company-next-minimum-width char-width))))
    (or (and no-update width)
        (set-frame-width (company-next~get-frame) width nil t))))

(defun company-next~percent (a b)
  (/ (float a) b))

(defun company-next~scrollbar-p (frame)
  (/= 1 (company-next~percent
         company-next~height
         (* (min company-candidates-length company-next-limit)
            (frame-char-height frame)))))

(defun company-next~update-scrollbar-buffer (height-blank height-scrollbar percent buffer)
  (with-current-buffer buffer
    (erase-buffer)
    (setq header-line-format nil
          mode-line-format nil)
    (unless (zerop height-blank)
      (insert (propertize " " 'display `(space :align-to right-fringe :height ,height-blank))
              (propertize "\n" 'face '(:height 1))))
    (setq height-scrollbar (if (= percent 1)
                               ;; Due to some casting in the emacs code, there might 1 or 2
                               ;; remainings pixels
                               (+ height-scrollbar 10)
                             height-scrollbar))
    (insert (propertize " " 'face (list :background (face-background 'company-next-scrollbar nil t))
                        'display `(space :align-to right-fringe :height ,height-scrollbar)))
    (current-buffer)))

(defun company-next~update-scrollbar (frame &optional first)
  (let* ((selection company-selection)
         (buffer (company-next~get-buffer "-scrollbar"))
         (height company-next~height)
         (n-elements (min company-candidates-length company-next-limit))
         (percent (company-next~percent selection (1- n-elements)))
         (percent-display (company-next~percent height (* n-elements (frame-char-height frame))))
         (height-scrollbar-1 (* height percent-display))
         (height-scrollbar (* height percent-display))
         (height-scrollbar (/ height-scrollbar (frame-char-height frame)))
         ;; (height-blank-1 (* (- height height-scrollbar-1) percent))
         (height-blank (* (- height height-scrollbar-1) percent))
         (height-blank (/ height-blank (frame-char-height frame))))
    (cond
     ((and first (= percent-display 1) (window-live-p company-next~scrollbar-window))
      (delete-window company-next~scrollbar-window))
     ((window-live-p company-next~scrollbar-window)
      (company-next~update-scrollbar-buffer height-blank height-scrollbar percent buffer))
     ((/= percent-display 1)
      (setq
       company-next~scrollbar-window
       (with-selected-frame (company-next~get-frame)
         (display-buffer-in-side-window
          (company-next~update-scrollbar-buffer height-blank height-scrollbar percent buffer)
          '((side . right) (window-width . 2)))))
      (window-preserve-size company-next~scrollbar-window t t)))))

;; ;; (message "selection: %s len: %s PERCENT: %s PERCENTS-DISPLAY: %s SIZE-FRAME: %s HEIGHT-S: %s HEIGHT-B: %s height: %s sum: %s"
;; ;;          selection n-elements percent percent-display height height-scrollbar height-blank height (+ height-scrollbar height-blank))
;; ;; (message "HEIGHT-S-1: %s HEIGHT-B-1: %s sum: %s" height-scrollbar-1 height-blank-1 (+ height-scrollbar-1 height-blank-1))

(defun company-next~change-line nil
  (let ((selection company-selection))
    (with-selected-window (get-buffer-window (company-next~get-buffer) t)
      (company-next~update-line selection))
    (company-next~update-scrollbar (company-next~get-frame))))

(defun company-next~next-line nil
  (interactive)
  (when (< (1+ company-selection) (min company-candidates-length
                                       company-next-limit))
    (setq company-selection (1+ company-selection))
    (company-next~change-line)
    (company-next~update-width)))

(defun company-next~prev-line nil
  (interactive)
  (setq company-selection (max (1- company-selection) 0))
  (company-next~change-line)
  (company-next~update-width))

(defun company-next~start-changed-p nil
  (not (= company-next~start (window-start))))

(defun company-next~post-command nil
  (cond ((company-next~start-changed-p)
         (company-next~on-window-change))))

(defun company-next-frontend (command)
  "`company-mode' frontend using child-frame.
COMMAND: See `company-frontends'."
  ;; (message "\nCOMMMAND: %s" command)
  ;; (message "prefix: %s" company-prefix)
  ;; (message "candidates: %s" company-candidates)
  ;; (message "common: %s" company-common)
  ;; (message "selection: %s" company-selection)
  ;; (message "point: %s" company-point)
  ;; (message "search-string: %s" company-search-string)
  ;;(message "last-command: %s" last-command)
  (cond
   ((or (eq command 'hide) (equal company-candidates-length 1))
    (company-next-hide))
   ((eq command 'update)
    (company-next-show))
   ((eq command 'post-command)
    (company-next~post-command))))

(defun company-next~on-window-change nil
  (company-next~set-frame-position (company-next~get-frame)))

(defvar company-next-mode-map nil
  "Keymap when `company-next' is active")

(unless company-next-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap company-select-next-or-abort] 'company-next~next-line)
    (define-key map [remap company-select-previous-or-abort] 'company-next~prev-line)
    (setq company-next-mode-map map)))

(defun company-next~set-mode (&optional frame)
  (cond
   ((and company-next-mode (not (display-graphic-p frame)))
    (company-next-mode -1))
   (company-next-mode
    (remove-hook 'after-make-frame-functions 'company-next~set-mode t)
    (make-variable-buffer-local 'company-frontends)
    (setq company-frontends (delq 'company-pseudo-tooltip-unless-just-one-frontend company-frontends))
    (add-to-list 'company-frontends 'company-next-frontend))
   ((memq 'company-next-frontend company-frontends)
    (setq company-frontends (delq 'company-next-frontend  company-frontends))
    (add-to-list 'company-frontends 'company-pseudo-tooltip-unless-just-one-frontend))))

;;;###autoload
(define-minor-mode company-next-mode
  "Company-next minor mode."
  :group 'company-next
  :lighter " company-next"
  ;; With emacs daemon and:
  ;; `(add-hook 'company-mode-hook 'company-next-mode)'
  ;; `company-next-mode' is called to early to know if we are in a GUI
  (if (and (daemonp)
           (not (frame-parameter nil 'client))
           company-next-mode)
      (add-hook 'after-make-frame-functions 'company-next~set-mode t t)
    (company-next~set-mode)))

(provide 'company-next)
;; company-next ends here
