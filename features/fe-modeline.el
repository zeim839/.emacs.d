;; -*- lexical-binding: t; -*-
;; --- Heavily adapted from: https://github.com/protesilaos/dotfiles ---

(defface mz/modeline-indicator-button nil
  "Generic face used for indicators that have a background.
Modify this face to, for example, add a :box attribute to all
relevant indicators (combines nicely with my `spacious-padding'
package).")

(defgroup mz/modeline nil
  "Custom modeline that is stylistically close to the default."
  :group 'mode-line)

(defgroup mz/modeline-faces nil
  "Faces for my custom modeline."
  :group 'mz/modeline)

(defcustom mz/modeline-string-truncate-length 9
  "String length after which truncation should be done in small windows."
  :type 'natnum)

(defface mz/modeline-indicator-red
  '((default :inherit bold)
    (((class color) (min-colors 88) (background light))
     :foreground "#880000")
    (((class color) (min-colors 88) (background dark))
     :foreground "#ff9f9f")
    (t :foreground "red"))
  "Face for modeline indicators (e.g. see my `notmuch-indicator')."
  :group 'mz/modeline-faces)

(defface mz/modeline-indicator-red-bg
  '((default :inherit (bold mz/modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#bd1f30" :foreground "white")
    (t :background "blue" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'mz/modeline-faces)

(defface mz/modeline-indicator-magenta-bg
  '((default :inherit (bold mz/modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#0000aa" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#ff7788" :foreground "black")
    (t :background "#ff7788" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'mz/modeline-faces)

(defface mz/modeline-indicator-cyan-bg
  '((default :inherit (bold mz/modeline-indicator-button))
    (((class color) (min-colors 88) (background light))
     :background "#006080" :foreground "white")
    (((class color) (min-colors 88) (background dark))
     :background "#40c0e0" :foreground "black")
    (t :background "cyan" :foreground "black"))
  "Face for modeline indicators with a background."
  :group 'mz/modeline-faces)

(defun mz/modeline-buffer-identification-face ()
  "Return appropriate face or face list for `mz/modeline-buffer-identification'."
  (let ((file (buffer-file-name)))
    (cond
     ((and (mode-line-window-selected-p)
           file
           (buffer-modified-p))
      '(italic mode-line-buffer-id))
     ((and file (buffer-modified-p))
      'italic)
     ((mode-line-window-selected-p)
      'mode-line-buffer-id))))

(defvar-local mz/modeline-kbd-macro
    '(:eval
      (when (and (mode-line-window-selected-p) defining-kbd-macro)
        (concat " " (propertize " KMacro " 'face 'mz/modeline-indicator-red-bg) " ")))
  "Mode line construct displaying `mode-line-defining-kbd-macro'.
Specific to the current window's mode line.")

(defvar-local mz/modeline-narrow
    '(:eval
      (when (and (mode-line-window-selected-p)
                 (buffer-narrowed-p)
                 (not (derived-mode-p 'Info-mode 'help-mode 'special-mode 'message-mode)))
        (concat " " (propertize " Narrow " 'face 'mz/modeline-indicator-cyan-bg) " ")))
  "Mode line construct displaying if buffer is narrowed.")

(defun mz/modeline-buffer-name ()
  "Return buffer name, with read-only indicator if relevant."
  (let ((name (buffer-name)))
    (if buffer-read-only
        (format "%s %s" (char-to-string #xE0A2) name)
      name)))

(defun mz/modeline-buffer-name-help-echo ()
  "Return `help-echo' value for `mz/modeline-buffer-identification'."
  (concat
   (propertize (buffer-name) 'face 'mode-line-buffer-id)
   "\n"
   (propertize
    (or (buffer-file-name)
        (format "No underlying file.\nDirectory is: %s" default-directory))
    'face 'font-lock-doc-face)))

(defvar-local mz/modeline-buffer-identification
    '(:eval
      (propertize (mz/modeline-buffer-name)
                  'face (mz/modeline-buffer-identification-face)
                  'mouse-face 'mode-line-highlight
                  'help-echo (mz/modeline-buffer-name-help-echo)))
  "Mode line construct for identifying the buffer being displayed.
Propertize the current buffer with the `mode-line-buffer-id'
face.  Let other buffers have no face.")

(defun mz/modeline-major-mode-indicator ()
  "Return appropriate propertized mode line indicator for the major mode."
  (let ((indicator (cond
                    ((derived-mode-p 'text-mode) "§")
                    ((derived-mode-p 'prog-mode) "λ")
                    ((derived-mode-p 'comint-mode) ">_")
                    (t "📖"))))
    (propertize indicator 'face 'shadow)))

(defun mz/modeline-major-mode-name ()
  "Return capitalized `major-mode' without the -mode suffix."
  (capitalize (string-replace "-mode" "" (symbol-name major-mode))))

(defun mz/modeline-major-mode-help-echo ()
  "Return `help-echo' value for `prot-modeline-major-mode'."
  (if-let ((parent (get major-mode 'derived-mode-parent)))
      (format "Symbol: `%s'.  Derived from: `%s'" major-mode parent)
    (format "Symbol: `%s'." major-mode)))

(defvar-local mz/modeline-major-mode
    '(:eval
      (propertize
       (concat
        " " (mz/modeline-major-mode-indicator) " "
        (mz/modeline-major-mode-name) " ")
       'mouse-face 'mode-line-highlight
       'help-echo (mz/modeline-major-mode-help-echo)
       'face 'mz/modeline-indicator-magenta-bg))
  "Mode line construct for displaying major modes.")

(defvar-local mz/modeline-misc-info
    '(:eval
      (when (mode-line-window-selected-p)
        mode-line-misc-info))
  "Mode line construct displaying `mode-line-misc-info'.
Specific to the current window's mode line.")

;; Do not display the modeline in specific buffers.
(use-package hide-mode-line :ensure t :defer t
  :commands (hide-mode-line-mode)
  :hook (vterm-mode . hide-mode-line-mode))

(set-face-attribute 'mode-line nil
                    :background "#2f527b"
                    :foreground "#ecf0ff"
                    :box '(:line-width 1 :color "black")
                    :overline nil
                    :underline nil)

(set-face-attribute 'mode-line-inactive nil
                    :background "#1a2331"
                    :foreground "#90a0dc"
                    :box '(:line-width 1 :color "black")
                    :overline nil
                    :underline nil)

(setq-default mode-line-format
              '(" "
                (:eval mz/modeline-major-mode)
                " "
                (:eval mz/modeline-buffer-identification)
                " "
                (:eval mz/modeline-misc-info)
                " "
                mode-line-format-right-align
                mode-line-position
                (:eval mz/modeline-kbd-macro)
                (:eval mz/modeline-narrow)
                "  "
                mode-line-end-spaces))
