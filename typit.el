;;; typit.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-
;;
;; Copyright © 2016–present Mark Karpov <markkarpov92@gmail.com>
;;
;; Author: Mark Karpov <markkarpov92@gmail.com>
;; URL: https://github.com/mrkkrp/typit
;; Version: 0.2.1
;; Package-Requires: ((emacs "24.4") (f "0.18") (mmt "0.1.1"))
;; Keywords: games
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;; Public License for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a typing game for Emacs. In this game, you type as many words as you can
;; until time is up (by default it's one minute).
;;
;; There are two different types of play on offer:
;;
;; * Dictionary test: where you type words that are picked randomly from N most
;;   frequent words in the language you're practicing. This is quite similar to the
;;   “10 fast fingers” tests, with the difference that it's playable and fully
;;   configurable inside your Emacs.
;;
;; * Literature test: the gameplay is identical to dictionary test although instead
;;   of random words picked from a dictionary, you type text from a specified
;;   file. You can use any text file you like, and Emacs will save your position
;;   between sessions for convenience so that you will always start just where you
;;   left off last time.

;;; Code:

(require 'cl-lib)
(require 'cl)
(require 'f)
(require 'mmt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & variables

(defgroup typit nil
  "Typing game similar to the tests on 10 fast fingers."
  :group  'games
  :tag    "Typit"
  :prefix "typit-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/typit"))

(defface typit-title
  '((t (:inherit font-lock-constant-face)))
  "Face used to display Typit buffer title.")

(defface typit-normal-text
  '((t (:inherit default)))
  "Face used to display words to type.")

(defface typit-current-word
  '((t (:inherit highlight)))
  "Face used to highlight current word.")

(defface typit-correct-char
  '((t (:inherit success)))
  "Face used to color correctly typed characters.")

(defface typit-wrong-char
  '((t (:inherit error)))
  "Face used to color incorrectly typed characters.")

(defface typit-statistic
  '((t (:inherit font-lock-type-face)))
  "Face used to render names of statistical values after typing.")

(defface typit-value
  '((t (:inherit font-lock-constant-face)))
  "Face used to render statistical values after typing.")

(defcustom typit-dict "english.txt"
  "Name of dictionary file to use."
  :tag  "Dictionary to use"
  :type '(choice (const :tag "English" "english.txt")
                 (const :tag "German"  "german.txt")
                 (const :tag "French"  "french.txt")))

(defcustom typit-dict-dir
  (when load-file-name
    (f-slash (f-join (f-parent load-file-name) "dict")))
  "Path to directory with collection of dictionaries."
  :tag  "Directory with dictionary files"
  :type 'directory)

(defcustom typit-literature-dir
  (when load-file-name
    (f-slash (f-join (f-parent load-file-name) "literature")))
  "Path to directory with collection of literature texts."
  :tag  "Directory with literature texts"
  :type 'directory)

(defcustom typit-line-length 80
  "Length of line of words to use."
  :tag  "Length of line of words"
  :type 'integer)

(defcustom typit-test-time 60
  "Number of second a test takes."
  :tag  "Test duration in seconds"
  :type 'integer)

(defvar typit--dict nil
  "Vector of words to use for `typit-dictionary-test' (ordered from most common to least common).

If the value is NIL, it means that no dictionary has been loaded
yet.")

(defvar typit--dict-file nil
  "File name of currently loaded dictionary.

If no dictionary is loaded, it's NIL.")

(defvar typit--dict-num-words 200
  "Number of words to use from the dictionary.")

(defvar typit--literature-file nil
  "File name of the text file used for `typit-literature-test'.

The default value is NIL. The literature file is included in
save-state - if it is still NIL after `typit--load-state' then
the default file will be chosen.")

(defvar typit--literature-file-marker 0
  "Current position in the literature file.")

(defvar typit--literature-words nil
  "List of words used for `typit-literature-test'.")

(defvar typit--pick-word-function nil
  "Function used by `typit--pick-word' to pick the next word.")

(defvar typit--next-word nil
  "The next word to be used.")

(defvar typit--state-file "~/.emacs.d/.typit"
  "File to save typit state info between sessions.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level functions

;; Loading and saving

(defun typit--dump-vars-to-file (varlist filename)
  "Simplistic dumping of variables in VARLIST to a file FILENAME.

Courtesy of Stack Overflow user Trey Jackson."
  (save-excursion
    (let ((buf (find-file-noselect filename)))
      (set-buffer buf)
      (erase-buffer)
      (typit--dump-vars-to-buffer varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun typit--dump-vars-to-buffer (varlist buffer)
  "Insert into buffer the setq statements to recreate the variables in VARLIST.

Courtesy of Stack Overflow user Trey Jackson."
  (loop for var in varlist do
        (print (list 'setq var (list 'quote (symbol-value var)))
               buffer)))

(defun typit--save-state ()
  (typit--dump-vars-to-file
   '(typit--literature-file
     typit--literature-file-marker)
   typit--state-file))

(defun typit--load-state ()
  (if (f-exists-p typit--state-file)
      (load typit--state-file)))

;; Text processing

(defun typit--split-string-retain-newlines (text)
  "Splits TEXT on whitespace whilst retaining newline characters.

Newline characters are included as separate words but empty
strings are discarded."
  (seq-filter (lambda (x) (not (string-empty-p x)))
              (split-string
               (replace-regexp-in-string "\n" " \n " text)
               "[ \f\t\r\v]+")))

(defun typit--literature-mode-p ()
  "Returns NON-NIL if the most recent test initiated was a
  literature test."
  (eq typit--pick-word-function 'typit--pick-word-from-file))

(defun typit--prepare-dict ()
  "Make sure that `typit--dict' and `typit--dict-file' are set."
  (let ((dict-file (f-expand typit-dict typit-dict-dir)))
    (when (or (not typit--dict-file)
              (not (f-same? typit--dict-file dict-file)))
      (setq typit--dict-file dict-file
            typit--dict
            (with-temp-buffer
              (insert-file-contents dict-file)
              (vconcat
               (split-string
                (buffer-substring-no-properties
                 (point-min)
                 (point-max))
                "\n" t "[[:space:]]*")))))))

(defun typit--prepare-literature ()
  "Setup `typit--literature-words'."
  ;; make sure that we don't carry over the last word loaded in previous test
  (setq typit--next-word nil)
  ;; make sure that literature file is set up
  (if (not typit--literature-file)
      (setq typit--literature-file (f-expand "default.txt" typit-literature-dir)))
  (if (not (f-exists? typit--literature-file))
      (error "file does not exist: %s" typit--literature-file))
  ;; get words from file
  (setq typit--literature-words
        (with-temp-buffer
          (insert-file-contents typit--literature-file)
          (if (or (< typit--literature-file-marker (point-min))
                  (> typit--literature-file-marker (point-max)))
              (setq typit--literature-file-marker (point-min)))
          ;; make sure that we're not part-way through a word
          (goto-char typit--literature-file-marker)
          (backward-word)
          (setq typit--literature-file-marker (point))
          ;; get a substantial chunk of text for use in test
          (let ((end-point (+ 3000 typit--literature-file-marker)))
            (if (> end-point (point-max))
                (setq end-point (point-max)))
            (split-string
             (buffer-substring-no-properties typit--literature-file-marker end-point)
             "[ \f\t\n\r\v]+" t "[[:space:]]*")))))

(defun typit--pick-word ()
  "Pick a word using `typit--pick-word-function'"
  (funcall typit--pick-word-function))

(defun typit--pick-word-from-dict ()
  "Pick a word from `typit--dict'.

Use first NUM words from loaded dictionary (if NUM is bigger than
length of the dictionary, use all words).  All words in
`typit--dict' have approximately the same probability."
  (elt typit--dict (random (min typit--dict-num-words
                           (length typit--dict)))))

(defun typit--pick-word-from-file ()
  "Pick a word from `typit--literature-text'."
  (if (not typit--literature-words)
      (typit--prepare-literature))
  (pop typit--literature-words))

(defun typit--generate-line ()
  "Generate a line of appropriate length picking words with `typit--pick-word'.

Result is returned as a list of strings with assumption that only
one space is inserted between each word (then total length should
be close to `typit-line-length')."
  ;; when we exceed line-length we want save NEXT-WORD to go at the beginning of
  ;; the next line
  (if (not typit--next-word)
      (setq typit--next-word (typit--pick-word)))
  (let ((words nil)
        (acc   0))
    (while (< acc typit-line-length)
      (setq acc
            (+ acc
               (length typit--next-word)
               (if words 1 0)))
      (push typit--next-word words)
      (setq typit--next-word (typit--pick-word)))
    ;; reverse list to get text in right order for literature test
    ;; ... doesn't matter for dictionary test
    (reverse words)))

(defun typit--render-line (words)
  "Transform list of words WORDS into one string."
  (mapconcat #'identity words " "))

(defun typit--render-lines (offset first-line second-line)
  "Render the both lines in current buffer.

The lines are placed beginning from OFFSET (text from OFFSET to
end of buffer is deleted).  FIRST-LINE and SECOND-LINE are
rendered with `typit--render-line'."
  (let ((inhibit-read-only t))
    (delete-region offset (point-max))
    (goto-char offset)
    (insert (propertize (typit--render-line first-line)
                        'face 'typit-normal-text)
            "\n")
    (insert (propertize (typit--render-line second-line)
                        'face 'typit-normal-text)
            "\n")))

(defun typit--select-word (offset current-word &optional unselect)
  "Change font properties of a word.

OFFSET specifies position where word starts.  CURRENT-WORD is the
word to highlight.  By default the word is selected, unless
UNSELECT is not NIL—in this case it's unselected."
  (if unselect
      (dolist (v (overlays-at offset))
        (when (eq (overlay-get v 'type) 'typit-current-word)
          (delete-overlay v)))
    (let ((overlay
           (make-overlay
            offset
            (+ offset (length current-word))
            nil t nil)))
      (overlay-put overlay 'type 'typit-current-word)
      (overlay-put overlay 'face 'typit-current-word))))

(defun typit--highlight-diff-char (pos correct &optional clear)
  "Highlight diff for one char at position POS.

If the char should be highlighted as correctly typed, pass
non-NIL CORRECT.  If CLEAR is not NIL, just clear that char."
  (let ((inhibit-read-only t))
    (with-silent-modifications
      (add-text-properties
       pos (1+ pos)
       (list
        'face
        (if clear
            'typit-normal-text
          (if correct
              'typit-correct-char
            'typit-wrong-char)))))))

(defmacro typit--with-buffer (quit-function &rest body)
  "Perform actions using a new temporary Typit buffer and window.

Make new Typit buffer and make it current buffer.  QUIT-FUNCTION
receives current window object and value returned by BODY as its
arguments.  It describes what to do when contents of buffer
generated in BODY are shown to the user.  By the time the buffer
is shown it's in read-only state.  Note that BODY is evaluated,
buffer is made empty.

The window is guaranteed to be killed at the end of the day."
  (declare (indent defun))
  (mmt-with-gensyms (buffer window value)
    `(let ((,buffer (get-buffer-create "*typit*")))
       (with-current-buffer ,buffer
         (with-current-buffer-window
          ;; buffer or name
          ,buffer
          ;; action (for `display-buffer')
          (cons 'display-buffer-below-selected
                '((window-height . fit-window-to-buffer)
                  (preserve-size . (nil . t))))
          ;; quit-function
          (lambda (,window ,value)
            (unwind-protect
                (funcall ,quit-function ,window ,value)
              (when (window-live-p ,window)
                (quit-restore-window ,window 'kill))))
          ;; body
          (setq cursor-type nil)
          ,@body)))))

(defun typit--run-end-of-test-options ()
  "Prompt user to play again, plus any mode-specific options."
  (let ((loop-again t))
    (while loop-again
      (setf loop-again nil)
      (case (read-char "Choose an option: (q)uit | (p)lay again | (v)isit file at point")
            ((?q ?Q) (message "quit"))
            ((?p ?P) (typit--test))
            ((?v ?V) (typit-visit-literature-file))
            (t (setf loop-again t))))))

(defun typit--report-results
    (total-time
     good-strokes
     bad-strokes
     good-words
     bad-words)
  "Report results of Typit test to the user and prompt to play again.

TOTAL-TIME, GOOD-STROKES, BAD-STROKES, GOOD-WORDS, and BAD-WORDS
are used to calculate statistics."

  ;; update file position for literature mode
  (if (typit--literature-mode-p)
      (incf typit--literature-file-marker (+ good-strokes bad-strokes)))

  ;; save current state to file
  (typit--save-state)

  (typit--with-buffer
    ;; quit-function
    (lambda (_window _buffer)
      (while (not (char-equal
                   (read-char "Press space bar to continue…" t)
                   32)))
      (typit--run-end-of-test-options))

    ;; body
    (insert
     (propertize
      (if (typit--literature-mode-p)
          (format "Literature Test --- File: %s --- position: %d"
                  (f-filename typit--literature-file) typit--literature-file-marker)
        (format "Dictionary Test --- %d most common words" typit--dict-num-words))
      'face 'typit-title)
     "\n\n"
     (propertize
      (if (typit--literature-mode-p)
          (format "Full file path: %s" typit--literature-file))
      'face 'typit-title)
     "\n\n"
     (propertize (format "Test Duration: %d seconds" typit-test-time) 'face 'typit-title)
     "\n\n"
     (propertize "Your results" 'face 'typit-title)
     "\n\n"
     (propertize "Words per minute (WPM)" 'face 'typit-statistic)
     "  "
     (propertize (format "%4d" (round (/ good-strokes (/ total-time 12))))
                 'face 'typit-value)
     "\n"
     (propertize "Keystrokes" 'face 'typit-statistic)
     "              "
     (propertize (format "%4d" (+ good-strokes bad-strokes))
                 'face 'typit-value)
     " ("
     (propertize (format "%4d" good-strokes) 'face 'typit-correct-char)
     " | "
     (propertize (format "%d" bad-strokes) 'face 'typit-wrong-char)
     ")\n"
     (propertize "Words" 'face 'typit-statistic)
     "                   "
     (propertize (format "%4d" (+ good-words bad-words))
                 'face 'typit-value)
     " ("
     (propertize (format "%4d" good-words) 'face 'typit-correct-char)
     " | "
     (propertize (format "%d" bad-words) 'face 'typit-wrong-char)
     ")\n"
     (propertize "Accuracy" 'face 'typit-statistic)
     "              "
     (propertize (format "%6.2f %%" (* 100 (/ (float good-strokes) (+ good-strokes bad-strokes))))
                 'face 'typit-value)
     "\n")))

(defun typit--test ()
  "Run typing test."
  ;; setup
  (typit--load-state)
  (if (typit--literature-mode-p)
      (typit--prepare-literature)
    (typit--prepare-dict))
  ;; run test
  (let ((first-line   (typit--generate-line))
        (second-line  (typit--generate-line))
        (test-started nil)
        (init-offset  0)
        (word-offset  0)
        (good-strokes 0)
        (bad-strokes  0)
        (good-words   0)
        (bad-words    0)
        (micro-index  0)
        (current-word nil))
    (typit--with-buffer
      (lambda (window _value)
        (message "Timer will start when you start typing…")
        (typit--report-results
         (catch 'total-time
           (cl-do
               ((ch
                 (prog1
                     (read-char nil t)
                   (setq test-started (float-time)))
                 (read-char "Typing…" t)))
               ((null ch))
             (cond
              ;; space
              ((= ch #x20)
               (when current-word
                 (typit--select-word word-offset (car first-line) t)
                 (cl-destructuring-bind (w . r) first-line
                   (if (cl-every #'identity current-word)
                       (setq good-words (1+ good-words))
                     (setq bad-words (1+ bad-words)))
                   (setq
                    first-line
                    (or r second-line)
                    second-line
                    (if r second-line (typit--generate-line))
                    word-offset
                    (if r (+ word-offset 1 (length w)) init-offset)
                    good-strokes
                    (1+ good-strokes) ;; we should count space itself
                    good-strokes
                    (+ good-strokes (cl-count t current-word))
                    bad-strokes
                    (+ bad-strokes  (cl-count nil current-word))
                    micro-index  0
                    current-word nil)
                   (unless r
                     (typit--render-lines init-offset first-line second-line))
                   (typit--select-word word-offset (car first-line)))
                 (let ((total-time (- (float-time) test-started)))
                   (when (>= total-time typit-test-time)
                     (quit-restore-window window 'kill)
                     (throw 'total-time total-time)))))
              ;; backspace
              ((= ch #x7f)
               (setq micro-index (max 0 (1- micro-index)))
               (pop current-word)
               (typit--highlight-diff-char (+ word-offset micro-index) nil t))
              ;; correct stroke
              ((and (< micro-index (length (car first-line)))
                    (= ch (elt (car first-line) micro-index)))
               (push t current-word)
               (typit--highlight-diff-char (+ word-offset micro-index) t)
               (setq micro-index (1+ micro-index)))
              ;; everything else = incorrect stroke
              (t
               (when (< micro-index (length (car first-line)))
                 (push nil current-word)
                 (typit--highlight-diff-char (+ word-offset micro-index) nil)
                 (setq micro-index (1+ micro-index)))))))
         good-strokes
         bad-strokes
         good-words
         bad-words))
      ;; ↓ body (construction of the buffer contents)
      (insert (propertize
               (format "Typit (%d second test)" typit-test-time)
               'face 'typit-title) "\n\n")
      (setq init-offset (point)
            word-offset init-offset)
      (typit--render-lines init-offset first-line second-line)
      (typit--select-word word-offset (car first-line)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface


(defun typit-set-marker-for-literature-test ()
  "Sets the current position in current file as start position for `typit-literature-test'.

ERROR if buffer has no filename."
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq typit--literature-file (buffer-file-name))
        (setq typit--literature-file-marker (point))
        (typit--save-state))
    (error
     "Error in typit-set-marker-for-literature-test: could not get file name for buffer %s"
     (buffer-name))))

(defun typit-visit-literature-file ()
  "Visit the file currently set for use in
`typit-literature-test' and jump to the current point."
  (interactive)
  ;; if typit literature test hasn't been run then the variable will be NIL
  (if (not typit--literature-file)
      (typit--load-state))
  (find-file (f-expand typit--literature-file typit-literature-dir))
  (goto-char typit--literature-file-marker))

;;;###autoload
(defun typit-dictionary-test (num)
  "Run typing test with using NUM most common words from dictionary.

Dictionary is an array of words in `typit-dict'.  By default it's
English words ordered from most common to least common.  You can
let-bind the variable and change it, it's recommended to use at
least 1000 words so `typit-advanced-test' could work properly."
  (interactive "p")
  (setq typit--dict-num-words num
        typit--pick-word-function 'typit--pick-word-from-dict)
  (typit--test))

;;;###autoload
(defun typit-basic-test ()
  "Basic typing test (top 200 words in dictionary).

See `typit-dictionary-test' for more information."
  (interactive)
  (typit-dictionary-test 200))

;;;###autoload
(defun typit-advanced-test ()
  "Advanced typing test (top 1000 words in dictionary).

See `typit-dictionary-test' for more information."
  (interactive)
  (typit-dictionary-test 1000))

;;;###autoload
(defun typit-literature-test ()
  "Typing test with text taken from a pre-defined text file.

See `typit--literature-file'."
  (interactive)
  (setq typit--pick-word-function 'typit--pick-word-from-file)
  (typit--test))

(provide 'typit)

;;; typit.el ends here
