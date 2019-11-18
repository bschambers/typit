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

(defface typit-paragraph-break-face
  '((t (:inherit warning)))
  "Face used to render paragraph break.")

(defcustom typit-line-length 80
  "Length of line of words to use."
  :tag  "Length of line of words"
  :type 'integer)

(defcustom typit-test-time 60
  "Number of second a test takes."
  :tag  "Test duration in seconds"
  :type 'integer)

(defvar typit--paragraph-break-symbol "<<<BREAK>>>")

(defvar typit--next-word nil
  "The next word to be used.")

(defvar typit--save-file "~/.emacs.d/.typit"
  "File to save typit state info between sessions.")

(defvar typit--all-lines nil
  "All of the lines used so far.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode variables

(defvar typit--mode-title nil
  "The title of the mode currently in operation.")

(defvar typit--pick-word-function nil
  "Function used by `typit--pick-word' to pick the next word.")

(defvar typit--init-test-function nil
  "Function run at the beginning of the typing test.")

(defvar typit--end-of-test-function nil
  "Function run at the end of the typing test.")

(defvar typit--report-String-function nil
  "Returns a list of mode-specific lines to add to the top of the report screen.")

(defvar typit--end-options-function nil
  "Returns a list of options to add to the end of game options.")

(defvar typit--save-function nil
  "Saves any persistent variables for the current mode.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level functions

(defun typit--save-vars-to-file (&rest varlist)
  "Simplistic dumping of variables in VARLIST into file `typit--save-file'."
  (save-excursion
    (let ((buf (find-file-noselect typit--save-file)))
      (set-buffer buf)
      (erase-buffer)
      (typit--dump-vars-to-buffer varlist buf)
      (save-buffer)
      (kill-buffer))))

(defun typit--dump-vars-to-buffer (varlist buffer)
  "Insert into buffer the setq statements to recreate the variables in VARLIST."
  (dolist (var varlist)
    (print (list 'setq var (list 'quote (symbol-value var)))
           buffer)))

(defun typit--save-state ()
  (if typit--save-function
      (funcall typit--save-function)))

(defun typit--load-state ()
  (if (f-exists-p typit--save-file)
      (load typit--save-file)
    (error "Save-file does not exist: %s" typit--save-file)))

(defun typit--get-line-of-buffer (line-num)
  "Gets the line at LINE-NUM and returns it as a string."
  (save-excursion
    (goto-line line-num)
    (beginning-of-line)
    (let ((beg (point)))
      (end-of-line)
      (buffer-substring beg (point)))))

(defun typit--split-string-convert-paragraph-breaks (text)
  "Splits TEXT on whitespace whilst converting paragraph breaks to `typit--paragraph-break-symbol'.

Paragraph break is interpreted as being two or more consecutive
newlines, optionally with whitespace in between them. Any single
newline characters along with any other whitespace characters are
discarded."
  (seq-filter
   (lambda (x) (not (string-empty-p x)))
   (split-string
    (replace-regexp-in-string "\n[ *\n]+" (concat " " typit--paragraph-break-symbol " ") text)
    "[ \n\f\t\r\v]+")))

(defun typit--pick-word ()
  "Pick a word using `typit--pick-word-function'"
  (funcall typit--pick-word-function))

(defun typit--generate-line ()
  "Generate a line of appropriate length picking words with `typit--pick-word'.

Result is returned as a list of strings with assumption that only
one space is inserted between each word (then total length should
be close to `typit-line-length').

Line will end early if `typit--paragraph-break-symbol' is
encountered."
  ;; when we exceed line-length we want save NEXT-WORD to go at the beginning of
  ;; the next line
  (if (not typit--next-word)
      (setq typit--next-word (typit--pick-word)))
  (let ((words nil)
        (acc   0)
        (paragraph-break nil))
    (while (and (< acc typit-line-length)
                (not paragraph-break))
      (setq acc
            (+ acc
               (length typit--next-word)
               (if words 1 0)))
      (push typit--next-word words)
      (if (equal typit--next-word typit--paragraph-break-symbol)
          (setq paragraph-break t))
      (setq typit--next-word (typit--pick-word)))
    ;; reverse list to get text in right order for literature test
    ;; ... doesn't matter for dictionary test
    (reverse words)))

(defun typit--render-line (words)
  "Transform list of words WORDS into one string."
  (let* ((end-offset (if (equal typit--paragraph-break-symbol (car (last words)))
                         (length typit--paragraph-break-symbol)
                       0))
         (line (mapconcat #'identity words " "))
         (len (length line))
         (split-pt (- len end-offset)))
    ;; hilight paragraph breaks
    (put-text-property 0 split-pt 'face 'typit-normal-text line)
    (put-text-property split-pt len 'face 'typit-paragraph-break-face line)
    line))

(defun typit--render-lines (offset first-line second-line)
  "Render the both lines in current buffer.

The lines are placed beginning from OFFSET (text from OFFSET to
end of buffer is deleted).  FIRST-LINE and SECOND-LINE are
rendered with `typit--render-line'."
  (let ((inhibit-read-only t))
    (delete-region offset (point-max))
    (goto-char offset)
    (insert (typit--render-line first-line)
            "\n")
    (insert (typit--render-line second-line)
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
  (let ((msg nil)
        (options '()))

    ;; build options list
    ;; reverse order - mode-specific options first
    (if typit--end-options-function
        (dolist (item (funcall typit--end-options-function))
          (push item options)))
    ;; generic options
    (push '((?p ?P) "(p)lay again" typit--test) options)
    (push '((?q ?Q) "(q)uit" (lambda () (message "quit"))) options)

    ;; build prompt message
    (dolist (item options)
      (setq msg (concat msg (if msg " | " "Choose an option: ") (nth 1 item))))

    ;; run menu
    (let ((continue t)
          (ch nil))
      (while continue
        (setq ch (read-char msg))
        (dolist (item options)
          (if (and continue
                   (member ch (car item)))
              (progn
                (setq continue nil)
                (funcall (nth 2 item)))))))))

(defun typit--report-results
    (total-time
     good-strokes
     bad-strokes
     good-words
     bad-words)
  "Report results of Typit test to the user and prompt to play again.

TOTAL-TIME, GOOD-STROKES, BAD-STROKES, GOOD-WORDS, and BAD-WORDS
are used to calculate statistics."

  ;; do any end-of-test housekeeping, the save current state
  (if typit--end-of-test-function
      (funcall typit--end-of-test-function good-strokes bad-strokes))
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
     (propertize (format "Typit results (test duration: %d seconds)" typit-test-time) 'face 'typit-title)
     "\n\n"
     (if typit--report-string-function
         (propertize (funcall typit--report-string-function) 'face 'typit-title))
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
     "\n\n"
     (mapconcat 'identity (reverse typit--all-lines) "\n"))))

(defun typit--test ()
  "Load stored state, run `init-test-function' then run typing test."

  ;; setup
  (setq typit--all-lines nil)
  (typit--load-state)
  (if typit--init-test-function
      (funcall typit--init-test-function))

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
        (current-word nil)) ; list of booleans (good/bad strokes for current word)
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
              ;; space or return
              ((or (= ch #x20)
                   (= ch #x0D))
               (when current-word
                 (typit--select-word word-offset (car first-line) t) ; unselect word
                 (cl-destructuring-bind (w . r) first-line
                   (if (cl-every #'identity current-word)
                       (setq good-words (1+ good-words))
                     (setq bad-words (1+ bad-words)))
                   ;; update variables
                   (setq
                    first-line
                    (or r second-line)
                    second-line
                    (if r second-line (typit--generate-line))
                    word-offset
                    (if r (+ word-offset 1 (length w)) init-offset)
                    good-strokes
                    (+ good-strokes (cl-count t current-word))
                    bad-strokes
                    (+ bad-strokes  (cl-count nil current-word))
                    micro-index  0
                    current-word nil) ; set current word to nil
                   ;; check whether next word is paragraph break
                   (if (equal (car first-line) typit--paragraph-break-symbol)
                       ;; YES: skip word and end line
                       (progn
                         (setq
                          first-line second-line
                          second-line (typit--generate-line)
                          word-offset init-offset
                          r nil)
                         ;; RETURN = good/SPACE = bad
                         (if (= ch #x0D)
                             (setq good-strokes (1+ good-strokes))
                           (setq bad-strokes (1+ bad-strokes))))
                     ;; NOT paragraph break
                     ;; SPACE = good/RETURN = bad
                     (if (= ch #x20)
                         (setq good-strokes (1+ good-strokes))
                       (setq bad-strokes (1+ bad-strokes))))
                   ;; if line ended, save old first-line then render new lines
                   (unless r
                     (progn
                       (push (typit--get-line-of-buffer 3) typit--all-lines)
                       (typit--render-lines init-offset first-line second-line)))
                   (typit--select-word word-offset (car first-line)))
                 (let ((total-time (- (float-time) test-started)))
                   (when (>= total-time typit-test-time)
                     ;; save text from buffer before quitting
                     (push (typit--get-line-of-buffer 3) typit--all-lines)
                     (push (typit--get-line-of-buffer 4) typit--all-lines)
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

(defun typit--run-test (title
                   pick-word-func
                   &optional
                   init-func
                   end-of-test-func
                   report-info-func
                   end-options-func
                   save-func)
  (setq typit--mode-title title
        typit--pick-word-function pick-word-func
        typit--init-test-function init-func
        typit--end-of-test-function end-of-test-func
        typit--report-string-function report-info-func
        typit--end-options-function end-options-func
        typit--save-function save-func)
  (typit--test))

(provide 'typit)

(load-file "typit-dictionary.el")
(load-file "typit-literature.el")

;;; typit.el ends here
