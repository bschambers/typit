;;; typit-dictionary.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-

(require 'typit)

(defcustom typit-dictionary-dict "english.txt"
  "Name of dictionary file to use."
  :group 'typit
  :tag  "Dictionary to use"
  :type '(choice (const :tag "English" "english.txt")
                 (const :tag "German"  "german.txt")
                 (const :tag "French"  "french.txt")))

(defcustom typit-dictionary-dir
  (when load-file-name
    (f-slash (f-join (f-parent load-file-name) "dict")))
  "Path to directory with collection of dictionaries."
  :group 'typit
  :tag  "Directory with dictionary files"
  :type 'directory)

(defvar typit-dictionary--dict-words nil
  "Vector of words to use for `typit-dictionary-test' (ordered from most common to least common).

If the value is NIL, it means that no dictionary has been loaded
yet.")

(defvar typit-dictionary--dict-file nil
  "File name of currently loaded dictionary.

If no dictionary is loaded, it's NIL.")

(defvar typit-dictionary--dict-num-words 200
  "Number of words to use from the dictionary.")

(defun typit-dictionary--prepare-dict ()
  "Make sure that `typit--dict' and `typit--dict-file' are set."
  (let ((dict-file (f-expand typit-dictionary-dict typit-dictionary-dir)))
    (when (or (not typit-dictionary--dict-file)
              (not (f-same? typit-dictionary--dict-file dict-file)))
      (setq typit-dictionary--dict-file dict-file
            typit-dictionary--dict-words
            (with-temp-buffer
              (insert-file-contents dict-file)
              (vconcat
               (split-string
                (buffer-substring-no-properties
                 (point-min)
                 (point-max))
                "\n" t "[[:space:]]*")))))))

(defun typit-dictionary--pick-word ()
  "Pick a word from `typit--dict'.

Use first NUM words from loaded dictionary (if NUM is bigger than
length of the dictionary, use all words).  All words in
`typit--dict' have approximately the same probability."
  (elt typit-dictionary--dict-words (random (min typit-dictionary--dict-num-words
                                 (length typit-dictionary--dict-words)))))

(defun typit-dictionary--get-report-string ()
  (format "Dictionary Test --- %d most common words\n\n"
          typit-dictionary--dict-num-words))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface

;;;###autoload
(defun typit-dictionary-test (num)
  "Run typing test with using NUM most common words from dictionary.

Dictionary is an array of words in `typit-dict'.  By default it's
English words ordered from most common to least common.  You can
let-bind the variable and change it, it's recommended to use at
least 1000 words so `typit-advanced-test' could work properly."
  (interactive "p")
  (setq typit-dictionary--dict-num-words num)
  (typit--run-test "Typit Dictionary Test"
                   'typit-dictionary--pick-word
                   #'typit-dictionary--prepare-dict
                   (lambda (gs bs))
                   'typit-dictionary--get-report-string
                   (lambda () '())
                   nil))

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
