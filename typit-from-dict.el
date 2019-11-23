;;; typit-from-dict.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-

(require 'typit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & variables

(defcustom typit-from-dict-dict "english.txt"
  "Name of dictionary file to use."
  :group 'typit
  :tag  "Dictionary to use"
  :type '(choice (const :tag "English" "english.txt")
                 (const :tag "German"  "german.txt")
                 (const :tag "French"  "french.txt")))

(defcustom typit-from-dict-dir
  (when load-file-name
    (f-slash (f-join (f-parent load-file-name) "dict")))
  "Path to directory with collection of dictionaries."
  :group 'typit
  :tag  "Directory with dictionary files"
  :type 'directory)

(defvar typit-from-dict--words nil
  "Vector of words to use for `typit-from-dict-test' (ordered from most common to least common).

If the value is NIL, it means that no dictionary has been loaded
yet.")

(defvar typit-from-dict--dict-file nil
  "File name of currently loaded dictionary.

If no dictionary is loaded, it's NIL.")

(defvar typit-from-dict--num-words 200
  "Number of words to use from the dictionary.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level functions

(defun typit-from-dict--prepare-dict ()
  "Make sure that `typit--dict' and `typit--dict-file' are set."
  (let ((dict-file (f-expand typit-from-dict-dict typit-from-dict-dir)))
    (when (or (not typit-from-dict--dict-file)
              (not (f-same? typit-from-dict--dict-file dict-file)))
      (setq typit-from-dict--dict-file dict-file
            typit-from-dict--words
            (with-temp-buffer
              (insert-file-contents dict-file)
              (vconcat
               (split-string
                (buffer-substring-no-properties
                 (point-min)
                 (point-max))
                "\n" t "[[:space:]]*")))))))

(defun typit-from-dict--pick-word ()
  "Pick a word from `typit--dict'.

Use first NUM words from loaded dictionary (if NUM is bigger than
length of the dictionary, use all words).  All words in
`typit--dict' have approximately the same probability."
  (elt typit-from-dict--words (random (min typit-from-dict--num-words
                                 (length typit-from-dict--words)))))

(defun typit-from-dict--get-report-string ()
  (format "Dictionary Test --- %d most common words\n\n"
          typit-from-dict--num-words))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface

;;;###autoload
(defun typit-from-dict-test (num)
  "Run typing test with using NUM most common words from dictionary.

Dictionary is an array of words in `typit-dict'.  By default it's
English words ordered from most common to least common.  You can
let-bind the variable and change it, it's recommended to use at
least 1000 words so `typit-advanced-test' could work properly."
  (interactive "p")
  (setq typit-from-dict--num-words num)
  (typit--run-test (format "Typit Dictionary Test - %d words" typit-from-dict--num-words)
                   'typit-from-dict--pick-word
                   'typit-from-dict--prepare-dict
                   nil
                   'typit-from-dict--get-report-string))

;;;###autoload
(defun typit-basic-test ()
  "Basic typing test (top 200 words in dictionary).

See `typit-from-dict-test' for more information."
  (interactive)
  (typit-from-dict-test 200))

;;;###autoload
(defun typit-advanced-test ()
  "Advanced typing test (top 1000 words in dictionary).

See `typit-from-dict-test' for more information."
  (interactive)
  (typit-from-dict-test 1000))
