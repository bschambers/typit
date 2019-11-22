;;; typit-literature.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-

(require 'typit)

(defconst typit-literature--default-text-file
  (when load-file-name
    (f-join (f-parent load-file-name) "literature" "default.txt")))

(defcustom typit-literature-text-file typit-literature--default-text-file
  "File name of the text file used for `typit-literature-test'."
  :group 'typit
  :tag "Text file for literature test"
  :type 'file)

(defcustom typit-literature-file-marker 0
  "Current position in the literature file."
  :group 'typit
  :tag "Current position in literature-test file"
  :type 'integer)

(defvar typit-literature--file-length 0)

(defvar typit-literature--file-sel-beg nil
  "The end point of the last body of text taken from the file.

The value is set to NIL at the start of every test to indicate
that the text selection should start with `typit-literature--file-marker' for
the first time of each session.")

(defvar typit-literature--file-sel-end nil
  "The end point of the last body of text taken from the file.

The value is set to NIL at the start of every test to indicate
that the text selection should start with `typit-literature--file-marker' for
the first time of each session.")

(defvar typit-literature--words nil
  "List of words used for `typit-literature-test'.

The initial value is NIL. The list will be created when needed by
`typit-literature--pick-word'.")

(defun typit-literature--pick-word ()
  "Pick a word from `typit--literature-text'."
  (if (not typit-literature--words)
      (typit-literature--prepare-words))
  (pop typit-literature--words))

(defun typit-literature--init-test ()
  "Initialise and get every thing ready to start the test."
  (setq typit-literature--file-sel-beg nil
        typit-literature--file-sel-end nil)
  (typit-literature--prepare-words))

(defun typit-literature--prepare-words ()
  "Setup `typit-literature--words' by taking a chunk of text from
file. If the file doesn't exist then use the default file. If
default file doesn't exist then throw an error."
  ;; make sure that we don't carry over the last word loaded in previous test
  (setq typit--next-word nil)
  ;; make sure that literature file is set up
  (if (not typit-literature-text-file)
      (setq typit-literature-text-file (f-expand "default.txt" typit-literature-dir)))
  (if (not (f-exists? typit-literature-text-file))
      (error "file does not exist: %s" typit-literature-text-file))
  ;; get words from file
  (setq typit-literature--words
        (with-temp-buffer
          (insert-file-contents typit-literature-text-file)
          (setq typit-literature--file-length (point-max))
          (setq typit-literature--file-sel-beg (if typit-literature--file-sel-beg
                                   typit-literature--file-sel-beg
                                 typit-literature-file-marker))
          (if (or (< typit-literature--file-sel-beg (point-min))
                  (> typit-literature--file-sel-beg typit-literature--file-length))
              (setq typit-literature--file-sel-beg (point-min)))
          ;; make sure that we're not part-way through a word
          (goto-char typit-literature--file-sel-beg)
          (forward-word)
          (backward-word)
          (setq typit-literature--file-sel-beg (point))
          ;; get the next several words
          ;; NOTE: forward-word will stop at end of file without throwing error
          (forward-word 20)
          (setq typit-literature--file-sel-end (point))
          ;; get the text and split it
          (typit--split-string-convert-paragraph-breaks
           (buffer-substring-no-properties typit-literature--file-sel-beg typit-literature--file-sel-end))))
  ;; set start point for next time, wrapping to beginning of file if required
  (setq typit-literature--file-sel-beg (if (< typit-literature--file-sel-end typit-literature--file-length)
                           typit-literature--file-sel-end
                         (point-min))))

(defun typit-literature--point-of-nearest-instance (word)
  "Search backwards and forwards and return the point of the
nearest match found for WORD, or the start point if no instance
is found. The point returned for a match is the first letter of
WORD, regardless of whether it was the backward or forward search
which was selected."
  (save-excursion
    ;; \\b = regexp word-boundary
    (let* ((regex (concat "\\b" word "\\b"))
           (start (point))
           (back-pos 0)
           (forward-pos 0))
      ;; find back/forward pos: either or both of these may be NIL
      (setq back-pos
            (search-backward-regexp regex (point-min) t))
      (goto-char start)
      (setq forward-pos
            (search-forward-regexp regex (point-max) t))
      ;; adjust forward-pos: search-forward returns pos at end of word but we
      ;; want the beginning
      (if forward-pos
          (progn
            (goto-char forward-pos)
            (backward-word)
            (setq forward-pos (point))))
      ;; which position to return?
      (if (and back-pos forward-pos)
          ;; neither is NIL: return nearest
          (if (< (abs (- start back-pos))
                 (abs (- forward-pos start)))
              back-pos
            forward-pos)
        ;; one or both is NIL: return the other one, or start point as default
        (or back-pos
            forward-pos
            start)))))

(defun typit-literature--end-of-test-cleanup (good-strokes bad-strokes)
  ;; shift file-marker along by number of strokes
  (setq typit-literature-file-marker
        (+ typit-literature-file-marker good-strokes bad-strokes))
  ;; wrap around to beginning if required
  (if (>= typit-literature-file-marker typit-literature--file-length)
      (setq typit-literature-file-marker 0))
  ;; adjustment: look for next word in file
  (setq typit-literature-file-marker
        (with-current-buffer (find-file-noselect typit-literature-text-file)
          (goto-char typit-literature-file-marker)
          (typit-literature--point-of-nearest-instance typit--next-word-to-type))))

(defun typit-literature--get-report-string ()
  (format "Literature Test --- File: %s --- position: %d\n\nPath: %s\n\n"
          (f-filename typit-literature-text-file)
          typit-literature-file-marker
          typit-literature-text-file))

(defun typit-literature--get-end-options ()
  '(((?v V) "(v)isit file at point" typit-visit-literature-file)))

(defun typit-literature--save-state ()
  (typit--save-vars-to-customize 'typit-literature-text-file 'typit-literature-file-marker))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface

(defun typit-set-marker-for-literature-test ()
  "Sets the current position in current file as start position for `typit-literature-test'.

ERROR if buffer has no filename."
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq typit-literature-text-file (buffer-file-name))
        (setq typit-literature-file-marker (point))
        (typit-literature--save-state))
    (error
     "Error in typit-set-marker-for-literature-test: could not get file name for buffer %s"
     (buffer-name))))

(defun typit-visit-literature-file ()
  "Visit the file currently set for use in
`typit-literature-test' and jump to the current point."
  (interactive)
  (if (not typit-literature-text-file)
      (error "typit-literature-text-file = NIL"))
  (if (not (f-exists-p typit-literature-text-file))
      (error "File does not exist: %s" typit-literature-text-file))
  (find-file typit-literature-text-file)
  (goto-char typit-literature-file-marker))

;;;###autoload
(defun typit-literature-test ()
  "Typing test with text taken from a pre-defined text file.

See `typit-literature-text-file'."
  (interactive)
  (typit--run-test (format "Typit Literature Test - using %s" (f-filename typit-literature-text-file))
                   'typit-literature--pick-word
                   'typit-literature--init-test
                   'typit-literature--end-of-test-cleanup
                   'typit-literature--get-report-string
                   'typit-literature--get-end-options
                   'typit-literature--save-state))
