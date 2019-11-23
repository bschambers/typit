;;; typit-from-file.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-

(require 'typit)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Settings & variables

(defconst typit-from-file--default-text-file
  (when load-file-name
    (f-join (f-parent load-file-name) "texts" "default.txt")))

(defcustom typit-from-file-text-file typit-from-file--default-text-file
  "File name of the text file used for `typit-from-file-test'."
  :group 'typit
  :tag "Text file for typit-from-file-test"
  :type 'file)

(defcustom typit-from-file-file-marker 0
  "Current position in the text file."
  :group 'typit
  :tag "Current position in text file"
  :type 'integer)

(defvar typit-from-file--file-length 0)

(defvar typit-from-file--file-sel-beg nil
  "The end point of the last body of text taken from the file.

The value is set to NIL at the start of every test to indicate
that the text selection should start with `typit-from-file--file-marker' for
the first time of each session.")

(defvar typit-from-file--file-sel-end nil
  "The end point of the last body of text taken from the file.

The value is set to NIL at the start of every test to indicate
that the text selection should start with `typit-from-file--file-marker' for
the first time of each session.")

(defvar typit-from-file--words nil
  "List of words used for `typit-from-file-test'.

The initial value is NIL. The list will be created when needed by
`typit-from-file--pick-word'.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Low-level functions

(defun typit-from-file--pick-word ()
  "Gets the next word from the text file."
  (if (not typit-from-file--words)
      (typit-from-file--prepare-words))
  (pop typit-from-file--words))

(defun typit-from-file--init-test ()
  "Initialise and get every thing ready to start the test."
  (setq typit-from-file--file-sel-beg nil
        typit-from-file--file-sel-end nil)
  (typit-from-file--prepare-words))

(defun typit-from-file--prepare-words ()
  "Setup `typit-from-file--words' by taking a chunk of text from
file. If the file doesn't exist then use the default file. If
default file doesn't exist then throw an error."
  ;; make sure that we don't carry over the last word loaded in previous test
  (setq typit--next-word nil)
  ;; make sure that text file is set up
  (if (not typit-from-file-text-file)
      (setq typit-from-file-text-file (f-expand "default.txt" typit-from-file-dir)))
  (if (not (f-exists? typit-from-file-text-file))
      (error "file does not exist: %s" typit-from-file-text-file))
  ;; get words from file
  (setq typit-from-file--words
        (with-temp-buffer
          (insert-file-contents typit-from-file-text-file)
          (setq typit-from-file--file-length (point-max))
          (setq typit-from-file--file-sel-beg (if typit-from-file--file-sel-beg
                                   typit-from-file--file-sel-beg
                                 typit-from-file-file-marker))
          (if (or (< typit-from-file--file-sel-beg (point-min))
                  (> typit-from-file--file-sel-beg typit-from-file--file-length))
              (setq typit-from-file--file-sel-beg (point-min)))
          ;; make sure that we're not part-way through a word
          (goto-char typit-from-file--file-sel-beg)
          (forward-word)
          (backward-word)
          (setq typit-from-file--file-sel-beg (point))
          ;; get the next several words
          ;; NOTE: forward-word will stop at end of file without throwing error
          (forward-word 20)
          (setq typit-from-file--file-sel-end (point))
          ;; get the text and split it
          (typit--split-string-convert-paragraph-breaks
           (buffer-substring-no-properties typit-from-file--file-sel-beg typit-from-file--file-sel-end))))
  ;; set start point for next time, wrapping to beginning of file if required
  (setq typit-from-file--file-sel-beg (if (< typit-from-file--file-sel-end typit-from-file--file-length)
                           typit-from-file--file-sel-end
                         (point-min))))

(defun typit-from-file--point-of-nearest-instance (word)
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

(defun typit-from-file--end-of-test-cleanup (good-strokes bad-strokes)
  ;; shift file-marker along by number of strokes
  (setq typit-from-file-file-marker
        (+ typit-from-file-file-marker good-strokes bad-strokes))
  ;; wrap around to beginning if required
  (if (>= typit-from-file-file-marker typit-from-file--file-length)
      (setq typit-from-file-file-marker 0))
  ;; adjustment: look for next word in file
  (setq typit-from-file-file-marker
        (with-current-buffer (find-file-noselect typit-from-file-text-file)
          (goto-char typit-from-file-file-marker)
          (typit-from-file--point-of-nearest-instance typit--next-word-to-type))))

(defun typit-from-file--get-report-string ()
  (format "Typit test (from file) --- file: %s --- position: %d\n\nPath: %s\n\n"
          (f-filename typit-from-file-text-file)
          typit-from-file-file-marker
          typit-from-file-text-file))

(defun typit-from-file--get-end-options ()
  '(((?v V) "(v)isit file at point" typit-visit-text-file)))

(defun typit-from-file--save-state ()
  (typit--save-vars-to-customize 'typit-from-file-text-file 'typit-from-file-file-marker))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface

(defun typit-set-text-file-marker ()
  "Sets the current position in current file as start position for `typit-from-file-test'.

ERROR if buffer has no filename."
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq typit-from-file-text-file (buffer-file-name))
        (setq typit-from-file-file-marker (point))
        (typit-from-file--save-state))
    (error
     "Error in typit-set-text-file-marker: could not get file name for buffer %s"
     (buffer-name))))

(defun typit-visit-text-file ()
  "Visit the file currently set for use in
`typit-from-file-test' and jump to the current point."
  (interactive)
  (if (not typit-from-file-text-file)
      (error "typit-from-file-text-file = NIL"))
  (if (not (f-exists-p typit-from-file-text-file))
      (error "File does not exist: %s" typit-from-file-text-file))
  (find-file typit-from-file-text-file)
  (goto-char typit-from-file-file-marker))

;;;###autoload
(defun typit-from-file ()
  "Typing test with text taken from a pre-defined text file.

See `typit-from-file-text-file'."
  (interactive)
  (typit--run-test (format "Typit from file - using %s" (f-filename typit-from-file-text-file))
                   'typit-from-file--pick-word
                   'typit-from-file--init-test
                   'typit-from-file--end-of-test-cleanup
                   'typit-from-file--get-report-string
                   'typit-from-file--get-end-options
                   'typit-from-file--save-state))
