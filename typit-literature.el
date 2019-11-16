;;; typit-literature.el --- Typing game similar to tests on 10 fast fingers -*- lexical-binding: t; -*-

(require 'typit)

(defconst typit-literature--default-text-file
  (when load-file-name
    (f-join (f-parent load-file-name) "literature" "default.txt")))

(defvar typit-literature--text-file typit-literature--default-text-file
  "File name of the text file used for `typit-literature-test'.")

(defvar typit-literature--file-marker 0
  "Current position in the literature file.")

(defvar typit-literature--words nil
  "List of words used for `typit-literature-test'.")

(defun typit-literature--pick-word ()
  "Pick a word from `typit--literature-text'."
  (if (not typit-literature--words)
      (typit-literature--prepare-words))
  (pop typit-literature--words))

(defun typit-literature--prepare-words ()
  "Setup `typit-literature--words'."
  ;; make sure that we don't carry over the last word loaded in previous test
  (setq typit--next-word nil)
  ;; make sure that literature file is set up
  (if (not typit-literature--text-file)
      (setq typit-literature--text-file (f-expand "default.txt" typit-literature-dir)))
  (if (not (f-exists? typit-literature--text-file))
      (error "file does not exist: %s" typit-literature--text-file))
  ;; get words from file
  (setq typit-literature--words
        (with-temp-buffer
          (insert-file-contents typit-literature--text-file)
          (if (or (< typit-literature--file-marker (point-min))
                  (> typit-literature--file-marker (point-max)))
              (setq typit-literature--file-marker (point-min)))
          ;; make sure that we're not part-way through a word
          (goto-char typit-literature--file-marker)
          (backward-word)
          (setq typit-literature--file-marker (point))
          ;; get a substantial chunk of text for use in test
          (let ((end-point (+ 2000 typit-literature--file-marker)))
            (if (> end-point (point-max))
                (setq end-point (point-max)))
            (typit--split-string-convert-paragraph-breaks
             (buffer-substring-no-properties typit-literature--file-marker end-point))))))

(defun typit-literature--end-of-test-cleanup (good-strokes bad-strokes)
  ;; shift file-marker along by number of strokes
  (setq typit-literature--file-marker
        (+ good-strokes bad-strokes typit-literature--file-marker)))

(defun typit-literature--get-report-string ()
  (format "Literature Test --- File: %s --- position: %d\n\nFull file path: %s\n\n"
          (f-filename typit-literature--text-file)
          typit-literature--file-marker
          typit-literature--text-file))

(defun typit-literature--get-end-options ()
  '(((?v V) "(v)isit file at point" typit-visit-literature-file)))

(defun typit-literature--save-state ()
  (typit--save-vars-to-file 'typit-literature--text-file 'typit-literature--file-marker))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top-level interface

(defun typit-set-marker-for-literature-test ()
  "Sets the current position in current file as start position for `typit-literature-test'.

ERROR if buffer has no filename."
  (interactive)
  (if (buffer-file-name)
      (progn
        (setq typit-literature--text-file (buffer-file-name))
        (setq typit-literature--file-marker (point))
        (typit--save-state))
    (error
     "Error in typit-set-marker-for-literature-test: could not get file name for buffer %s"
     (buffer-name))))

(defun typit-visit-literature-file ()
  "Visit the file currently set for use in
`typit-literature-test' and jump to the current point."
  (interactive)
  (if (not typit-literature--text-file)
      (error "typit-literature--text-file = NIL"))
  (if (not (f-exists-p typit-literature--text-file))
      (error "File does not exist: %s" typit-literature--text-file))
  (find-file typit-literature--text-file)
  (goto-char typit-literature--file-marker))

;;;###autoload
(defun typit-literature-test ()
  "Typing test with text taken from a pre-defined text file.

See `typit-literature--text-file'."
  (interactive)
  (typit--run-test "Typit Literature Test"
                   'typit-literature--pick-word
                   'typit-literature--prepare-words
                   'typit-literature--end-of-test-cleanup
                   'typit-literature--get-report-string
                   'typit-literature--get-end-options
                   'typit-literature--save-state))