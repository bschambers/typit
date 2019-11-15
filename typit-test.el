;;; typit-test.el --- typit unit tests -*- lexical-binding: t; -*-

(require 'typit)
(require 'ert)

(ert-deftest typit-test--split-string-retain-newlines ()
  (should (equal '("ginger" "ferrari" "\n" "under" "the" "rug.")
                 (typit--split-string-retain-newlines "ginger ferrari\nunder the rug.")))
  (should (equal '("bannanas" "\n" "under" "the" "bed" "\n" "convalescing" "\n" "quietly")
                 (typit--split-string-retain-newlines "bannanas\nunder the bed\nconvalescing\nquietly")))
  (should (equal '("budgies" "\n" "\n" "\n" "parrots" "&" "dolphins" "\n" "\n")
                 (typit--split-string-retain-newlines "budgies\n\n\nparrots & dolphins\n\n"))))
