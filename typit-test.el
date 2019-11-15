;;; typit-test.el --- typit unit tests -*- lexical-binding: t; -*-

(require 'typit)
(require 'ert)

(ert-deftest typit-test--split-string-convert-paragraph-breaks ()
  (should (equal `("apple" "batcave" "cat" "dog" "entwine" ,typit--paragraph-break-symbol "flapper" "geometry" "handbag" ,typit--paragraph-break-symbol)
                 (typit--split-string-convert-paragraph-breaks
                  "apple batcave\ncat dog entwine\n\nflapper geometry\nhandbag\n\n")))
  (should (equal `("apple" "batcave" "cat" "dog" ,typit--paragraph-break-symbol "entwine" "flapper" "geometry" "handbag")
                 (typit--split-string-convert-paragraph-breaks
                  "apple batcave\ncat dog \n \n     \n\nentwine flapper geometry\nhandbag\n"))))
