# Typit

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/typit-badge.svg)](https://melpa.org/#/typit)
[![Build Status](https://travis-ci.org/mrkkrp/typit.svg?branch=master)](https://travis-ci.org/mrkkrp/typit)

This is a typing game for Emacs. In this game, you type as many words as you can
until time is up (by default it's one minute).

There are two different types of play on offer:

* Test from Dictionary: where you type words that are picked randomly from N
  most frequent words in the language you're practicing. This is quite similar
  to the “10 fast fingers” tests, with the difference that it's playable and
  fully configurable inside your Emacs.
* Test from File: the gameplay is identical although instead of random words
  picked from a dictionary, you type text from a specified file. You can use any
  text file you like, and Emacs will remember your position between sessions for
  convenience so that you can start next time just where you left off.

![Typit typing](https://raw.githubusercontent.com/mrkkrp/typit/gh-pages/typit-typing.png)

## Installation

Download this package and place it somewhere, so Emacs can see it. Then put
`(require 'typit)` into your configuration file. Done!

It's available via MELPA, so you can just <kbd>M-x package-install RET typit
RET</kbd>.

## Usage

Use one of these commands to launch Typit:

* <kbd>M-x typit-basic-test RET</kbd>: dictionary test using the 200 most common
  words in the dictionary.
* <kbd>M-x typit-advanced-test RET</kbd>: dictionary test using the 1000 most
  common words in the dictionary.
* <kbd>M-x typit-from-dict-test RET</kbd>: dictionary test with a numeric
  argument specifying how many words to use (note that default dictionary has
  1000 words total at the moment).
* <kbd>M-x typit-from-file RET</kbd>: literature test using the currently saved
  literature file, or the default file if no other has been chosen.

The Typit window should appear (see the picture above). Timer will start when
you start typing. When you are done, the following statistics will appear:

![Typit results](https://raw.githubusercontent.com/mrkkrp/typit/gh-pages/typit-results.png)

Some other useful user commands are also provided:

* <kbd>M-x typit-visit-text-file</kbd>: visit the current text file and jump to
  the current point.
* <kbd>M-x typit-set-text-file-marker</kbd>: sets the starting point for
  `typit-from-file` to the current point in the current file.
* <kbd>M-x typit-replace-special-characters-in-buffer</kbd>: replaces certain
  special characters with easier-to-type alternatives. You can use this to help
  cleanup and prepare text files for use in from-file test.

## Customization and Persistence

There are some configuration parameters that allow you change things like:

* Faces controlling appearance of various UI elements
* Dictionary to use (e.g. this allows to switch between languages)
* Location of dictionary directory (usually it's automatically detected)
* Length of generated line of words (in characters)
* Test duration in seconds
* Text file to use
* Point to start in text file

To access these, type <kbd>M-x customize-group RET typit RET</kbd>.

The last two parameters can also be set by typing <kbd>M-x
typit-set-text-file-marker</kbd> in any text file at a point of your
choosing. The file will remain set until you set it to something else but the
starting point is updated automatically every time that you reach the end of the
from-file test, meaning that you can very easily type throught the whole text in
sequence if you desire. If you reach the end of the text file then Typit will
loop back around to the beginning.

## Preparing Text Files for the From-File Test

Any text file can be used, although you may want to take some things into
consideration to ensure the best experience:

* The file should end with two newlines in a row - this is how paragraph breaks
  are recongnised so if you don't have two newlines at the end then when
  wraparound happens the last line and the first line will run together.
* The file should not contain any characters which are not typable on a standard
  keyboard.

You can use <kbd>M-x typit-replace-special-characters-in-buffer</kbd> to help
convert or get rid of unwanted special characters. This will convert a number of
common special characters e.g. “ or ” to ", é to e etc. Just visit the file, run
the function and then save.

You can add your own custom substitutions to extend what this function does by
adding to the <kbd>typit--symbol-substitution-hash-table</kbd>. To make
persistent additions you could put something like this in your .emacs file:

<kbd>(eval-after-load 'typit
  (puthash "_" "" typit--symbol-substitution-hash-table))
</kbd>

As an example, I used the above to clean up a copy of Jonathan Swift's *Polite
Conversation* (pub. 1738) downloaded from Project Gutenberg, which was
completely riddled with unnecessary underscores (I just got rid of them
all). This is an excerpt of what it was like before cleanup:

>    [\_Lady\_ Smart’\_s Antichamber\_.
>
>     \_Lady\_ Smart \_and Lady\_ Answerall \_at the Tea-table\_.
>
> \_Lady Smart.\_ My Lord, your Lordship’s most humble Servant.
>
> \_Ld. Sparkish.\_ Madam, you spoke too late; I was your Ladyship’s before.
>
> \_Lady Smart.\_ Oh! Colonel, are you here!
>
> \_Col.\_ As sure as you’re there, Madam.
>
> \_Lady Smart.\_ Oh, Mr. \_Neverout\_! what, such a Man alive!
>
> \_Neverout.\_ Ay, Madam; alive, and alive like to be, at your Ladyship’s Service.

## Contribution

If you would like to improve the package, PR and issues are welcome. Also,
it's OK to add dictionaries for other languages than English. To do so, you
need to create a text file named `your-language.txt` and put it under the
`dict` directory. That file should contain 1000 most common words from the
language, a word per line. Please make sure that it uses Unix-style (that
is, LF) end-of-line sequence and the file ends with a newline.

Once dictionary file is created, add it to the definition of `typit-dict`
customization parameter in `typit.el`. To try the game with a new language
added, change value of `typit-dict` accordingly via the customization
interface, `setq`, or with `let`-binding, and then run one of the commands
that start the game (`typit-basic-test`, `typit-advanced-test`, or
`typit-test`).

### Additional Note
I have made quite extensive modifications to Mark Karpov's version in order to
add the from-file test feature, paragraph breaks and multi-line scrolling
etc. At time of writing this I am not very experienced in writing Lisp code and
would be very happy to see anyone else improve it or to receive any constructive
criticism. (B. S. Chambers)

## License

Original work Copyright © 2016–present Mark Karpov

Modified work Copyright © 2019–present B. S. Chambers

Distributed under GNU GPL, version 3.
