# Typit

[![License GPL 3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)
[![MELPA](https://melpa.org/packages/typit-badge.svg)](https://melpa.org/#/typit)
[![Build Status](https://travis-ci.org/mrkkrp/typit.svg?branch=master)](https://travis-ci.org/mrkkrp/typit)

This is a typing game for Emacs. In this game, you type as many words as you can
until time is up (by default it's one minute).

There are two different types of play on offer:

* Dictionary test: where you type words that are picked randomly from N most
  frequent words in the language you're practicing. This is quite similar to the
  “10 fast fingers” tests, with the difference that it's playable and fully
  configurable inside your Emacs.
* Literature test: the gameplay is identical to the dictionary test although
  instead of random words picked from a dictionary, you type text from a
  specified file. You can use text file you like, and Emacs will save your
  position between sessions for convenience so that you will always start just
  where you left off last time.

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
* <kbd>M-x typit-dictionary-test RET</kbd>: dictionary test with a numeric
  argument specifying how many words to use (note that default dictionary has
  1000 words total at the moment).
* <kbd>M-x typit-literature-test RET</kbd>: literature test using the currently
  saved literature file, or the default file if no other has been chosen.

The Typit window should appear (see the picture above). Timer will start when
you start typing. When you are done, the following statistics will appear:

![Typit results](https://raw.githubusercontent.com/mrkkrp/typit/gh-pages/typit-results.png)

Some other useful user commands are also provided:

* <kbd>M-x typit-visit-literature-file</kbd>: visit the current literature file
  and jump to the current point.
* <kbd>M-x typit-set-marker-for-literature-test</kbd>: sets the starting point
  for `typit-literature-test` to the current point in the current file.

## Save-state file

Typit saves the values of some variables to file in order to ensure continuity
between sessions.

The default save file is `~/.emacs.d/.typit`

## Customization

There are some configuration parameters that allow you change things like:

* Faces controlling appearance of various UI elements
* Dictionary to use (e.g. this allows to switch between languages)
* Location of dictionary directory (usually it's automatically detected)
* Length of generated line of words (in characters)
* Test duration in seconds

To access these, type <kbd>M-x customize-group RET typit RET</kbd>.

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

## License

Copyright © 2016–present Mark Karpov

Distributed under GNU GPL, version 3.
