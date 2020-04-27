# BookRecon

## A book finder written in Haskell.

Find the book that perfectly fits a list of genres.

![bookrecon demo](bookrecon.gif)

## Installation

Best way is using [stack](https://docs.haskellstack.org/en/stable/README/).

Installation instructions are in the [README](https://docs.haskellstack.org/en/stable/README/#how-to-install).

```
cd BookRecon
stack setup
stack build
stack install
```

## Usage

You are browsing this week's most-read in 'fantasy' section on goodreads and you want to find a book that has

1. 'urban fantasy' elements with some
2. 'sci-fi' thrown in there

but you don't want it to be

3. a 'series' or
4. a 'young-adult' standalone.

The section has 20 books, you've gone through first 4 but you got 16 more to go!

So this is what you do:

```
bookrecon -l https://www.goodreads.com/genres/most_read/fantasy
          -f 4
          -t 20
          -g urban-fantasy -g sci-fi
          -n series -n young-adult
```


`-l` expects a link.

`-f` expects a "search from" number.

`-t` expects a "search to" number.

`-g` expects the genres to match.

`-n` expects genres not to match (completely exclude).
