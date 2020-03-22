# HBookRecon

## A book finder written in Haskell.

Find the book that perfectly fits a list of genres.

## Usage

Say you are browsing 'most read' section in 'fantasy' genre and you want to find a book that has 'urban fantasy' elements with some 'sci-fi' thrown in there but you don't want it to be a book in a 'series' or a 'young-adult' standalone.

The section has 20 books, you've gone through first 4 books but there are 16 more to go!

So this is what you do:

```sh
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
