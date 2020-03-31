# BookRecon

## A book finder written in Haskell.

Find the book that perfectly fits a list of genres.

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

Say you are browsing 'most read' section in 'fantasy' genre and you want to find a book that has 'urban fantasy' elements with some 'sci-fi' thrown in there but you don't want it to be a book in a 'series' or a 'young-adult' standalone.

The section has 20 books, you've gone through first 4 books but there are 16 more to go!

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

The output will look like this

```
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Processing...
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Found link for -
"Smoke Bitten (Mercy Thompson, #12)"
"Circe"
"Magical Midlife Madness (Leveling Up #1)"
"The Cruel Prince (The Folk of the Air, #1)"
"Harry Potter and the Cursed Child: Parts One and Two (Harry Potter, #8)"
"A Court of Mist and Fury (A Court of Thorns and Roses, #2)"
"Red Queen (Red Queen, #1)"
...
...
...
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Processing - https://www.goodreads.com/book/show/44571368-smoke-bitten
Processing - https://www.goodreads.com/book/show/35959740-circe
Processing - https://www.goodreads.com/book/show/50489197-magical-midlife-madness
Processing - https://www.goodreads.com/book/show/26032825-the-cruel-prince
Processing - https://www.goodreads.com/book/show/29056083-harry-potter-and-the-cursed-child
Processing - https://www.goodreads.com/book/show/17927395-a-court-of-mist-and-fury
Processing - https://www.goodreads.com/book/show/22328546-red-queen
...
...
...
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Finding genres from - https://www.goodreads.com/work/shelves/53043399
Finding genres from - https://www.goodreads.com/work/shelves/75461078
Finding genres from - https://www.goodreads.com/work/shelves/69176712
Finding genres from - https://www.goodreads.com/work/shelves/60021532
Finding genres from - https://www.goodreads.com/work/shelves/42090179
Finding genres from - https://www.goodreads.com/work/shelves/25126749
Finding genres from - https://www.goodreads.com/work/shelves/51396954
...
...
...
━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
──────────────────────────────────────────────────
Elevation
──────────────────────────────────────────────────
URL -  https://www.goodreads.com/book/show/38355410-elevation
Top 5 Genres        Matched Genres

to-read             fiction
currently-reading
2019
fiction
stephen-king
──────────────────────────────────────────────────
Battle Bond (Death Before Dragons, #2)
──────────────────────────────────────────────────
URL -  https://www.goodreads.com/book/show/51883032-battle-bond
Top 5 Genres        Matched Genres

currently-reading         fiction
to-read             urban-fantasy
urban-fantasy
fantasy
dragons
──────────────────────────────────────────────────
The Obelisk Gate (The Broken Earth, #2)
──────────────────────────────────────────────────
URL -  https://www.goodreads.com/book/show/26228034-the-obelisk-gate
Top 5 Genres        Matched Genres

to-read             fiction
currently-reading
fantasy
fiction
science-fiction
```
