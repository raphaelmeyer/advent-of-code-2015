# Advent of Code 2015

[Advent of Code 2015](https://adventofcode.com/2015)

## Getting Started

Run all tests

    cabal test

Run all solutions

    cabal run aoc

Run solution for a specific day

    cabal run aoc -- --day DAY

### VS Code Setup

Open the VS Code workspace in `advent-of.code-workspace`.
Then install the recommended plugins.

### Test Output

Set default options when running `cabal test`

    cabal configure --enable-tests --test-show-details=streaming --test-option=--color

### ghcid

Install `ghcid` with `cabal install ghcid`.
Start ghcid in a terminal.

    cabal exec ghcid -- --command='cabal repl test:test'
