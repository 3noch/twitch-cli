# Twitch CLI

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

A simple command-line tool to watch one or more Glob patterns and run commands when they change. Essentially this is a more ergonomic version of `inotifywait`.


## Usage

```bash
twitch --help

# Watches for .pyc files at any depth and deletes
# them when created or modified.
twitch --no-debounce -p '**/*.pyc:rm $FILE'

# Watches .cabal files and reconfigures when they change.
# Also watches .hs files at any depth in the src folder
# and rebuilds project when they change.
twitch -p '*.cabal:cabal configure && cabal build' 'src/**/*.hs:cabal build'

# Watches all .log files in /var/log and prints their last line
# when they change.
twitch --no-debounce --dir /var/log -p '*.log:tail -n 1 $FILE'
```


## Development

Build with one of:

  * `stack build`
  * `nix-shell --run 'cabal build'`
  * `nix-build`

Develop with one of:

  * `stack ghci`
  * `nix-shell --run 'cabal repl'`
