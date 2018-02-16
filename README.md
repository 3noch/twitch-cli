# Twitch CLI

[![License](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)

A simple command-line tool to watch one or more [Glob patterns](https://hackage.haskell.org/package/Glob/docs/System-FilePath-Glob.html#v:compile) and run commands when they change. Essentially this is a more ergonomic version of [`inotifywait`](https://linux.die.net/man/1/inotifywait).

This CLI is a simple wrapper around the excellent [`twitch` library](https://github.com/jfischoff/twitch).

For a more advanced tool with similar features, checkout [steeloverseer](https://github.com/schell/steeloverseer). steeloverseer allows for more complex rules at the cost of a slightly less friendly interface.

**Built by [Grafted-In](https://graftedin.io).**


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
