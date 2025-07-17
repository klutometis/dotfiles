# TODO

## Bazelify the scripts

Stow handles relative symlinks; Bazel builds?

## Desktop- and laptop-specific stow

Right now, things like `.screenrc` are all in common; separate out the desk- and
lap-top specific things.

## Surfraw from selection

`stumpwm` module is too fragile: create a script which is `surfraw-sel` (or
similar), which takes the search term from the selection (or clipboard).

Also, rebind to `exec sr code-search`, etc.; which means we can't use the
stumpwm-prompt.
