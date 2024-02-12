# Dotfiles

Dotfiles for keeping home directory under version control. The dotfiles
themselves are in:

- [dotfiles/common](dotfiles/common)
- [dotfiles/desktop](dotfiles/desktop)
- [dotfiles/laptop](dotfiles/laptop)

under things like `dot-zshrc`, `dot-emacs.d` for legibility.

To active the actual dotfile-symlinks in top-level, install
[GNU Stow](https://www.gnu.org/software/stow/) and try something like:

```
stow common
```
