# TODO

## Content Extraction & Media

### Fetch.AI Event Video
- Extract video from Fetch.AI event (interview with folks for health-tech week)
- Extract still of slide to print for dad

## Harvard Submission
- Prepare headshot for Harvard
- Prepare bio for Harvard

## Website Update
- Update danenberg.ai after running LI-extraction
- Include latest Davos talks in the update

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

## Emacs Configuration

### High Priority
- [ ] Evaluate `projectile` vs built-in `project.el`
