# HHKB Studio keymap configs

Canonical TOML profiles for the HHKB Studio (PFU `04fe:0016`, serial
`CRWT001550`, firmware `B006`). These are the *source of truth* — flash them
to the keyboard with `hhkb load <name>`.

Source: <https://github.com/yuja/hhkb-studio-tools>

## Install (per machine)

The upstream Rust binary is not committed (4.5 MB). On a fresh machine:

```sh
git clone https://github.com/yuja/hhkb-studio-tools /tmp/hhkb-studio-tools
cd /tmp/hhkb-studio-tools && cargo build --release
cp target/release/hhkb-studio-tools ~/bin/
```

Then install the udev rule (see Permissions below) so `~/bin/hhkb` works
without sudo after replug.

## Files

| File              | Contents                                              |
|-------------------|-------------------------------------------------------|
| `base.toml`       | Factory Windows default (profiles 1, 3, 4 ship this). |
| `vision-pro.toml` | Dvorak-on-QWERTY + macOS modifier order, used as a    |
|                   | USB keyboard with Apple Vision Pro.                   |

The TOML format is `yuja/hhkb-studio-tools`'s own serialisation of the 240-byte
× 4-layer profile data (Base, Fn1, Fn2, Fn3).

## Usage

```sh
hhkb info                      # who's plugged in
hhkb current                   # which named profile is active right now
hhkb toggle                    # flip active slot base <-> vision-pro
hhkb base                      # flash base   to active slot (shortcut)
hhkb vision-pro                # flash vision-pro to active slot (shortcut)
hhkb load base       --index 0 # flash base to a specific slot (0-based)
hhkb show vision-pro           # human-readable layer dump
hhkb diff base vision-pro      # see what changes
hhkb dump                      # back up all 4 slots to ~/var/hhkb/
```

Day-to-day: just `hhkb toggle` (or `hhkb vision-pro` before plugging into the
headset, `hhkb base` when done).

Raw `.bin` dumps and per-slot snapshots live in `~/var/hhkb/`.

## Permissions

After every replug the hidraw node loses its ACL:

```sh
sudo setfacl -m u:$USER:rw /dev/hidrawN
```

A persistent udev rule lives at `udev-rules.d/70-hhkb-studio.rules`; install
with:

```sh
sudo cp ~/etc/hhkb/udev-rules.d/70-hhkb-studio.rules /etc/udev/rules.d/
sudo udevadm control --reload && sudo udevadm trigger
```
