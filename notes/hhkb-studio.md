# HHKB Studio remapping on Linux

Keymap customisation for the **HHKB Studio** (PFU `04fe:0016`,
serial `CRWT001550`, firmware `B006`) on Linux, plus recovery from
"I remapped the active profile and now can't switch back".

## TL;DR — switching between base and Vision Pro

```sh
hhkb toggle           # flip the active slot between base <-> vision-pro
hhkb base             # explicitly flash Windows default
hhkb vision-pro       # explicitly flash Dvorak/macOS
hhkb current          # which named profile is active right now
hhkb info             # device info / current slot / firmware
hhkb dump             # back all 4 slots up to ~/var/hhkb/
```

`toggle` reads the active slot, hashes it, and flashes the *other* known
profile. Takes ~1–2s. Since `Fn+C` profile-switching is dead on firmware B006
(see below), this is the only way to swap layouts.

## The tool

- Upstream: <https://github.com/yuja/hhkb-studio-tools> (Rust, MIT, ~40 commits,
  35 stars — small but the only Linux option I could find).
- Speaks raw HID to `/dev/hidrawN`. Four subcommands: `info`, `read-profile`,
  `write-profile`, `show-profile`. **No factory-reset command** — you have to
  flash a known-good profile yourself.
- Built locally to `~/bin/hhkb-studio-tools`; thin wrapper at `~/bin/hhkb`.
  The Rust binary is *not* checked in (4.5 MB); rebuild per-machine with:

  ```sh
  git clone https://github.com/yuja/hhkb-studio-tools /tmp/hhkb-studio-tools
  cd /tmp/hhkb-studio-tools && cargo build --release
  cp target/release/hhkb-studio-tools ~/bin/
  ```

## Layout

LFH-style split:

- `~/etc/hhkb/`           — source-of-truth `.toml` configs (`base.toml`,
                            `vision-pro.toml`, README, udev rule).
- `~/var/hhkb/`           — volatile state: raw `.bin` dumps, per-slot snapshots
                            (`profile0..3.toml/.bin`), `info.txt`, layout dumps.
- `~/bin/hhkb`            — wrapper script (auto-discovers hidraw, subcommands).
- `~/bin/hhkb-studio-tools` — upstream Rust binary.

## Permissions / hidraw

Each replug rebuilds the hidraw nodes with `root:root 0660`. Two options:

1. **One-shot**: `sudo setfacl -m u:$USER:rw /dev/hidrawN`
2. **Persistent** (recommended): install the udev rule, which gives the
   logged-in seat (`uaccess`) and `plugdev` rw automatically:

   ```sh
   sudo cp ~/etc/hhkb/udev-rules.d/70-hhkb-studio.rules /etc/udev/rules.d/
   sudo udevadm control --reload && sudo udevadm trigger
   ```

The keyboard exposes ~5 hidraw nodes (one per HID interface); only one
accepts the config protocol. `hhkb device` probes them and prints the
responsive one (currently `/dev/hidraw6`).

## Profiles

The Studio has **4 profile slots** of 4 layers (Base, Fn1, Fn2, Fn3) each.
Factory shipment:

| Slot (idx) | Factory contents                                  |
|------------|---------------------------------------------------|
| 1 (0)      | Windows layout                                    |
| 2 (1)      | macOS layout                                      |
| 3 (2)      | same as Profile 1 (Windows)                       |
| 4 (3)      | same as Profile 1 (Windows)                       |

The `--index` flag to the CLI is **0-based** (matches the underlying
firmware), but the manual / LEDs label them 1-based. Mind the off-by-one.

### My `vision-pro.toml`

Modifications vs `base.toml` (49 bytes differ, all on slot 2 / index 1):

- Alphas re-laid out as **Dvorak** (`',.pyfgcrl` / `aoeuidhtns` / `;qjkxbmwvz`)
  on the QWERTY positions.
- `[ ] - = /` repositioned to match Dvorak corners.
- Modifier order swapped `Alt-Meta` / `Meta-Alt` → `Meta-Alt` / `Alt-Meta`
  so Cmd sits next to Space (macOS / visionOS convention).
- Gesture-pad slide changed `5f8c/5f8d` → `5f8e/5f8f` (Alt+Tab → Cmd+Tab).

## Recovery: the `Fn+C` profile-switch shortcut doesn't work

Per the [PFU manual][switch] you switch profiles by holding `Fn` and pressing
`C`, then `1`–`4`. **On firmware `B006` (US, US-layout) this shortcut is dead**:
the physical C key on layer Fn1 is `0x00a8` = "Mute" (consumer HID), in *both*
the factory and the modified profile. So you cannot switch slots from the
keyboard — you can only flash a working profile over the active slot.

If you've nuked the only-good slot:

1. Get rw on the hidraw node from any machine (`setfacl` or the udev rule).
2. `hhkb load base --index <currently-active-slot>` to restore Windows defaults.
3. Re-flash your custom profile to a different slot when you want it back.

If you don't have a backup, dump all 4 slots first (`hhkb dump`) — slots
3/4 are factory Windows out of the box, so unless you've touched them they
are a usable "base.toml" source.

[switch]: https://happyhackingkb.com/manual/studio/ug-us/en/ug/topic/profile.html

## DIP switches

Six DIP switches on the back, all `Down` (off) from factory. SW5 affects
the macOS profile (disabled in factory). Mine reads `000010` (SW5 on).
DIP-switch state does **not** reset user keymap layers.

## Restore-to-factory by reflashing

PFU's own Keymap Tool (Windows/macOS only) has a "Restore to default" button.
Linux has no equivalent — the recovery story is:

1. Dump a known-untouched slot from your own board (`hhkb dump`) **before**
   you start customising. Slots 3/4 ship as Windows defaults, so even after
   the fact you can usually grab a clean copy from there.
2. Keep the TOML in `~/etc/hhkb/` and re-flash with `hhkb load`.

## See also

- `~/etc/hhkb/README.md` — file inventory + commands.
- `/tmp/hhkb-studio-tools/` — upstream source clone (Rust, can re-build).
- `~/var/hhkb/{base,vision-pro}.layout.txt` — human-readable layer dumps.
