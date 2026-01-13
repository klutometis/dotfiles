# Voice Dictation / Push-to-Talk

Goal: speak to the computer (especially aidermacs) via a push-to-talk hotkey, have Whisper transcribe, optionally show an overlay, and insert the text into the focused application.

## Options considered

1. Keep X11 (current i3 setup) and implement a small helper  
   • Capture global hotkey with XGrabKey or sxhkd.  
   • Run RealtimeSTT or faster-whisper as a long-lived server.  
   • Optional live captions: simple override-redirect X11 window or echo in Emacs minibuffer.  
   • Inject final text with `xdotool` / `xvkbd` or `(insert …)` via emacsclient.

2. Port whisper-overlay to X11  
   • Replace layer-shell overlay with a frameless X11 window.  
   • Use XTEST / `xdotool` for key injection.

3. Switch to Wayland (sway/hyprland)  
   • whisper-overlay works out-of-the-box.  
   • Brings Wayland benefits but requires workflow migration/testing.

4. Existing X-friendly alternatives  
   • Talon Voice, nerd-dictation, voice2json, etc.

## Recommendation

Begin with the X11 route (Option 1) for minimal disruption. Re-evaluate Wayland later when other tooling is ready.

## Next steps

- Create a small client that  
  1. Listens for a PTT key via sxhkd.  
  2. Sends start/stop to RealtimeSTT.  
  3. Inserts the final text into Emacs with emacsclient.  
- Add live caption overlay later if desired.

## Implementation Options

### Option A – Roll your own with sxhkd + RealtimeSTT

* Bind a spare key or foot-pedal in `sxhkd`.  
  `press` → start recorder, `release` → stop & insert text.
* A tiny helper script (Python/Bash, ~100 LOC) can:
  1. Spawn a long-lived RealtimeSTT recorder or `stt-server` client.  
  2. Stream audio while the key is held.  
  3. On key-release, fetch the final transcript.  
  4. Deliver it either through  
     • `emacsclient --eval '(insert "...")'` for Emacs, or  
     • `xdotool type --delay 1 -- "$TEXT"` for any other window.
* Runs completely in user-space; only needs membership of the `input`
  group.

**Pros:** Perfect Emacs integration, full control, no fake keystrokes.  
**Cons:** You own the error handling, VAD tuning & overlay code.

---

### Option B – Piggy-back on VoxInput

* Build the Go project, add yourself to `input` group, create the `uinput`
  udev rule from the README.  
* Start the daemon at login:  
  `voxinput listen --no-show-status`
* Map the same hot-key to  
  `voxinput record`  (and `voxinput stop` if running with realtime VAD).  
* VoxInput performs VAD, calls any OpenAI-compatible /realtime endpoint,
  and “types” the text via `dotool`.

**Pros:** Works in 15 minutes, status GUI, Nix flake, Wayland/X11/console.  
**Cons:** Always synthesises key events; can interfere with modal editors.

---

### API vs. Local GPU Cost Snapshot

|              | Whisper API | Used GPU (RTX 3060 12 GB) |
|--------------|-------------|---------------------------|
| 10 h / week  | ≈ $14 / mo  | $250–300 one-off + $4 / mo power |
| Occasional coding sessions | $1-3 / mo | CPU fallback free |

If you don’t already own a CUDA-capable card Daniel Rosehill’s argument
stands: pay the API and skip the hardware spend.

---

## Hardware Notes

* **USB foot pedals** – classic transcription gear; cheap HID models can
  be remapped via `evtest` + `hwdb` to emit F13-F24.
* **Single-key USB macro switches** – \$6 AliExpress units make
  delightful hand-held push-to-talk buttons.
* **Microphones / headsets** – a wired USB headset (e.g. Jabra / Poly /
  Yealink BH-70) avoids flaky Bluetooth codecs and keeps the mic close to
  your mouth.
* **Optional display** – a tiny overlay window (X11 override-redirect /
  Wayland layer-shell) can show live captions or “REC” status.

---

## References

- whisper-overlay (Wayland): https://github.com/oddlama/whisper-overlay  
- RealtimeSTT fork (includes `stt-server`): https://github.com/oddlama/RealtimeSTT  
- VoxInput (Go daemon): https://github.com/richiejp/VoxInput  
- Daniel Rosehill article on Whisper hardware & workflow:  
  https://heyitworks.tech/blog/my-openai-whisper-voice-typing-setup-for-faster-more-convenient-text-entry/
