"""Compose (ABC across providers) → render (abc2midi + fluidsynth → WAV) → judge
(Gemini, by ear). Provider-agnostic core with per-step logging + retry."""

from __future__ import annotations

import os
import re
import sys
import time
import pathlib
import subprocess

SOUNDFONT = os.environ.get("GOLDBERG_SF2", "/usr/share/sounds/sf2/FluidR3_GM.sf2")


def log(msg: str) -> None:
    print(f"[{time.strftime('%H:%M:%S')}] {msg}", file=sys.stderr, flush=True)


def _key(*names: str) -> str:
    for n in names:
        v = os.environ.get(n)
        if v:
            return v
    raise RuntimeError("Set one of: " + ", ".join(names))


def _retry(desc: str, fn, tries: int = 3, delay: int = 4):
    last = None
    for i in range(1, tries + 1):
        try:
            return fn()
        except Exception as e:  # noqa: BLE001
            last = e
            log(f"  ! {desc}: attempt {i}/{tries} failed: {type(e).__name__}: {str(e)[:200]}")
            if i < tries:
                time.sleep(delay * i)
    raise last


# ---------------------------------------------------------------- compose

def compose(provider: str, model: str, prompt: str) -> str:
    log(f"compose[{provider}/{model}]: sending ({len(prompt)} chars)")

    def call():
        if provider == "anthropic":
            import anthropic
            c = anthropic.Anthropic(api_key=_key("ANTHROPIC_API_KEY"))
            m = c.messages.create(model=model, max_tokens=8000,
                                  messages=[{"role": "user", "content": prompt}])
            txt = "".join(b.text for b in m.content if getattr(b, "type", "") == "text")
            if not txt:
                log(f"  anthropic returned no text; stop_reason={getattr(m,'stop_reason',None)} "
                    f"blocks={[getattr(b,'type','?') for b in m.content]}")
            return txt
        if provider == "gemini":
            from google import genai
            c = genai.Client(api_key=_key("GEMINI_API_KEY", "GOOGLE_API_KEY"))
            r = c.models.generate_content(model=model, contents=prompt)
            return r.text or ""
        if provider == "openai":
            from openai import OpenAI
            c = OpenAI(api_key=_key("OPENAI_API_KEY"))
            r = c.chat.completions.create(model=model,
                                          messages=[{"role": "user", "content": prompt}])
            return r.choices[0].message.content or ""
        raise ValueError(f"unknown provider: {provider}")

    out = _retry(f"compose {provider}/{model}", call)
    log(f"compose[{provider}/{model}]: got {len(out)} chars")
    return out


def extract_abc(text: str) -> str:
    t = text.strip()
    if "```" in t:
        blocks = re.findall(r"```(?:abc)?\s*\n(.*?)```", t, re.S)
        if blocks:
            t = blocks[0]
    i = t.find("X:")
    return (t[i:] if i >= 0 else t).strip()


# ---------------------------------------------------------------- render

def render(abc: str, stem) -> tuple[pathlib.Path, pathlib.Path]:
    stem = pathlib.Path(stem)
    stem.parent.mkdir(parents=True, exist_ok=True)
    abcp = stem.with_suffix(".abc")
    midp = stem.with_suffix(".mid")
    wavp = stem.with_suffix(".wav")
    abcp.write_text(abc)
    log(f"render[{stem.name}]: abc {len(abc)}c -> abc2midi")
    r = subprocess.run(["abc2midi", str(abcp), "-o", str(midp)], capture_output=True, text=True)
    if not midp.exists():
        raise RuntimeError(f"abc2midi produced no MIDI. out={r.stdout[-200:]!r} err={r.stderr[-200:]!r}")
    log(f"render[{stem.name}]: midi ok -> fluidsynth")
    r = subprocess.run(["fluidsynth", "-ni", "-F", str(wavp), "-r", "44100",
                        SOUNDFONT, str(midp)], capture_output=True, text=True)
    if not wavp.exists() or wavp.stat().st_size < 1000:
        raise RuntimeError(f"fluidsynth produced no WAV. err={r.stderr[-200:]!r}")
    # small mono/22kHz OGG for the judge + quick A/B (uploads instantly; full WAV kept)
    oggp = stem.with_suffix(".ogg")
    subprocess.run(["ffmpeg", "-y", "-i", str(wavp), "-ac", "1", "-ar", "22050",
                    "-q:a", "4", str(oggp)], capture_output=True, text=True)
    audio = oggp if (oggp.exists() and oggp.stat().st_size > 500) else wavp
    log(f"render[{stem.name}]: wav {wavp.stat().st_size // 1024}KB, judge={audio.name} "
        f"{audio.stat().st_size // 1024}KB")
    return midp, audio


def to_lilypond(abc_path, ly_path) -> pathlib.Path:
    ly_path = pathlib.Path(ly_path)
    subprocess.run(["abc2ly", "-o", str(ly_path), str(abc_path)], capture_output=True, text=True)
    return ly_path


# ---------------------------------------------------------------- judge

def _upload(client, wav):
    f = client.files.upload(file=str(wav))
    while getattr(f.state, "name", "ACTIVE") == "PROCESSING":
        time.sleep(1)
        f = client.files.get(name=f.name)
    return f


def judge(entries, brief: str, model: str = "gemini-2.5-pro") -> str:
    """entries: [(name, wav_path)]. Gemini listens and critiques/ranks by ear."""
    from google import genai
    c = genai.Client(api_key=_key("GEMINI_API_KEY", "GOOGLE_API_KEY"))
    n = len(entries)
    log(f"judge[{model}]: uploading {n} wav(s)")
    head = (
        "You are the judge of a music composition bake-off. Listen to each piece "
        "as a discerning human musician would — by EAR, not by inspecting notation."
        f"\n\nThe brief given to the composers:\n{brief}\n\n"
        f"You will now hear {n} piece(s). For each, give a short, honest critique "
        "(musicality, line, harmony, coherence; does it sing; does it work as an "
        "Aria — a theme that could bear variations). "
        + ("Then rank them best-to-worst. End your reply with exactly one line:\n"
           "WINNER: <name>"
           if n > 1 else "Then give concrete, specific suggestions for the next revision.")
    )
    contents = [head]
    for name, wav in entries:
        contents.append(f"--- Piece: {name} ---")
        contents.append(_upload(c, wav))
    log(f"judge[{model}]: generating verdict")

    def call():
        r = c.models.generate_content(model=model, contents=contents)
        return r.text or ""

    out = _retry(f"judge {model}", call)
    log(f"judge[{model}]: verdict {len(out)} chars")
    return out


def parse_winner(verdict: str, names) -> str:
    m = re.search(r"WINNER:\s*([A-Za-z0-9_-]+)", verdict)
    if m:
        w = m.group(1)
        for n in names:
            if n.lower() == w.lower():
                return n
    low = verdict.lower()
    return min(names, key=lambda n: (low.find(n.lower()) if n.lower() in low else 1e9))
