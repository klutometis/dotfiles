"""goldberg CLI. Two comparable strategies:
  parallel   — siloed self-iteration (each model its own Ralph loop), then compare
  tournament — champion relay (all models revise the reigning champion each round)
Plus bakeoff (one-shot) and iterate (single model). Composers run in parallel."""

from __future__ import annotations

import argparse
import datetime
import pathlib
from concurrent.futures import ThreadPoolExecutor

import yaml

from . import core


def _load(spec: str) -> dict:
    return yaml.safe_load(open(spec))


def _prompt(d: dict) -> str:
    return f"{d['brief'].strip()}\n\n{d.get('constraints', '').strip()}".strip()


def _outdir(a, d) -> pathlib.Path:
    stamp = datetime.datetime.now().strftime("%Y%m%d-%H%M%S")
    sub = getattr(a, "cmd", "run")
    return pathlib.Path(a.out or (pathlib.Path(a.spec).parent / "out" / f"{sub}-{stamp}"))


def _jmodel(d):
    return d.get("judge", {}).get("model", "gemini-2.5-pro")


def _init(out, brief):
    out.mkdir(parents=True, exist_ok=True)
    (out / "brief.md").write_text(brief)
    return out


def _compose_render(comp, prompt, stem):
    """One model: compose → save raw+abc → render. Returns (name, abc, wav) or None."""
    stem = pathlib.Path(stem)
    stem.parent.mkdir(parents=True, exist_ok=True)
    try:
        raw = core.compose(comp["provider"], comp["model"], prompt)
        stem.with_suffix(".raw.md").write_text(raw or "")
        abc = core.extract_abc(raw)
        stem.with_suffix(".abc").write_text(abc)
        _, wav = core.render(abc, stem)
        return (comp["name"], abc, wav)
    except Exception as e:  # noqa: BLE001
        core.log(f"FAILED {comp['name']}: {type(e).__name__}: {str(e)[:200]}")
        return None


def _pmap(fn, items):
    with ThreadPoolExecutor(max_workers=max(1, len(items))) as ex:
        return list(ex.map(fn, items))


def _finalize(out, champ, verdict=None):
    if verdict is not None:
        (out / "final.verdict.md").write_text(verdict)
    (out / "WINNER.abc").write_text(champ[1])
    try:
        core.to_lilypond(out / "WINNER.abc", out / "WINNER.ly")
    except Exception as e:  # noqa: BLE001
        core.log(f"(lilypond skipped: {e})")
    core.log(f"WINNER: {champ[0]} -> {champ[2]}")
    print(f"\nWINNER: {champ[0]}  ->  {champ[2]}\nEverything in {out}/ (all rounds kept).")


def cmd_bakeoff(a):
    d = _load(a.spec)
    out = _outdir(a, d)
    prompt = _prompt(d)
    _init(out, prompt)
    core.log(f"bakeoff: {len(d['composers'])} composers (parallel) -> {out}")
    results = _pmap(lambda c: _compose_render(c, prompt, out / c["name"]), d["composers"])
    entries = [(r[0], r[2]) for r in results if r]
    if not entries:
        print("no entries rendered")
        return
    verdict = core.judge(entries, prompt, model=_jmodel(d))
    (out / "verdict.md").write_text(verdict)
    print("\n=== VERDICT ===\n" + verdict)
    print(f"\nEverything in {out}/ — listen to the .wav files; you are the final arbiter.")


def cmd_parallel(a):
    """Strategy A — siloed self-iteration; the three loops run concurrently."""
    d = _load(a.spec)
    out = _outdir(a, d)
    base = _prompt(d)
    jmodel = _jmodel(d)
    n = a.n
    _init(out, base)

    def selfloop(comp):
        label = comp["name"]
        abc = critique = wav = None
        for i in range(1, n + 1):
            p = base if abc is None else (
                f"{base}\n\nYour previous attempt (ABC):\n{abc}\n\n"
                f"A listener, judging by ear, said:\n{critique}\n\n"
                "Revise to address it. Output ONLY valid ABC.")
            try:
                raw = core.compose(comp["provider"], comp["model"], p)
                (out / f"{label}-r{i}.raw.md").write_text(raw or "")
                abc = core.extract_abc(raw)
                _, wav = core.render(abc, out / f"{label}-r{i}")
                critique = core.judge([(f"{label}-r{i}", wav)], base, model=jmodel)
                (out / f"{label}-r{i}.critique.md").write_text(critique)
            except Exception as e:  # noqa: BLE001
                core.log(f"FAILED {label} r{i}: {type(e).__name__}: {str(e)[:200]}")
                break
        return (label, abc, wav) if abc else None

    core.log(f"parallel (siloed): {len(d['composers'])} loops x {n} rounds, concurrent -> {out}")
    finals = [r for r in _pmap(selfloop, d["composers"]) if r]
    if not finals:
        print("no finals")
        return
    core.log("parallel: final by-ear comparison")
    verdict = core.judge([(l, w) for l, _, w in finals], base, model=jmodel)
    print("\n=== FINAL COMPARISON ===\n" + verdict)
    _finalize(out, next(e for e in finals if e[0] == core.parse_winner(verdict, [l for l, _, _ in finals])), verdict)


def cmd_tournament(a):
    """Strategy B — champion relay; each round's challengers compose in parallel."""
    d = _load(a.spec)
    out = _outdir(a, d)
    base = _prompt(d)
    jmodel = _jmodel(d)
    _init(out, base)
    champion = None
    critique = ""
    for rnd in range(1, a.n + 1):
        core.log(f"=== tournament round {rnd}/{a.n} ===")
        if champion is None:
            prompt = base
        else:
            prompt = (f"{base}\n\nCurrent best (ABC):\n{champion[1]}\n\n"
                      f"A listener, judging by ear, said:\n{critique}\n\n"
                      "Produce a BETTER version (it may depart from this). Output ONLY valid ABC.")
        results = _pmap(lambda c: _compose_render(c, prompt, out / f"r{rnd}-{c['name']}"), d["composers"])
        entries = [r for r in results if r]
        if champion is not None:
            entries.append(("champion", champion[1], champion[2]))
        if not entries:
            print("no entries; stopping")
            break
        critique = core.judge([(l, w) for l, _, w in entries], base, model=jmodel)
        (out / f"round{rnd}.verdict.md").write_text(critique)
        winner = core.parse_winner(critique, [l for l, _, _ in entries])
        champion = next(e for e in entries if e[0] == winner)
        core.log(f"round {rnd} champion: {winner}")
    if champion:
        _finalize(out, champion)


def cmd_iterate(a):
    d = _load(a.spec)
    a.cmd = "iterate"
    out = _outdir(a, d)
    comp = next(c for c in d["composers"] if c["name"] == a.model)
    base = _prompt(d)
    jmodel = _jmodel(d)
    _init(out, base)
    abc = critique = None
    for i in range(1, a.n + 1):
        core.log(f"--- iterate round {i}/{a.n}: {comp['name']} ---")
        p = base if abc is None else (
            f"{base}\n\nYour previous attempt (ABC):\n{abc}\n\n"
            f"A listener said:\n{critique}\n\nRevise. Output ONLY valid ABC.")
        raw = core.compose(comp["provider"], comp["model"], p)
        (out / f"round{i}.raw.md").write_text(raw or "")
        abc = core.extract_abc(raw)
        _, wav = core.render(abc, out / f"round{i}")
        critique = core.judge([(f"round{i}", wav)], base, model=jmodel)
        (out / f"round{i}.critique.md").write_text(critique)
        print(f"round {i}: {wav}\n{critique}\n")
    print(f"Everything in {out}/")


def main():
    p = argparse.ArgumentParser(prog="goldberg")
    sub = p.add_subparsers(dest="cmd", required=True)

    for name, fn, extra in [
        ("bakeoff", cmd_bakeoff, False),
        ("parallel", cmd_parallel, True),
        ("tournament", cmd_tournament, True),
    ]:
        s = sub.add_parser(name)
        s.add_argument("--spec", default="aria.yaml")
        s.add_argument("--out")
        if extra:
            s.add_argument("-n", type=int, default=3, help="rounds")
        s.set_defaults(fn=fn)

    it = sub.add_parser("iterate")
    it.add_argument("--spec", default="aria.yaml")
    it.add_argument("--model", required=True)
    it.add_argument("-n", type=int, default=3)
    it.add_argument("--out")
    it.set_defaults(fn=cmd_iterate)

    a = p.parse_args()
    a.fn(a)
