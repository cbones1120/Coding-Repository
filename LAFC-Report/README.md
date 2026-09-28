# LAFC Pre-Match Report — Analysis Code

Sporting KC First Team Data Analyst — Candidate Task

## What's here

- `analysis.py` — loads the provided data pack (`sb_match_info.csv`,
  `sb_match_players.csv`, `sb_season_players.csv`, `sb_season_teams.csv`),
  cleans the match dates (the source file mixes `D/M/YY` and `D/M/YYYY`
  formats), builds team-match-level xG for/against (the pack has no direct
  match-level team file, so this is aggregated from player-match rows),
  and computes every figure used in the report. Outputs `metrics.json`.
- `generate_report.py` — renders the 3-page PDF report from `metrics.json`
  (HTML/CSS -> PDF via wkhtmltopdf). Outputs `report.html` and `report.pdf`.
- `metrics.json` — every number used in the final report, in case you want
  to check a figure without re-running the pipeline.

## How to reproduce

```
python3 analysis.py            # place the 4 source CSVs in this folder first
python3 generate_report.py     # requires wkhtmltopdf on PATH
```

## Key methodology notes

- **Upcoming fixture identified from the data itself.** The pack's schedule
  shows Sporting KC hosting LAFC on 2026-09-13 (Matchweek 26) — the exact
  submission deadline — so the report is framed as opposition scouting for
  that specific match rather than a general LAFC profile.
- **Team-match xG for/against** is not a column in the pack; it's built by
  summing `player_match_np_xg` by `match_id`/`team_id`, then joining each
  team's match to its opponent's total in the same match to get "xG against."
  This powers the form-trend chart and the home/away splits.
- **Metric selection** favored a small number of well-understood, decision-
  relevant metrics (npxG, xA, PPDA, counter-attacking shots, set-piece xG)
  over exhaustive coverage of the ~200 columns in the season-team file, per
  the brief's instruction to prioritize clarity over volume.
- **All season-level figures** are means over MLS 2026 matches with
  `match_status == "available"` (i.e., played, not scheduled) as of the data
  pull date; "last 5" / "last 10" are the 5 or 10 most recent available
  matches by date for the team in question.
