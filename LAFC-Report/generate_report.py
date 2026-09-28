"""
Builds report.html from metrics.json, then renders it to a 3-page PDF via
wkhtmltopdf. Run analysis.py first to produce metrics.json.

Run: python3 generate_report.py
"""
import json
import subprocess

with open("metrics.json") as f:
    M = json.load(f)

BLUE = "#2a78d6"
BLUE_DARK = "#184f95"
RED = "#d03b3b"
GRAY = "#c3c2b7"
MUTED = "#898781"
INK = "#0b0b0b"
INK2 = "#52514e"
GRID = "#e1e0d9"
SURFACE = "#fcfcfb"
PAGE_BG = "#f9f9f7"
GOOD = "#0ca30c"

LAFC = M["season_comparison"]["lafc"]
SKC = M["season_comparison"]["skc"]
AVG = M["season_comparison"]["league_avg"]


def fmt(v, d=2):
    return f"{v:.{d}f}"


def pct(v):
    return f"{v*100:.0f}%"


def bar_row(label, value, max_value, color=BLUE, value_fmt=None, sub=None):
    w = max(2, min(100, value / max_value * 100))
    vtxt = value_fmt if value_fmt is not None else fmt(value)
    sub_html = f'<div class="bar-sub">{sub}</div>' if sub else ""
    return f"""
    <div class="bar-row">
      <div class="bar-label">{label}{sub_html}</div>
      <div class="bar-track"><div class="bar-fill" style="width:{w:.1f}%;background:{color}"></div></div>
      <div class="bar-value">{vtxt}</div>
    </div>"""


def lollipop_row(label, lafc_val, avg_val, max_value, unit="", higher_is_danger=True, skc_val=None, skc_label="SKC"):
    lafc_pos = max(1, min(99, lafc_val / max_value * 100))
    avg_pos = max(1, min(99, avg_val / max_value * 100))
    color = RED if higher_is_danger else BLUE
    skc_html = ""
    if skc_val is not None:
        skc_pos = max(1, min(99, skc_val / max_value * 100))
        skc_html = f'<div class="lolli-dot lolli-skc" style="left:{skc_pos:.1f}%" title="{skc_label}"></div>'
    return f"""
    <div class="lolli-row">
      <div class="lolli-label">{label}</div>
      <div class="lolli-track">
        <div class="lolli-dot lolli-avg" style="left:{avg_pos:.1f}%"></div>
        {skc_html}
        <div class="lolli-dot lolli-lafc" style="left:{lafc_pos:.1f}%;background:{color};border-color:{color}"></div>
      </div>
      <div class="lolli-values"><b style="color:{color}">{lafc_val:.2f}{unit}</b> <span class="muted">vs {avg_val:.2f}{unit} avg</span></div>
    </div>"""


# ---------------------------------------------------------------- threats bar
threat_max = max(t["npxgxa90"] for t in M["top_threats"])
threat_bars = ""
for t in M["top_threats"]:
    is_top = t["player_name"] in ("Denis Bouanga", "Heung-Min Son")
    color = BLUE if is_top else GRAY
    name = t["player_name"]
    pos = t["primary_position"]
    threat_bars += bar_row(
        f'{name} <span class="pos">{pos}</span>',
        t["npxgxa90"], threat_max, color=color, value_fmt=fmt(t["npxgxa90"], 2),
        sub=f'npxG {fmt(t["player_season_np_xg_90"])} + xA {fmt(t["player_season_xa_90"])} per 90'
    )

# ---------------------------------------------------------------- trend chart
trend = M["trend"]
n = len(trend)
W, H = 620, 260
pad_l, pad_r, pad_t, pad_b = 38, 16, 20, 32
plot_w = W - pad_l - pad_r
plot_h = H - pad_t - pad_b
all_vals = [t["np_xg"] for t in trend] + [t["np_xg_against"] for t in trend]
vmax = max(all_vals) * 1.12


def xy(i, v):
    x = pad_l + (i / (n - 1)) * plot_w if n > 1 else pad_l
    y = pad_t + plot_h - (v / vmax) * plot_h
    return x, y


pts_for = " ".join(f"{xy(i, t['np_xg'])[0]:.1f},{xy(i, t['np_xg'])[1]:.1f}" for i, t in enumerate(trend))
pts_against = " ".join(f"{xy(i, t['np_xg_against'])[0]:.1f},{xy(i, t['np_xg_against'])[1]:.1f}" for i, t in enumerate(trend))

grid_lines = ""
for frac in (0.25, 0.5, 0.75, 1.0):
    y = pad_t + plot_h - frac * plot_h
    grid_lines += f'<line x1="{pad_l}" y1="{y:.1f}" x2="{W-pad_r}" y2="{y:.1f}" stroke="{GRID}" stroke-width="1"/>'
    grid_lines += f'<text x="{pad_l-8}" y="{y+3:.1f}" font-size="10" fill="{MUTED}" text-anchor="end">{vmax*frac:.1f}</text>'

x_labels = ""
skc_marker = ""
for i, t in enumerate(trend):
    x, _ = xy(i, 0)
    x_labels += f'<text x="{x:.1f}" y="{H-8}" font-size="10" fill="{MUTED}" text-anchor="middle">{t["date_str"]}</text>'
    if t["is_skc"]:
        skc_marker = f'''
        <line x1="{x:.1f}" y1="{pad_t}" x2="{x:.1f}" y2="{H-pad_b}" stroke="{INK}" stroke-width="1" stroke-dasharray="2,2" opacity="0.35"/>
        <text x="{x:.1f}" y="{pad_t-6}" font-size="10" fill="{INK}" text-anchor="middle" font-weight="700">vs SKC</text>'''

dots_for = "".join(
    f'<circle cx="{xy(i,t["np_xg"])[0]:.1f}" cy="{xy(i,t["np_xg"])[1]:.1f}" r="{4.5 if t["is_skc"] else 3}" fill="{BLUE}"/>'
    for i, t in enumerate(trend)
)
dots_against = "".join(
    f'<circle cx="{xy(i,t["np_xg_against"])[0]:.1f}" cy="{xy(i,t["np_xg_against"])[1]:.1f}" r="{4.5 if t["is_skc"] else 3}" fill="{RED}"/>'
    for i, t in enumerate(trend)
)

trend_svg = f"""
<svg viewBox="0 0 {W} {H}" class="trend-svg">
  {grid_lines}
  {skc_marker}
  <polyline points="{pts_for}" fill="none" stroke="{BLUE}" stroke-width="2.5"/>
  <polyline points="{pts_against}" fill="none" stroke="{RED}" stroke-width="2.5"/>
  {dots_for}{dots_against}
  {x_labels}
  <text x="{pad_l}" y="{pad_t+12}" font-size="11" fill="{BLUE_DARK}" font-weight="700">● npxG created</text>
  <text x="{pad_l+130}" y="{pad_t+12}" font-size="11" fill="{RED}" font-weight="700">● npxG conceded</text>
</svg>"""

# ---------------------------------------------------------------- set piece bars
sp_max = max(SKC["team_season_sp_xg_conceded_pg"], AVG["team_season_sp_xg_conceded_pg"],
             LAFC["team_season_sp_xg_pg"], AVG["team_season_sp_xg_pg"]) * 1.15

set_piece_html = (
    bar_row("SKC — set-piece xG <b>conceded</b>/g", SKC["team_season_sp_xg_conceded_pg"], sp_max, color=RED, sub="season average")
    + bar_row("League average — conceded/g", AVG["team_season_sp_xg_conceded_pg"], sp_max, color=GRAY)
    + bar_row("LAFC — set-piece xG <b>created</b>/g", LAFC["team_season_sp_xg_pg"], sp_max, color=BLUE, sub="below league average")
    + bar_row("League average — created/g", AVG["team_season_sp_xg_pg"], sp_max, color=GRAY)
)

sp_taker_names = ", ".join(p["player_name"].split()[-1] if p["player_name"] != "Denis Bouanga" else "Bouanga" for p in M["set_piece_takers"])

# ---------------------------------------------------------------- build-up rows
buildup_rows = ""
for p in M["build_up_engine"][:2]:
    buildup_rows += f"""
    <div class="threat-card">
      <div class="threat-name">{p['player_name']}</div>
      <div class="threat-pos">{p['primary_position']}</div>
      <div class="threat-stats">
        <span><b>{p['player_season_deep_progressions_90']:.1f}</b> deep progressions/90</span>
        <span><b>{p['player_season_op_passes_90']:.0f}</b> open-play passes/90</span>
        <span><b>{p['player_season_passing_ratio']*100:.0f}%</b> pass completion</span>
      </div>
    </div>"""

# ---------------------------------------------------------------- individual threat cards (page 3)
danger_cards = ""
for t in M["top_threats"][:2]:
    danger_cards += f"""
    <div class="threat-card accent">
      <div class="threat-name">{t['player_name']}</div>
      <div class="threat-pos">{t['primary_position']}</div>
      <div class="threat-stats">
        <span><b>{t['player_season_np_xg_90']:.2f}</b> npxG/90</span>
        <span><b>{t['player_season_xa_90']:.2f}</b> xA/90</span>
        <span><b>{t['player_season_obv_90']:.2f}</b> OBV/90</span>
      </div>
    </div>"""

reverse_scorers = " · ".join(
    f"{s['player_name']} ({s['player_match_goals']}g, {s['player_match_np_xg']:.2f} npxG)"
    for s in M["reverse_fixture"]["scorers"]
)

gk = M["goalkeeper"][0]

HTML = f"""<!doctype html>
<html><head><meta charset="utf-8">
<style>
  @page {{ size: Letter; margin: 0.7in 0.75in; }}
  * {{ box-sizing: border-box; }}
  body {{
    font-family: -apple-system, "Segoe UI", Helvetica, Arial, sans-serif;
    color: {INK}; background: {PAGE_BG}; margin: 0; font-size: 12pt; line-height: 1.5;
  }}
  .page {{ page-break-after: always; padding: 2px 2px; min-height: 9.3in; display: flex; flex-direction: column; }}
  .page:last-child {{ page-break-after: auto; }}
  h1 {{ font-size: 25pt; margin: 0 0 4px 0; letter-spacing: -0.3px; }}
  h2 {{ font-size: 16pt; margin: 0 0 12px 0; color: {INK}; border-bottom: 2px solid {INK}; padding-bottom: 6px; }}
  .subtitle {{ color: {INK2}; font-size: 12.5pt; margin-bottom: 18px; }}
  .muted {{ color: {MUTED}; }}
  .banner {{
    background: {INK}; color: white; padding: 22px 26px; border-radius: 8px; margin-bottom: 20px;
  }}
  .banner h1 {{ color: white; }}
  .banner .subtitle {{ color: #d9d8d2; margin-bottom: 0; }}
  .tile-row {{ display: flex; gap: 14px; margin-bottom: 20px; }}
  .tile {{
    flex: 1; background: {SURFACE}; border: 1px solid {GRID}; border-radius: 8px; padding: 16px 18px;
  }}
  .tile .tile-label {{ font-size: 9.5pt; text-transform: uppercase; letter-spacing: 0.5px; color: {MUTED}; margin-bottom: 6px; }}
  .tile .tile-value {{ font-size: 19pt; font-weight: 700; }}
  .tile .tile-sub {{ font-size: 10.5pt; color: {INK2}; margin-top: 5px; }}
  .summary-box {{
    background: {SURFACE}; border: 1px solid {GRID}; border-left: 5px solid {BLUE};
    border-radius: 6px; padding: 16px 20px; margin-bottom: 22px;
  }}
  .summary-box ul {{ margin: 0; padding-left: 18px; }}
  .summary-box li {{ margin-bottom: 11px; }}
  .summary-box li:last-child {{ margin-bottom: 0; }}
  .section {{ margin-bottom: 22px; }}
  .bar-row {{ display: grid; grid-template-columns: 240px 1fr 58px; align-items: center; gap: 10px; margin-bottom: 13px; }}
  .bar-label {{ font-size: 11pt; }}
  .bar-label .pos {{ color: {MUTED}; font-weight: 400; font-size: 9.5pt; }}
  .bar-sub {{ font-size: 9pt; color: {MUTED}; margin-top: 1px; }}
  .bar-track {{ background: {GRID}; border-radius: 4px; height: 13px; overflow: hidden; }}
  .bar-fill {{ height: 100%; border-radius: 4px; }}
  .bar-value {{ font-size: 10.5pt; font-weight: 700; text-align: right; }}
  .lolli-row {{ display: grid; grid-template-columns: 190px 1fr 170px; align-items: center; gap: 10px; margin-bottom: 22px; }}
  .lolli-label {{ font-size: 10.8pt; }}
  .lolli-track {{ position: relative; height: 8px; background: {GRID}; border-radius: 4px; margin: 9px 0; }}
  .lolli-dot {{ position: absolute; top: 50%; width: 14px; height: 14px; border-radius: 50%; transform: translate(-50%,-50%); border: 3px solid; }}
  .lolli-avg {{ background: {SURFACE}; border-color: {MUTED}; }}
  .lolli-skc {{ background: {SURFACE}; border-color: {INK2}; }}
  .lolli-lafc {{ }}
  .lolli-values {{ font-size: 10.3pt; text-align: right; }}
  .two-col {{ display: flex; gap: 22px; }}
  .two-col > div {{ flex: 1; }}
  .threat-card {{ background: {SURFACE}; border: 1px solid {GRID}; border-radius: 8px; padding: 14px 16px; margin-bottom: 13px; }}
  .threat-card.accent {{ border-left: 5px solid {BLUE}; }}
  .threat-name {{ font-weight: 700; font-size: 12pt; }}
  .threat-pos {{ font-size: 9.8pt; color: {MUTED}; margin-bottom: 8px; }}
  .threat-stats span {{ font-size: 9.8pt; color: {INK2}; margin-right: 14px; }}
  .reco-list {{ margin: 0; padding-left: 20px; }}
  .reco-list li {{ margin-bottom: 13px; font-size: 11.3pt; }}
  .caption {{ font-size: 9.3pt; color: {MUTED}; margin-top: 8px; }}
  .trend-svg {{ width: 100%; height: auto; }}
  footer {{ font-size: 9pt; color: {MUTED}; border-top: 1px solid {GRID}; padding-top: 10px; margin-top: auto; }}
  table.mini {{ width: 100%; border-collapse: collapse; font-size: 10.3pt; }}
  table.mini th {{ text-align: left; color: {MUTED}; font-weight: 600; font-size: 9pt; text-transform: uppercase; border-bottom: 1px solid {GRID}; padding: 5px 8px; }}
  table.mini td {{ padding: 6px 8px; border-bottom: 1px solid {GRID}; }}
</style></head>
<body>

<!-- ============================= PAGE 1 ============================= -->
<div class="page">
  <div class="banner">
    <h1>LAFC — Pre-Match Scouting Report</h1>
    <div class="subtitle">Prepared for the Sporting KC coaching staff &middot; Matchweek {M['fixture']['week']} &middot; {M['fixture']['date']} &middot; Sporting KC (home) vs LAFC (away)</div>
  </div>

  <div class="tile-row">
    <div class="tile">
      <div class="tile-label">Last Meeting</div>
      <div class="tile-value">{M['reverse_fixture']['score']}</div>
      <div class="tile-sub">{M['reverse_fixture']['date']} &middot; Week {M['reverse_fixture']['week']} &middot; {reverse_scorers}</div>
    </div>
    <div class="tile">
      <div class="tile-label">Season xG Differential / game</div>
      <div class="tile-value" style="color:{BLUE}">LAFC {LAFC['team_season_xgd_pg']:+.2f}</div>
      <div class="tile-sub">SKC {SKC['team_season_xgd_pg']:+.2f} &middot; League avg {AVG['team_season_xgd_pg']:+.2f}</div>
    </div>
    <div class="tile">
      <div class="tile-label">Counter-Attack Shots / game</div>
      <div class="tile-value" style="color:{RED}">LAFC {LAFC['team_season_counter_attacking_shots_pg']:.2f}</div>
      <div class="tile-sub">SKC concede {SKC['team_season_counter_attacking_shots_conceded_pg']:.2f}/g (league avg {AVG['team_season_counter_attacking_shots_conceded_pg']:.2f})</div>
    </div>
  </div>

  <div class="summary-box">
    <div style="font-weight:700; font-size:9.5pt; margin-bottom:5px;">Bottom line</div>
    <ul>
      <li><b>The reverse fixture is not a fluke to ignore.</b> LAFC won 4-0 in July; Denis Bouanga and Heung-Min Son combined for 3 of the 4 goals on a combined 0.77 non-penalty xG &mdash; consistent with their season-long profile as LAFC's clear top two threats (see below), not a one-off performance.</li>
      <li><b>The sharpest tactical mismatch is transition, not set pieces.</b> LAFC generate counter-attacking shots at roughly 1.5&times; the league rate (1.52/g vs. 1.03 league avg); we concede counter-attacking shots at roughly 1.3&times; the league rate (1.32/g). That pairing is the single biggest danger of this match.</li>
      <li><b>This is a more beatable LAFC than the one that beat us in July.</b> Their finishing has cooled over their last 5 matches (0.4 goals/game despite ~1.1 npxG/game created &mdash; underlying quality similar, output has dropped), and they concede more away from home (1.23 npxG against/game away vs. 1.06 at home).</li>
      <li><b>Set pieces remain a live secondary risk.</b> We concede well above the league average from set plays (0.41 xG/g conceded vs. 0.31 league avg); LAFC aren't a high-volume set-piece side, but Bouanga and Son both feature among LAFC's top set-piece delivery threats.</li>
    </ul>
  </div>

  <div class="section">
    <h2>LAFC's Attacking Threats — Season npxG + xA per 90</h2>
    {threat_bars}
    <div class="caption">Minimum 400 minutes played, MLS 2026 season to date. Bouanga and Son (highlighted) are clearly separated from LAFC's next tier of attacking output.</div>
  </div>
</div>

<!-- ============================= PAGE 2 ============================= -->
<div class="page">
  <h2>How LAFC Attack &amp; Defend</h2>
  <p class="subtitle" style="margin-top:-4px">Style profile vs. league average, MLS 2026 season to date</p>

  <div class="two-col">
    <div>
      {lollipop_row("Possession", LAFC['team_season_possession']*100, AVG['team_season_possession']*100, 65, unit="%", higher_is_danger=False)}
      {lollipop_row("Press intensity (PPDA, lower = more aggressive)", LAFC['team_season_ppda'], AVG['team_season_ppda'], 20, higher_is_danger=False)}
      {lollipop_row("Counter-attacking shots / game", LAFC['team_season_counter_attacking_shots_pg'], AVG['team_season_counter_attacking_shots_pg'], 2.2)}
      {lollipop_row("npxG created / game", LAFC['team_season_np_xg_pg'], AVG['team_season_np_xg_pg'], 1.8, higher_is_danger=False)}
    </div>
    <div>
      <div class="summary-box" style="margin-bottom:0;">
        <div style="font-weight:700; font-size:9.5pt; margin-bottom:5px;">Reading LAFC's identity</div>
        <p style="margin:0 0 6px 0;">LAFC press <b>less</b> intensely than the league average (PPDA {LAFC['team_season_ppda']:.1f} vs. {AVG['team_season_ppda']:.1f}) and build patiently through a settled double pivot &mdash; Marco Delgado and Mathieu Choiniere lead the squad in deep progressions and carry the bulk of open-play passing (both &gt;88% completion). But their clearest route to a shot is the counter: {LAFC['team_season_counter_attacking_shots_pg']:.2f} counter-attacking shots/game, roughly 50% above league average.</p>
        <p style="margin:0;">That is the mismatch that matters most on {M['fixture']['date']}: a team that manufactures shots in transition, facing a defense that gives up counter-attacking shots at an above-average rate.</p>
      </div>
    </div>
  </div>

  <div class="section" style="margin-top:6px;">
    <h2>Form Trend — Last 10 Matches (npxG Created vs. Conceded)</h2>
    {trend_svg}
    <div class="caption">Team totals built from player-match data (StatsBomb-style pack). The July 26 meeting with Sporting KC, marked above, was LAFC's best attacking output of this stretch &mdash; not their recent norm.</div>
  </div>

  <div class="tile-row" style="margin-top:6px;">
    <div class="tile">
      <div class="tile-label">LAFC — Away vs. Home (npxG against/g)</div>
      <div class="tile-value" style="color:{RED}">{M['lafc_form']['by_side']['np_xg_against']['away']:.2f} away</div>
      <div class="tile-sub">vs. {M['lafc_form']['by_side']['np_xg_against']['home']:.2f} at home &mdash; LAFC are leakier on the road, and this fixture is on the road for them.</div>
    </div>
    <div class="tile">
      <div class="tile-label">Sporting KC — Home vs. Away (npxG created/g)</div>
      <div class="tile-value" style="color:{BLUE}">{M['skc_form']['by_side']['np_xg']['home']:.2f} home</div>
      <div class="tile-sub">vs. {M['skc_form']['by_side']['np_xg']['away']:.2f} away &mdash; our attack has looked meaningfully better at home this season.</div>
    </div>
  </div>
</div>

<!-- ============================= PAGE 3 ============================= -->
<div class="page">
  <h2>Danger Zones &amp; Recommendations</h2>

  <div class="two-col">
    <div>
      <div style="font-weight:700; font-size:9pt; margin-bottom:6px;">Mark closely</div>
      {danger_cards}
      <div class="caption">Bouanga and Son are also two of LAFC's top three set-piece delivery threats (alongside Shaffelburg) &mdash; account for both in and out of possession.</div>
    </div>
    <div>
      <div style="font-weight:700; font-size:9pt; margin-bottom:6px;">Press triggers &mdash; disrupt the build-up here</div>
      {buildup_rows}
      <div class="caption">Delgado and Choiniere carry LAFC's progression from deep. Cutting their first-pass options is a more efficient use of pressing energy than chasing the front line.</div>
    </div>
  </div>

  <div class="section">
    <h2>Set-Piece Exposure</h2>
    {set_piece_html}
    <div class="caption">Sporting KC's set-piece defense is a standing weakness independent of opponent. LAFC aren't a high-volume set-piece threat by season average, but their primary set-piece deliverers ({sp_taker_names}) are also their two most productive attackers open play &mdash; worth tight marking regardless of delivery type. LAFC's goalkeeper, Hugo Lloris, has a solid but unremarkable season (+{gk['player_season_gsaa_90']:.2f} GSAA/90, {gk['player_season_save_ratio']*100:.0f}% save ratio) &mdash; no obvious individual exploit there.</div>
  </div>

  <div class="section">
    <h2>Recommendations for {M['fixture']['date']}</h2>
    <ol class="reco-list">
      <li><b>Prioritize transition defense over set-piece defense.</b> The counter-attack mismatch (LAFC's shot-creation strength meeting our shot-concession weakness) is the largest, most repeatable danger in this matchup &mdash; bigger than the set-piece risk, which is real but secondary.</li>
      <li><b>Assign a specific defensive plan to Bouanga and Son</b>, both in open play and on second balls from set pieces &mdash; they accounted for 75% of LAFC's goals in the reverse fixture and remain clearly separated from LAFC's other attackers in season output.</li>
      <li><b>Press selectively, not blindly.</b> LAFC's own press is below league intensity and their build-up funnels through Delgado and Choiniere &mdash; cutting their angles is a better use of energy than a high, indiscriminate press that can be bypassed directly to Bouanga or Son in transition.</li>
      <li><b>Lean on the home fixture.</b> Our attacking output is materially better at home than away this season, and LAFC concede more away from home than at home &mdash; the venue favors an assertive approach rather than a defensive one.</li>
      <li><b>Treat the reverse fixture as a form outlier, not a ceiling.</b> LAFC's finishing has cooled over their last five matches even as their underlying chance quality held steady &mdash; this LAFC side is not shooting the way it did on July 26.</li>
    </ol>
  </div>

  <footer>
    Data: MLS 2026 season pack (StatsBomb-style team/player/match data), provided for this task. Team-match xG figures were built from player-match rows (no direct match-level team file in the pack); full methodology and analysis code accompany this report as a separate file. npxG = non-penalty expected goals; xA = expected assists; OBV = on-ball value; PPDA = passes allowed per defensive action (pressing proxy, lower = more intense); GSAA = goals saved above average.
  </footer>
</div>

</body></html>
"""

with open("report.html", "w") as f:
    f.write(HTML)

subprocess.run(
    ["wkhtmltopdf", "--enable-local-file-access", "--print-media-type",
     "--margin-top", "0", "--margin-bottom", "0", "--margin-left", "0", "--margin-right", "0",
     "report.html", "report.pdf"],
    check=True,
)
print("done")
