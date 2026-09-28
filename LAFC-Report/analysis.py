"""
LAFC Pre-Match Scouting Report - Analysis Pipeline
Sporting KC First Team Data Analyst - Candidate Task

Data source: StatsBomb-style MLS 2026 data pack (sb_match_info.csv,
sb_match_players.csv, sb_season_players.csv, sb_season_teams.csv).

This script:
  1. Cleans match dates (mixed D/M/YY and D/M/YYYY formats in the source file).
  2. Builds team-match level aggregates (xG for/against per match) from
     player-match data, since the pack has no direct match-level team file.
  3. Computes the season, form, and venue splits used in the report.
  4. Identifies LAFC's individual threats, set-piece takers, build-up
     progressors, and goalkeeper profile.
  5. Dumps every number used in the report to metrics.json so the report
     template pulls figures programmatically rather than by hand-transcription.

Run: python3 analysis.py
"""
import json
import pandas as pd

pd.set_option("display.max_columns", None)
pd.set_option("display.width", 220)

TARGET_TEAM = "LAFC"
OPP_TEAM = "Sporting Kansas City"


def parse_date(s):
    parts = str(s).split("/")
    if len(parts) != 3:
        return pd.NaT
    d, m, y = parts
    y = int(y)
    if y < 100:
        y += 2000
    try:
        return pd.Timestamp(year=y, month=int(m), day=int(d))
    except Exception:
        return pd.NaT


def main():
    matches = pd.read_csv("sb_match_info.csv")
    matches["date"] = matches["match_date"].apply(parse_date)
    assert matches["date"].isna().sum() == 0, "date parse failures remain"

    mp = pd.read_csv("sb_match_players.csv")
    sp = pd.read_csv("sb_season_players.csv")
    st = pd.read_csv("sb_season_teams.csv")

    # ---- the upcoming fixture -------------------------------------------------
    upcoming = matches[
        (matches["home_team"] == OPP_TEAM)
        & (matches["away_team"] == TARGET_TEAM)
        & (matches["match_status"] == "scheduled")
    ].iloc[0]

    # ---- reverse fixture --------------------------------------------------
    reverse = matches[
        (matches["home_team"] == TARGET_TEAM)
        & (matches["away_team"] == OPP_TEAM)
        & (matches["match_status"] == "available")
    ].iloc[0]
    reverse_mid = int(reverse["match_id"])

    # ---- team-match level aggregates (built from player-match rows) -------
    team_match = mp.groupby(["match_id", "team_id", "team_name"]).agg(
        np_xg=("player_match_np_xg", "sum"),
        goals=("player_match_goals", "sum"),
        np_shots=("player_match_np_shots", "sum"),
        obv=("player_match_obv", "sum"),
    ).reset_index()

    mi = matches[["match_id", "date", "home_team", "away_team", "match_status", "match_week"]]
    tm = team_match.merge(mi, on="match_id")
    tm["side"] = tm.apply(lambda r: "home" if r["team_name"] == r["home_team"] else "away", axis=1)
    tm["opponent"] = tm.apply(lambda r: r["away_team"] if r["side"] == "home" else r["home_team"], axis=1)

    opp_xg = team_match[["match_id", "team_name", "np_xg"]].rename(
        columns={"team_name": "opponent", "np_xg": "np_xg_against"}
    )
    tm = tm.merge(opp_xg, on=["match_id", "opponent"])

    lafc_log = tm[(tm["team_name"] == TARGET_TEAM) & (tm["match_status"] == "available")].sort_values("date")
    skc_log = tm[(tm["team_name"] == OPP_TEAM) & (tm["match_status"] == "available")].sort_values("date")

    lafc_last5 = lafc_log.tail(5)[["np_xg", "np_xg_against", "goals"]].mean()
    lafc_season_form = lafc_log[["np_xg", "np_xg_against", "goals"]].mean()
    lafc_by_side = lafc_log.groupby("side")[["goals", "np_xg", "np_xg_against"]].mean()
    skc_by_side = skc_log.groupby("side")[["goals", "np_xg", "np_xg_against"]].mean()

    # trend series for the chart (last 10 available matches)
    trend = lafc_log.tail(10)[["date", "opponent", "np_xg", "np_xg_against", "side"]].copy()
    trend["date_str"] = trend["date"].dt.strftime("%-m/%-d")
    trend["is_skc"] = trend["opponent"] == OPP_TEAM

    # ---- season team comparison: LAFC vs SKC vs league avg ----------------
    key_cols = [
        "team_name", "team_season_matches", "team_season_gd_pg", "team_season_xgd_pg",
        "team_season_np_xg_pg", "team_season_np_xg_conceded_pg", "team_season_possession",
        "team_season_ppda", "team_season_directness", "team_season_np_shots_pg",
        "team_season_np_shots_conceded_pg", "team_season_sp_xg_pg", "team_season_sp_xg_conceded_pg",
        "team_season_counter_attacking_shots_pg", "team_season_counter_attacking_shots_conceded_pg",
        "team_season_obv_pg", "team_season_obv_conceded_pg",
    ]
    sub = st[st["team_name"].isin([TARGET_TEAM, OPP_TEAM])][key_cols].set_index("team_name")
    league_avg = st[key_cols[1:]].mean()

    # ---- LAFC individual threats -------------------------------------------
    lafc_players = sp[(sp["team_name"] == TARGET_TEAM) & (sp["player_season_minutes"] > 400)].copy()
    lafc_players["npxgxa90"] = lafc_players["player_season_np_xg_90"] + lafc_players["player_season_xa_90"]
    top_threats = lafc_players.sort_values("npxgxa90", ascending=False).head(6)[
        ["player_name", "primary_position", "player_season_minutes", "player_season_np_xg_90",
         "player_season_xa_90", "npxgxa90", "player_season_obv_90"]
    ]

    sp_takers = lafc_players.sort_values("player_season_sp_xa_90", ascending=False).head(3)[
        ["player_name", "player_season_sp_xa_90"]
    ]

    build_up = lafc_players.sort_values("player_season_deep_progressions_90", ascending=False).head(3)[
        ["player_name", "primary_position", "player_season_deep_progressions_90",
         "player_season_op_passes_90", "player_season_passing_ratio"]
    ]

    gk = lafc_players[lafc_players["primary_position"] == "Goalkeeper"][
        ["player_name", "player_season_minutes", "player_season_gsaa_90",
         "player_season_save_ratio", "player_season_np_xg_faced_90"]
    ]

    # ---- reverse-fixture box score ------------------------------------------
    game = mp[mp["match_id"] == reverse_mid]
    box = game[game["team_name"].isin([TARGET_TEAM, OPP_TEAM])].sort_values(
        ["team_name", "player_match_np_xg"], ascending=[True, False]
    )
    lafc_box_top = box[(box["team_name"] == TARGET_TEAM) & (box["player_match_goals"] > 0)][
        ["player_name", "player_match_goals", "player_match_np_xg", "player_match_xa"]
    ]

    # ---- assemble output -----------------------------------------------------
    out = {
        "fixture": {
            "date": str(upcoming["date"].date()),
            "venue": f"{OPP_TEAM} (home) vs {TARGET_TEAM} (away)",
            "week": int(upcoming["match_week"]),
        },
        "reverse_fixture": {
            "date": str(reverse["date"].date()),
            "score": f"{TARGET_TEAM} {int(reverse['home_score'])}-{int(reverse['away_score'])} {OPP_TEAM}",
            "week": int(reverse["match_week"]),
            "scorers": lafc_box_top.to_dict("records"),
        },
        "season_comparison": {
            "lafc": sub.loc[TARGET_TEAM].round(3).to_dict(),
            "skc": sub.loc[OPP_TEAM].round(3).to_dict(),
            "league_avg": league_avg.round(3).to_dict(),
        },
        "lafc_form": {
            "last5": lafc_last5.round(2).to_dict(),
            "season": lafc_season_form.round(2).to_dict(),
            "by_side": lafc_by_side.round(2).to_dict(),
        },
        "skc_form": {
            "by_side": skc_by_side.round(2).to_dict(),
        },
        "trend": trend[["date_str", "np_xg", "np_xg_against", "is_skc", "opponent"]].to_dict("records"),
        "top_threats": top_threats.round(3).to_dict("records"),
        "set_piece_takers": sp_takers.round(3).to_dict("records"),
        "build_up_engine": build_up.round(3).to_dict("records"),
        "goalkeeper": gk.round(3).to_dict("records"),
    }

    with open("metrics.json", "w") as f:
        json.dump(out, f, indent=2, default=str)

    print(json.dumps(out, indent=2, default=str))


if __name__ == "__main__":
    main()
