# =============================================================================
# Mariners 2027 DS Intern Problem Set - Problem 2
# Post-game review of our catcher's ABS challenge decisions (game 01103572,
# our affiliate at home).
#
# Each called ball our catcher received is judged with information available at
# the moment of the decision: where the pitch crossed relative to the batter's
# zone. A decision is scored against the league-wide overturn rate for challenged
# balls at that distance (from the full training season), not against hindsight.
#
# Usage
#   Rscript catcher_report.R                                  # data-train.csv, game 01103572
#   Rscript catcher_report.R path/to/data-train.csv 01103572
#
# Outputs: catcher-decisions.csv     (every challenge + every 75%+ odds ball left unchallenged)
#          league-overturn-rates.csv (overturn rate by distance from the zone edge)
#          catcher-zone.png          (strike-zone chart for the report)
# Requires: data.table, ggplot2, ggrepel
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(ggrepel)
})

TRAIN_PATH <- "data-train.csv"
GAME_ID    <- "01103572"
HALF_PLATE <- 17 / 24   # plate half-width in feet
BALL_R_IN  <- 1.45      # baseball radius in inches (ABS counts any part of the ball)
MISS_ODDS  <- 0.75     # an unchallenged ball at >= this league overturn rate is a missed chance
DIST_BINS  <- c(-Inf, -3, -2, -1.5, -1, -0.5, 0, 0.5, 1, 2, Inf)   # inches, + = centre inside zone

COL_WON    <- "#008300"   # green circle: challenged and won
COL_LOST   <- "#e34948"   # red X: challenged and lost
COL_MISSED <- "#e34948"   # big open red circle: should have challenged (shape tells it from the X)
INK        <- "#0b0b0b"
INK_2      <- "#52514e"


load_train <- function(path) {
  d <- fread(path, colClasses = "character", na.strings = c("", "NA"))
  # Restore leading zeros a spreadsheet stripped (formatC pads text with spaces)
  d[, game_id := paste0(strrep("0", pmax(8 - nchar(trimws(game_id)), 0)), trimws(game_id))]
  num <- c("balls", "strikes", "outs", "inning", "pre_score_home", "pre_score_away",
           "pa_of_game", "pitch_of_pa", "plate_x", "plate_z", "sz_top", "sz_bot",
           "is_challenge", "is_success")
  for (c in num) set(d, j = c, value = suppressWarnings(as.numeric(d[[c]])))
  d[, top := tolower(trimws(is_top_inning)) %in% c("true", "1")]
  d[, strike := pitch_result == "called_strike"]
  # Signed distance of the pitch centre from the zone edge, in inches (+ = inside)
  d[, inside_in := -12 * pmax(abs(plate_x) - HALF_PLATE, plate_z - sz_top, sz_bot - plate_z)]
  d[, dist_bin := cut(inside_in, DIST_BINS)]
  setorder(d, game_id, pa_of_game, pitch_of_pa)
  d[]
}

league_rates <- function(d) {
  # Defensive (catcher) challenges are only possible on called balls
  d[strike == FALSE & !is.na(dist_bin), .(called_balls = .N,
                       challenge_rate = mean(is_challenge),
                       challenges = sum(is_challenge),
                       overturn_rate = mean(is_success[is_challenge == 1])),
    keyby = dist_bin]
}

main <- function(train_path = TRAIN_PATH, game_id_arg = GAME_ID) {
  d <- load_train(train_path)
  rates <- league_rates(d)
  fwrite(rates, "league-overturn-rates.csv")

  g <- d[game_id == game_id_arg]
  if (!nrow(g)) stop("game ", game_id_arg, " not found in ", train_path)
  # Our club is the home team, so we field in the top half of each inning
  ours <- g[top == TRUE]
  catcher <- ours[, names(sort(table(catcher_id), decreasing = TRUE))[1]]
  ours <- ours[catcher_id == catcher]
  balls <- ours[strike == FALSE]
  balls[rates, league_odds := i.overturn_rate, on = "dist_bin"]

  balls[, decision := fcase(
    is_challenge == 1 & is_success == 1, "Challenged - won",
    is_challenge == 1,                   "Challenged - lost",
    league_odds >= MISS_ODDS,            "Not challenged (75%+ odds)",
    default = "Other called balls")]
  balls[, label := sprintf("T%d, %d-%d, %d out", as.integer(inning), as.integer(balls),
                           as.integer(strikes), as.integer(outs))]

  key <- balls[decision != "Other called balls",
               .(inning, outs, count = paste0(balls, "-", strikes),
                 score = paste0(pre_score_home, "-", pre_score_away, " (home-away)"),
                 inches_from_edge = round(inside_in, 2),
                 league_overturn_odds = round(league_odds, 2), decision)]
  setorder(key, decision, inning)
  fwrite(key, "catcher-decisions.csv")

  # League context: defensive challenges per team-game
  team_games <- uniqueN(d[, .(game_id, top)])
  lg <- d[challenge_source == "pitching_team"]
  cat(sprintf("Game %s - our catcher %s received %d pitches (%d called balls)\n",
              game_id_arg, catcher, nrow(ours), nrow(balls)))
  cat(sprintf("Challenges: %d used, %d won | league: %.2f per team-game, %.0f%% won\n",
              sum(balls$is_challenge), sum(balls$is_success, na.rm = TRUE),
              nrow(lg) / team_games, 100 * mean(lg$is_success)))
  cat(sprintf("High-odds balls (>= %.0f%% league overturn) left unchallenged: %d\n\n",
              100 * MISS_ODDS, sum(balls$decision == "Not challenged (75%+ odds)")))
  print(key, row.names = FALSE)

  # Strike-zone chart (catcher's view). Each batter's zone is rescaled to the
  # game's average zone so all pitches share one box.
  zb <- mean(ours$sz_bot); zt <- mean(ours$sz_top)
  balls[, z_plot := zb + (plate_z - sz_bot) / (sz_top - sz_bot) * (zt - zb)]
  lev <- c("Challenged - won", "Challenged - lost", "Not challenged (75%+ odds)", "Other called balls")
  balls[, decision := factor(decision, levels = lev)]
  hi <- balls[decision != "Other called balls"]
  r <- BALL_R_IN / 12

  # Only the challenged pitches and the ones that should have been challenged
  hi[, pt_size := fifelse(decision == "Not challenged (75%+ odds)", 6, 3.6)]
  p <- ggplot(hi, aes(plate_x, z_plot)) +
    annotate("rect", xmin = -HALF_PLATE - r, xmax = HALF_PLATE + r, ymin = zb - r, ymax = zt + r,
             fill = NA, colour = INK_2, linetype = "22", linewidth = 0.4) +
    annotate("rect", xmin = -HALF_PLATE, xmax = HALF_PLATE, ymin = zb, ymax = zt,
             fill = NA, colour = INK, linewidth = 0.7) +
    geom_point(aes(colour = decision, shape = decision, size = pt_size), stroke = 1.5) +
    scale_size_identity() +
    geom_label_repel(data = hi, aes(label = label), size = 2.7, colour = INK, label.size = 0,
                     fill = "#fcfcfbdd", box.padding = 0.6, point.padding = 0.4,
                     min.segment.length = 0, segment.colour = INK_2, segment.size = 0.3,
                     max.overlaps = Inf, seed = 7) +
    scale_colour_manual(values = c("Challenged - won" = COL_WON, "Challenged - lost" = COL_LOST,
                                   "Not challenged (75%+ odds)" = COL_MISSED), drop = FALSE,
                        breaks = lev[1:3], name = NULL) +
    scale_shape_manual(values = c("Challenged - won" = 16, "Challenged - lost" = 4,
                                  "Not challenged (75%+ odds)" = 1), drop = FALSE,
                       breaks = lev[1:3], name = NULL) +
    guides(colour = guide_legend(override.aes = list(size = c(3.6, 3.6, 6)))) +
    coord_fixed(xlim = c(-1.4, 1.4), ylim = c(zb - 0.7, zt + 0.6)) +
    labs(title = "Our catcher's ABS challenge decisions",
         subtitle = sprintf("Game %s - challenged pitches and missed chances, catcher's view", game_id_arg),
         caption = paste0("Solid box: strike zone. Dashed box: any part of the ball touches the zone ",
                          "(ball radius 1.45 in).\nLabels: inning (T = top), count, outs.\n",
                          "Open circle = called ball with 75%+ league overturn odds that was not challenged."),
         x = "Horizontal location (ft)", y = "Height (ft, scaled to average zone)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "top", panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = "#ecebe7", linewidth = 0.3),
          plot.title = element_text(face = "bold", colour = INK),
          plot.subtitle = element_text(colour = INK_2), axis.title = element_text(colour = INK_2),
          plot.caption = element_text(colour = INK_2, hjust = 0, size = 7.5),
          plot.background = element_rect(fill = "#fcfcfb", colour = NA))
  ggsave("catcher-zone.png", p, width = 6.5, height = 6, dpi = 200)
  cat("\nwrote catcher-decisions.csv, league-overturn-rates.csv, catcher-zone.png\n")
  invisible(key)
}

if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  do.call(main, as.list(args[seq_len(min(2, length(args)))]))
}
