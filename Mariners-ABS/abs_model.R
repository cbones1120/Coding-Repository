# =============================================================================
# Mariners 2027 DS Intern Problem Set - Problem 1
# Predict P(challenge), challenge source, and P(success | challenge) for called
# pitches.
#
# Pipeline
#   1. Load + clean (restore 8-digit game_ids, cast types, sort pitches in game order)
#   2. Pitch-level features: zone geometry, count leverage, game state, pitch traits,
#      and count-sequence flags. pitch_result is the pre-challenge call, so the count
#      on the next called pitch of the PA reveals most overturned calls. These flags
#      use later pitches in the same PA (fine for scoring the file, but they are not
#      "at the time of the decision" information).
#   3. Stage 1: LightGBM P(challenge) and P(success|challenge), game-grouped 5-fold CV
#   4. Challenge budget: teams get 2 challenges and keep them on a success. The test
#      file has no challenge labels, so we estimate each team's remaining budget from
#      model predictions (a small DP over 0/1/2 failed challenges). In train we build
#      the same feature from out-of-fold predictions so train and test match.
#   5. Stage 2: refit P(challenge) with the budget features.
#   6. Stage 3: add shrunken batter/catcher/home-club/umpire tendencies (OOF by game);
#      predict test.
#
# Usage
#   Rscript abs_model.R                                   # files in the working dir
#   Rscript abs_model.R path/to/data-train.csv path/to/data-test.csv
#
# Inputs : data-train.csv, data-test.csv (as provided)
# Outputs: data-test-predictions.csv  (test file with the three prediction columns filled)
#          train-scored.csv           (train + out-of-fold predictions, used for Problem 2)
# Requires: data.table, lightgbm
# =============================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(lightgbm)
})

TRAIN_PATH        <- "data-train.csv"
TEST_PATH         <- "data-test.csv"
OUT_PATH          <- "data-test-predictions.csv"
SCORED_TRAIN_PATH <- "train-scored.csv"

SEED       <- 7
N_FOLDS    <- 5
HALF_PLATE <- 17 / 24   # plate half-width in feet (8.5 in)
MAX_FAILS  <- 2         # a team is out of challenges after 2 failed ones

INT_COLS   <- c("balls", "strikes", "outs", "inning", "pre_score_home",
                "pre_score_away", "pa_of_game", "pitch_of_pa")
FLOAT_COLS <- c("velocity", "break_x", "break_z", "release_x", "release_z",
                "release_angle_x", "release_angle_z", "extension", "plate_x",
                "plate_z", "sz_top", "sz_bot")


# --------------------------------------------------------------------- load ----
read_raw <- function(path) {
  # Read every column as text so IDs keep their leading zeros (e.g. game 01103572)
  fread(path, colClasses = "character", na.strings = c("", "NA"))
}

load_data <- function(path) {
  df <- read_raw(path)
  # game_ids are 8 digits; pad in case a spreadsheet program ever stripped the zeros
  df[, game_id := formatC(game_id, width = 8, flag = "0")]
  for (c in INT_COLS)   set(df, j = c, value = as.integer(df[[c]]))
  for (c in FLOAT_COLS) set(df, j = c, value = suppressWarnings(as.numeric(df[[c]])))
  # accept True/False, TRUE/FALSE or 1/0
  df[, is_top_inning := as.integer(tolower(trimws(is_top_inning)) %in% c("true", "1"))]
  if ("is_challenge" %in% names(df)) {
    df[, is_challenge := as.integer(is_challenge)]
    df[, is_success := suppressWarnings(as.numeric(is_success))]
  }
  setorder(df, game_id, pa_of_game, pitch_of_pa)
  df[]
}


# ----------------------------------------------------------------- features ----
add_features <- function(d) {
  d[, is_strike_call := as.integer(pitch_result == "called_strike")]

  # Zone geometry (pitch center vs. batter's zone; + means outside the zone)
  d[, x_out     := abs(plate_x) - HALF_PLATE]
  d[, z_top_out := plate_z - sz_top]
  d[, z_bot_out := sz_bot - plate_z]
  d[, edge      := pmax(x_out, z_top_out, z_bot_out, na.rm = TRUE)]
  d[, z_norm    := (plate_z - sz_bot) / (sz_top - sz_bot)]
  # How wrong the call looks: + = strike called outside / ball called inside
  d[, wrong_margin := ifelse(is_strike_call == 1, edge, -edge)]
  # (abs(edge) was tested as a "how borderline" feature; ablation showed no gain
  #  for the tree model since edge + wrong_margin already capture it, so it was dropped)

  # Count leverage: does the call end the PA?
  d[, ends_pa_k  := as.integer(is_strike_call == 1 & strikes == 2)]
  d[, ends_pa_bb := as.integer(is_strike_call == 0 & balls == 3)]
  d[, ends_pa    := as.integer(ends_pa_k == 1 | ends_pa_bb == 1)]
  d[, count_code := balls * 3L + strikes]

  # Game state from the batting team's view
  d[, bat_score_diff := ifelse(is_top_inning == 1,
                               pre_score_away - pre_score_home,
                               pre_score_home - pre_score_away)]
  d[, abs_score_diff := abs(bat_score_diff)]
  d[, late_close     := as.integer(inning >= 7 & abs_score_diff <= 2)]

  d[, same_hand   := as.integer(pitcher_hand == batter_hand)]
  d[, batter_left := as.integer(batter_hand == "Left")]

  # Which side can challenge this call: strike -> hitting team, ball -> pitching team
  d[, bat_side     := ifelse(is_top_inning == 1, "away", "home")]
  d[, fld_side     := ifelse(is_top_inning == 1, "home", "away")]
  d[, chal_side    := ifelse(is_strike_call == 1, bat_side, fld_side)]
  d[, chal_is_home := as.integer(chal_side == "home")]

  # Count sequence: pitch_result is the call *before* any challenge, so the count on
  # the next called pitch of the PA shows whether the call was overturned.
  # (Requires rows sorted by game, PA, pitch - done in load_data.)
  d[, `:=`(seq_gap = shift(pitch_of_pa, type = "lead") - pitch_of_pa,
           seq_db  = shift(balls,       type = "lead") - balls,
           seq_ds  = shift(strikes,     type = "lead") - strikes),
    by = .(game_id, pa_of_game)]
  st  <- d$is_strike_call == 1
  adj <- d$seq_gap %in% 1
  flip_adj  <- adj & d$seq_db == st & d$seq_ds == !st
  stood_adj <- adj & d$seq_db == !st & d$seq_ds == st
  # "Strikeout" or "walk" but the PA kept going
  pa_cont   <- !is.na(d$seq_gap) & d$ends_pa == 1
  # With uncalled pitches in between (swings, fouls) the ball count can only rise, so
  # a called ball with no ball added was flipped; a called strike with none added stood
  flip_gap  <- d$seq_gap %in% 2:99 & !st & d$seq_db %in% 0
  stood_gap <- d$seq_gap %in% 2:99 &  st & d$seq_db %in% 0
  overturn  <- flip_adj | pa_cont | flip_gap
  d[, seq_overturn := as.integer(overturn)]
  d[, seq_stood    := as.integer((stood_adj | stood_gap) & !overturn)]
  d[]
}

BASE_FEATS <- c("is_strike_call", "plate_x", "plate_z", "sz_top", "sz_bot", "x_out",
                "z_top_out", "z_bot_out", "edge", "z_norm", "wrong_margin",
                "balls", "strikes", "count_code", "ends_pa_k", "ends_pa_bb", "ends_pa",
                "outs", "inning", "bat_score_diff", "abs_score_diff", "late_close",
                "pitch_of_pa", "pa_of_game", "same_hand", "batter_left", "chal_is_home",
                "velocity", "break_x", "break_z", "extension", "release_z",
                "seq_overturn", "seq_stood", "seq_gap", "seq_db", "seq_ds")
BUDGET_FEATS <- c("exp_fails", "p_has_chal", "exp_chals_used", "innings_left_frac")


# --------------------------------------------------------- challenge budget ----
budget_features <- function(df, p_chal, p_succ) {
  # For each pitch, estimate the challenging side's budget *before* this pitch.
  # Track a distribution over failed challenges (0, 1, 2+) per side per game.
  # Each earlier opportunity for that side fails with prob p_chal * (1 - p_succ).
  n <- nrow(df)
  exp_fails <- numeric(n); p_has <- numeric(n); exp_used <- numeric(n)
  games  <- df$game_id
  side   <- ifelse(df$chal_side == "home", 1L, 2L)   # 1 = home, 2 = away
  p_fail <- p_chal * (1 - p_succ)

  # state per side: P(0 fails), P(1 fail), P(2+ fails), expected challenges used
  d0 <- d1 <- d2 <- used <- numeric(2)
  cur_game <- ""
  for (i in seq_len(n)) {
    if (games[i] != cur_game) {               # new game: both teams start fresh
      d0[] <- 1; d1[] <- 0; d2[] <- 0; used[] <- 0
      cur_game <- games[i]
    }
    s <- side[i]
    exp_fails[i] <- d1[s] + 2 * d2[s]
    p_has[i]     <- 1 - d2[s]
    exp_used[i]  <- used[s]
    # update: a challenge can only happen if the side still has one
    pf <- p_fail[i]
    used[s] <- used[s] + p_chal[i] * (1 - d2[s])
    n0 <- d0[s] * (1 - pf)
    n1 <- d1[s] * (1 - pf) + d0[s] * pf
    n2 <- d2[s] + d1[s] * pf
    d0[s] <- n0; d1[s] <- n1; d2[s] <- n2
  }
  df[, exp_fails := exp_fails]
  df[, p_has_chal := p_has]
  df[, exp_chals_used := exp_used]
  df[, innings_left_frac := pmax(9 - inning, 0) / 9]
  invisible(df)
}

actual_budget <- function(df) {
  # Oracle version from true labels (train only) - used as an upper-bound ablation
  tmp <- df[, .(game_id, chal_side, inning,
                fail = as.integer(is_challenge == 1 & is_success %in% 0),
                used = is_challenge)]
  tmp[, prev_fail := cumsum(fail) - fail, by = .(game_id, chal_side)]
  tmp[, prev_used := cumsum(used) - used, by = .(game_id, chal_side)]
  data.table(exp_fails = tmp$prev_fail,
             p_has_chal = as.numeric(tmp$prev_fail < MAX_FAILS),
             exp_chals_used = tmp$prev_used,
             innings_left_frac = pmax(9 - tmp$inning, 0) / 9)
}


# ------------------------------------------------------- player tendencies ----
group_folds <- function(groups, k = N_FOLDS) {
  # Same idea as sklearn's GroupKFold: every game lands in exactly one fold, and
  # games are assigned largest-first to whichever fold is currently smallest.
  sizes <- sort(table(groups), decreasing = TRUE)
  fold_of <- integer(length(sizes)); names(fold_of) <- names(sizes)
  totals <- numeric(k)
  for (gname in names(sizes)) {
    f <- which.min(totals)
    fold_of[gname] <- f
    totals[f] <- totals[f] + sizes[[gname]]
  }
  unname(fold_of[as.character(groups)])
}

tendency_encode <- function(train, test, resid, expected, col, folds,
                            mask = NULL, k = 50) {
  # Shrunken 'actual minus expected' rate per ID, relative to the model's expectation.
  #   ratio = sum(actual - expected) / (sum(expected) + k)
  # 0 = behaves as the model expects; +0.3 = challenges 30% more than expected.
  # k shrinks small samples toward 0. Train values are out-of-fold by game so a
  # pitch never sees its own label.
  if (is.null(mask)) mask <- rep(TRUE, nrow(train))
  tmp <- data.table(id = train[[col]], r = resid, e = expected, fold = folds, use = mask)
  ratio_table <- function(rows) {
    s <- tmp[rows, .(r = sum(r), e = sum(e)), by = id]
    s[, ratio := r / (e + k)]
    s
  }
  tr_out <- numeric(nrow(train))
  for (f in seq_len(N_FOLDS)) {
    s <- ratio_table(tmp$fold != f & tmp$use)
    va <- which(tmp$fold == f)
    v <- s$ratio[match(tmp$id[va], s$id)]
    tr_out[va] <- ifelse(is.na(v), 0, v)
  }
  s <- ratio_table(tmp$use)
  v <- s$ratio[match(test[[col]], s$id)]
  list(train = tr_out, test = ifelse(is.na(v), 0, v))
}

add_id_keys <- function(df) {
  # The person most likely driving the decision: batter on strike calls,
  # catcher on ball calls (defensive challenges)
  df[, chal_person := ifelse(is_strike_call == 1, paste0("B_", batter_id),
                                                  paste0("C_", catcher_id))]
  # Venue identifies the home club, so this captures the home team's tendency
  df[, home_chal_venue := ifelse(chal_is_home == 1, venue_id, "away")]
  invisible(df)
}

TEND_FEATS      <- c("te_person", "te_venue", "te_ump")
SUCC_TEND_FEATS <- c("te_person_succ", "te_ump_succ")


# ------------------------------------------------------------------- models ----
CHAL_PARAMS <- list(objective = "binary", learning_rate = 0.03, num_leaves = 31,
                    min_data_in_leaf = 200, feature_fraction = 0.8,
                    bagging_fraction = 0.8, bagging_freq = 1, lambda_l2 = 5.0,
                    verbose = -1, seed = SEED)
SUCC_PARAMS <- list(objective = "binary", learning_rate = 0.02, num_leaves = 15,
                    min_data_in_leaf = 50, feature_fraction = 0.8,
                    bagging_fraction = 0.8, bagging_freq = 1, lambda_l2 = 5.0,
                    verbose = -1, seed = SEED)

as_mat <- function(dt, feats) as.matrix(dt[, ..feats])

cv_fit <- function(X, y, folds, params, rounds = 3000) {
  # Game-grouped CV. Returns OOF predictions and the mean best iteration.
  oof <- numeric(length(y)); best <- integer(N_FOLDS)
  for (f in seq_len(N_FOLDS)) {
    tr <- folds != f; va <- folds == f
    dtr <- lgb.Dataset(X[tr, , drop = FALSE], label = y[tr])
    dva <- lgb.Dataset.create.valid(dtr, X[va, , drop = FALSE], label = y[va])
    m <- lgb.train(params, dtr, nrounds = rounds, valids = list(valid = dva),
                   early_stopping_rounds = 100, verbose = -1)
    oof[va] <- predict(m, X[va, , drop = FALSE], num_iteration = m$best_iter)
    best[f] <- m$best_iter
  }
  list(oof = oof, iters = as.integer(mean(best)))
}

fit_full <- function(X, y, params, rounds) {
  lgb.train(params, lgb.Dataset(X, label = y), nrounds = rounds, verbose = -1)
}

log_loss <- function(y, p) {
  p <- pmin(pmax(p, 1e-15), 1 - 1e-15)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

auc <- function(y, p) {
  # Mann-Whitney form: share of (positive, negative) pairs ranked correctly
  # (as.numeric: n1 * n0 is ~2.5 billion here, past R's integer limit)
  r <- rank(p); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

report <- function(name, y, p) {
  cat(sprintf("  %-38s logloss %.5f  (base-rate %.5f)  AUC %.4f\n",
              name, log_loss(y, p), log_loss(y, rep(mean(y), length(y))), auc(y, p)))
}


# --------------------------------------------------------------------- main ----
main <- function(train_path = TRAIN_PATH, test_path = TEST_PATH) {
  set.seed(SEED)
  train <- add_features(load_data(train_path))
  test  <- add_features(load_data(test_path))
  g   <- train$game_id
  y_c <- train$is_challenge
  ch  <- y_c == 1
  y_s <- as.integer(train$is_success[ch])

  folds    <- group_folds(g)        # folds for all pitches
  folds_ch <- folds[ch]             # same game partition, challenged pitches only

  cat(sprintf("train %s pitches / %d games; test %s / %d\n",
              format(nrow(train), big.mark = ","), uniqueN(g),
              format(nrow(test), big.mark = ","), uniqueN(test$game_id)))
  cat(sprintf("challenge rate %.4f; success rate %.4f\n", mean(y_c), mean(y_s)))

  # --- Stage 1: no budget features
  cat("\nStage 1 (pitch + context features)\n")
  X_base <- as_mat(train, BASE_FEATS)
  c1 <- cv_fit(X_base, y_c, folds, CHAL_PARAMS)
  report("P(challenge)", y_c, c1$oof)
  s1 <- cv_fit(X_base[ch, ], y_s, folds_ch, SUCC_PARAMS)
  report("P(success | challenge)", y_s, s1$oof)

  # success predictions for every train pitch (OOF by game) for the budget DP
  oof_s_all <- numeric(nrow(train))
  for (f in seq_len(N_FOLDS)) {
    trm <- folds != f & ch
    m <- fit_full(X_base[trm, ], as.integer(train$is_success[trm]), SUCC_PARAMS, s1$iters)
    oof_s_all[folds == f] <- predict(m, X_base[folds == f, ])
  }

  # --- Oracle ablation: how much would knowing the true budget help?
  X_oracle <- cbind(X_base, as.matrix(actual_budget(train)))
  oracle <- cv_fit(X_oracle, y_c, folds, CHAL_PARAMS)
  report("P(challenge) w/ TRUE budget (oracle)", y_c, oracle$oof)

  # --- Stage 2: estimated budget features (two passes to let the estimate settle)
  cat("\nStage 2 (+ estimated challenge budget)\n")
  F2 <- c(BASE_FEATS, BUDGET_FEATS)
  p_c_est <- c1$oof
  for (pass in 1:2) {
    budget_features(train, p_c_est, oof_s_all)
    c2 <- cv_fit(as_mat(train, F2), y_c, folds, CHAL_PARAMS)
    p_c_est <- c2$oof
  }
  report("P(challenge) w/ estimated budget", y_c, c2$oof)

  # --- Stage 3: who is deciding (batter / catcher / home club tendencies)
  cat("\nStage 3 (+ player and club tendencies)\n")
  add_id_keys(train); add_id_keys(test)
  resid_c <- y_c - c2$oof
  tp <- tendency_encode(train, test, resid_c, c2$oof, "chal_person", folds)
  tv <- tendency_encode(train, test, resid_c, c2$oof, "home_chal_venue", folds)
  tu <- tendency_encode(train, test, resid_c, c2$oof, "umpire_id", folds)
  train[, `:=`(te_person = tp$train, te_venue = tv$train, te_ump = tu$train)]
  test[,  `:=`(te_person = tp$test,  te_venue = tv$test,  te_ump = tu$test)]
  F_C <- c(BASE_FEATS, BUDGET_FEATS, TEND_FEATS)
  c3 <- cv_fit(as_mat(train, F_C), y_c, folds, CHAL_PARAMS)
  report("P(challenge) final", y_c, c3$oof)

  # success: some challengers may simply be better judges of the zone
  resid_s <- numeric(nrow(train)); exp_s <- numeric(nrow(train))
  resid_s[ch] <- y_s - s1$oof; exp_s[ch] <- s1$oof
  ts <- tendency_encode(train, test, resid_s, exp_s, "chal_person", folds, mask = ch, k = 20)
  tus <- tendency_encode(train, test, resid_s, exp_s, "umpire_id", folds, mask = ch, k = 20)
  train[, `:=`(te_person_succ = ts$train, te_ump_succ = tus$train)]
  test[,  `:=`(te_person_succ = ts$test,  te_ump_succ = tus$test)]
  F_S <- c(BASE_FEATS, SUCC_TEND_FEATS)
  s3 <- cv_fit(as_mat(train, F_S)[ch, ], y_s, folds_ch, SUCC_PARAMS)
  report("P(success | challenge) + tendency", y_s, s3$oof)
  use_s3 <- log_loss(y_s, s3$oof) < log_loss(y_s, s1$oof)
  if (!use_s3) { F_S <- BASE_FEATS; s3 <- s1 }
  cat(sprintf("  -> success model uses %s features\n", if (use_s3) "tendency" else "base"))

  # Source rule check
  src_pred <- ifelse(train$is_strike_call == 1, "hitting_team", "pitching_team")
  cat(sprintf("\nSource rule accuracy on challenged pitches: %.4f\n",
              mean(src_pred[ch] == train$challenge_source[ch])))

  # --- Fit full models and predict test (same staged logic)
  cat("\nFitting full models and scoring test\n")
  m_c1 <- fit_full(X_base, y_c, CHAL_PARAMS, c1$iters)
  m_s1 <- fit_full(X_base[ch, ], y_s, SUCC_PARAMS, s1$iters)
  m_c2 <- fit_full(as_mat(train, F2), y_c, CHAL_PARAMS, c2$iters)
  m_c3 <- fit_full(as_mat(train, F_C), y_c, CHAL_PARAMS, c3$iters)
  m_s  <- fit_full(as_mat(train, F_S)[ch, ], y_s, SUCC_PARAMS, s3$iters)

  ps_test1 <- predict(m_s1, as_mat(test, BASE_FEATS))
  pc_test  <- predict(m_c1, as_mat(test, BASE_FEATS))
  for (pass in 1:2) {
    budget_features(test, pc_test, ps_test1)
    pc_test <- predict(m_c2, as_mat(test, F2))
  }
  pc_test <- predict(m_c3, as_mat(test, F_C))
  ps_test <- predict(m_s, as_mat(test, F_S))

  # Clip so a confident miss can't blow up log loss
  eps <- 1e-4
  out <- read_raw(test_path)   # original file, untouched apart from the prediction columns
  out[, game_id := formatC(game_id, width = 8, flag = "0")]
  idx <- match(out$play_id, test$play_id)
  out[, p_challenge := pmin(pmax(pc_test[idx], eps), 1 - eps)]
  out[, challenge_source := ifelse(test$is_strike_call[idx] == 1, "hitting_team", "pitching_team")]
  out[, p_success_g_challenge := pmin(pmax(ps_test[idx], eps), 1 - eps)]
  fwrite(out, OUT_PATH)
  cat(sprintf("wrote %s\n", OUT_PATH))
  cat(sprintf("mean p_challenge test %.4f (train rate %.4f); mean p_success %.4f\n",
              mean(out$p_challenge), mean(y_c), mean(out$p_success_g_challenge)))

  # Save out-of-fold predictions for diagnostics and Problem 2
  train[, oof_p_chal := c3$oof]          # final P(challenge)
  train[, oof_p_succ_all := oof_s_all]   # P(success) for every pitch (budget input)
  train[, oof_p_succ := NA_real_]        # final P(success), challenged pitches only
  train[ch, oof_p_succ := s3$oof]
  fwrite(train, SCORED_TRAIN_PATH)

  imp <- lgb.importance(m_c3)
  cat("\nTop features (gain) for P(challenge):\n")
  print(imp[order(-Gain)][1:12, .(Feature, Gain = round(Gain, 3))], row.names = FALSE)
}


# Run only when called as a script (not when source()'d to borrow functions)
if (sys.nframe() == 0) {
  args <- commandArgs(trailingOnly = TRUE)
  do.call(main, as.list(args[seq_len(min(2, length(args)))]))
}
