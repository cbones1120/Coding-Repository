# -----------------------------
# Run / Pass Tendencies
# -----------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(nflfastR)
  library(gt)
  library(scales)
  library(glue)
  library(purrr)
  library(magick)
  library(pagedown)
  library(purrr)
  library(htmltools)
})

# ---- Helper: bin down & distance exactly like the chart ----
bin_down_distance <- function(down, ydstogo) {
  case_when(
    down == 1 & ydstogo == 10 ~ "1st & 10",
    down == 2 & ydstogo >= 1 & ydstogo <= 2  ~ "2nd & 1-2",
    down == 2 & ydstogo >= 3 & ydstogo <= 6  ~ "2nd & 3-6",
    down == 2 & ydstogo >= 7  ~ "2nd & 7+",
    down == 3 & ydstogo >= 1 & ydstogo <= 2  ~ "3rd & 1-2",
    down == 3 & ydstogo >= 3 & ydstogo <= 5  ~ "3rd & 3-5",
    down == 3 & ydstogo >= 6 & ydstogo <= 9 ~ "3rd & 6-9",
    down == 3 & ydstogo >= 10               ~ "3rd & 10+",
    down == 4 & ydstogo >= 1 & ydstogo <= 2  ~ "4th & 1-2",
    down == 4 & ydstogo >= 3                ~ "4th & 3+",
    TRUE ~ NA_character_
  )
}

row_order <- c(
  "1st & 10",
  "2nd & 1-2", "2nd & 3-6", "2nd & 7+",
  "3rd & 1-2", "3rd & 3-5", "3rd & 6-9", "3rd & 10+",
  "4th & 1-2", "4th & 3+"
)

# ---- Core function: build summary table for a given team ----
make_run_pass_summary <- function(pbp, team_abbr, season = 2025) {
  
  # Filters chosen to match the example:
  # - offensive plays only
  # - keep only run/pass (exclude QB kneels, spikes, etc. via play_type)
  # - remove penalty plays
  # - remove “backed up” (own 1-10) and “red zone” (+1-+20)
  #   using yardline_100 between 21 to 89
  # - remove non-standard down/distance by enforcing the bins above
  dat <- pbp %>%
    filter(season == !!season, posteam == !!team_abbr, season_type == "REG") %>%
    filter(play_type %in% c("run", "pass")) %>%
    filter(is.na(penalty) | penalty == 0) %>%
    # exclude red zone (1-20) and backed up (own 10 and in)
    filter(!is.na(yardline_100), dplyr::between(yardline_100, 21, 89)) %>%
    filter(!is.na(down), !is.na(ydstogo)) %>%
    mutate(
      bucket = bin_down_distance(down, ydstogo),
      is_run  = play_type == "run",
      is_pass = play_type == "pass"
    ) %>%
    filter(!is.na(bucket))
  
  # Summaries by bucket
  by_bucket <- dat %>%
    group_by(bucket) %>%
    summarise(
      run_plays  = sum(is_run),
      run_ypp    = if_else(run_plays > 0, mean(yards_gained[is_run]), NA_real_),
      pass_plays = sum(is_pass),
      pass_ypp   = if_else(pass_plays > 0, mean(yards_gained[is_pass]), NA_real_),
      all_plays  = n(),
      all_ypp    = mean(yards_gained),
      run_pct    = 100 * run_plays / all_plays,
      .groups = "drop"
    ) %>%
    mutate(bucket = factor(bucket, levels = row_order)) %>%
    arrange(bucket)
  
  # Total row
  total <- dat %>%
    summarise(
      bucket = "Total",
      run_plays  = sum(is_run),
      run_ypp    = mean(yards_gained[is_run]),
      pass_plays = sum(is_pass),
      pass_ypp   = mean(yards_gained[is_pass]),
      all_plays  = n(),
      all_ypp    = mean(yards_gained),
      run_pct    = 100 * run_plays / all_plays
    )
  
  bind_rows(by_bucket %>% mutate(bucket = as.character(bucket)), total)
}

# ---- Render function: gt table styled similar to the prompt ----
render_gt_table <- function(summary_df, team_title) {
  
  # Formatting like the example: run% as whole number, YPP 1 decimal, plays integer
  out <- summary_df %>%
    mutate(
      run_pct   = round(run_pct),
      run_ypp   = round(run_ypp, 1),
      pass_ypp  = round(pass_ypp, 1),
      all_ypp   = round(all_ypp, 1)
    ) %>%
    gt(rowname_col = "bucket") %>%
    tab_stubhead(label = "Down & Distance") %>%
    cols_move_to_start(columns = run_pct) %>%   # <-- Run % first
    tab_header(
      title = md(glue("**Offensive Run / Pass Summary**")),
      subtitle = md(glue("**{team_title} - 2025 Regular Season**"))
    ) %>%
    cols_label(
      run_pct    = "Run %",
      run_plays  = "Plays",
      run_ypp    = "Yards / Play",
      pass_plays = "Plays",
      pass_ypp   = "Yards / Play",
      all_plays  = "Plays",
      all_ypp    = "Yards / Play"
    ) %>%
    sub_missing(
      columns = c(run_ypp, pass_ypp, all_ypp),
      missing_text = "-"
    ) %>%
    tab_spanner(label = "Run Plays",  columns = c(run_plays, run_ypp)) %>%
    tab_spanner(label = "Pass Plays", columns = c(pass_plays, pass_ypp)) %>%
    tab_spanner(label = "All Plays",  columns = c(all_plays, all_ypp)) %>%
    tab_style(
      style = list(
        cell_fill(color = "#be0735"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_spanners(spanners = c("Run Plays", "Pass Plays", "All Plays"))
    ) %>%
    fmt_number(columns = c(run_ypp, pass_ypp, all_ypp), decimals = 1) %>%
    fmt(columns = run_pct, fns = ~paste0(round(.x), "%")) %>%  # <-- append %
    fmt_number(columns = c(run_plays, pass_plays, all_plays), decimals = 0) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "black", weight = px(1)),
      locations = cells_body(columns = c(run_plays, pass_plays, all_plays))
    ) %>%
    # column labels row: continue the line through the header
    tab_style(
      style = cell_borders(sides = "right", color = "black", weight = px(1)),
      locations = cells_column_labels(columns = c(run_plays, pass_plays, all_plays))
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#00308f"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = list(
        cells_title(groups = "title"),
        cells_title(groups = "subtitle")
      )
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#be0735"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_column_labels(everything())
    ) %>%
    tab_style(
      style = list(cell_text(weight = "bold")),
      locations = cells_body(rows = bucket == "Total")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#be0735"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_body(rows = bucket == "Total")
    ) %>%
    tab_style(
      style = list(
        cell_fill(color = "#be0735"),
        cell_text(color = "white", weight = "bold")
      ),
      locations = cells_stub(rows = bucket == "Total")
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "black", weight = px(1)),
      locations = cells_body(columns = c(run_pct, run_ypp, pass_ypp))
    ) %>%
    # header (column label) separators so the line continues upward
    tab_style(
      style = cell_borders(sides = "right", color = "black", weight = px(1)),
      locations = cells_column_labels(columns = c(run_pct, run_ypp, pass_ypp))
    ) %>%
    tab_style(
      style = cell_borders(sides = "right", color = "black", weight = px(1)),
      locations = cells_column_spanners(spanners = c("Run Plays", "Pass Plays"))
    ) %>%
    tab_options(
      table.font.size = 12,
      data_row.padding = px(2),
      heading.padding = px(6)
    ) %>%
    cols_align(
      align = "center",
      columns = c(run_pct, run_plays, run_ypp, pass_plays, pass_ypp, all_plays, all_ypp)
    ) %>%
    tab_source_note(
      source_note = md("Excludes red zone (+1 to +20), backed up (-1 to -10), plays with penalties, and non-standard down and distances.")
    )
  
  out
}

# ---- Main execution ----

# Season Input
season <- 2025

# Teams Input
teams <- c(
  WAS = "Washington Commanders",
  PHI = "Philadelphia Eagles",
  IND = "Indianapolis Colts"
)

pbp_2025 <- load_pbp(season)

# build the gt tables and convert to HTML strings
table_html <- imap(teams, function(team_name, team_abbr) {
  summ   <- make_run_pass_summary(pbp_2025, team_abbr = team_abbr, season = season)
  gt_tbl <- render_gt_table(summ, team_title = team_name)
  
  # get HTML for the table
  as_raw_html(gt_tbl)
})
# ---- make 3 tables + save PDFs (one per team) ----
out_dir <- "part2_out"
dir.create(out_dir, showWarnings = FALSE)

combined_html <- file.path(out_dir, glue("Part2_RunPassTendencies_{season}.html"))

pagebreak <- "<div style='page-break-after: always; break-after: page;'></div>"

html_doc <- paste0(
  "<html><head><meta charset='utf-8'>",
  # ensure printing keeps colors
  "<style>
     @page { margin: 0.5in; }
     body { -webkit-print-color-adjust: exact; print-color-adjust: exact; }
   </style>",
  "</head><body>",
  paste(table_html, collapse = pagebreak),
  "</body></html>"
)

writeLines(html_doc, combined_html)

pdf_out <- file.path(out_dir, glue("Part2_RunPassTendencies_{season}.pdf"))
pagedown::chrome_print(combined_html, output = pdf_out)

message("Wrote: ", pdf_out)

