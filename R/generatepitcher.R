#' Generate Pitcher Performance Report
#'
#' This function fetches Statcast data for a given pitcher or list of pitchers, calculates key performance metrics,
#' and generates a strike zone heatmap displaying the most frequently used pitch type and whiff rate for each zone.
#'
#' @param player_ids A vector of numeric player IDs representing the pitchers to analyze.
#' @param start_date A character string representing the start date of the data range in the format "YYYY-MM-DD".
#' @param end_date A character string representing the end date of the data range in the format "YYYY-MM-DD".
#' @param game_types Character vector of Baseball Savant game type codes to include. Default
#'   `c("R", "PO")` is regular season and postseason; add `"S"` to include spring training.
#'
#' @return For a single ID, a `ggplot` object showing the strike zone heatmap with the most frequently used
#' pitch type and whiff rate for each zone (or `NULL`, invisibly, when no data is found).
#' For several IDs, a named list with one such plot per pitcher, named by player ID.
#' Additionally, tables summarizing general pitching statistics and pitch-specific statistics are drawn
#' for each pitcher.
#'
#' @details
#' The function performs the following steps for each pitcher separately:
#' - **Fetches player names** using the MLB Stats API.
#' - **Fetches Statcast data** from Baseball Savant for the pitcher and date range. Only `start_date`
#'   and `end_date` restrict the dates; there is no season filter.
#' - **Calculates general statistics**: batters faced, walk rate, strikeout rate and whiff rate.
#' - **Generates pitch-specific statistics**: pitch count, usage, average velocity and whiff rate.
#' - **Creates a strike zone heatmap** of the most frequently used pitch type and the whiff rate in each of
#'   the nine Statcast strike zones.
#'
#' Definitions:
#' - Plate appearances (batters faced) are rows with a non-empty `events` value, excluding baserunning
#'   events such as `caught_stealing_*` and `pickoff_*` and `truncated_pa`.
#' - Strikeout rate = (`strikeout` + `strikeout_double_play`) / PA.
#' - Walk rate = (`walk` + `intent_walk`) / PA. Hit-by-pitch is not counted as a walk.
#' - Whiff rate = whiffs / swings. Whiffs are `swinging_strike`, `swinging_strike_blocked`, `foul_tip`
#'   and `missed_bunt`; swings are whiffs plus `foul`, `foul_bunt` and `hit_into_play`. Counting foul tips
#'   as whiffs matches Baseball Savant's Whiff%.
#'
#' The heatmap is drawn from the catcher's view: zone 1 is top-left (third-base side, high) and
#' zone 9 is bottom-right (first-base side, low).
#'
#' @note This function requires an active internet connection to fetch data from the MLB Stats API and Baseball Savant.
#'
#' @examples
#' \donttest{
#' # Example usage:
#' pitcher_ids <- c(543037)  # Gerrit Cole
#' start_date <- "2024-01-01"
#' end_date <- "2024-12-31"
#' generate_pitcher(player_ids = pitcher_ids, start_date = start_date, end_date = end_date)
#' }
#'
#' @import data.table
#' @import ggplot2
#' @importFrom gridExtra grid.arrange tableGrob
#' @importFrom grid textGrob gpar
#'
#' @export
generate_pitcher <- function(player_ids, start_date, end_date, game_types = c("R", "PO")) {
  # Fetch player names
  player_mapping <- fetch_player_names(player_ids)

  plots <- lapply(player_ids, function(id) {
    data <- tryCatch(
      fetch_statcast_data(id, start_date, end_date, "pitcher", game_types),
      error = function(e) {
        message(conditionMessage(e))
        NULL
      }
    )
    if (is.null(data)) {
      message("No Statcast data available for pitcher ", id, ".")
      return(NULL)
    }
    pitcher_report(data, player_name_or(player_mapping, id, "Unknown Pitcher"))
  })

  if (length(player_ids) == 1) {
    if (is.null(plots[[1]])) return(invisible(NULL))
    return(plots[[1]])
  }
  stats::setNames(plots, as.character(player_ids))
}

#' Statcast zones 1-9 laid out from the catcher's view
#'
#' Zones run left to right, top to bottom: 1 = top-left, 3 = top-right, 7 = bottom-left.
#' @noRd
zone_grid <- function() {
  data.table(
    zone = 1:9,
    x = rep(1:3, times = 3),
    y = rep(3:1, each = 3)
  )
}

#' Most-used pitch and whiff rate per swing for strike zones 1-9
#' @noRd
pitcher_zone_summary <- function(data) {
  data <- as.data.table(data)
  zone_performance <- data[!is.na(zone) & zone %in% 1:9, .(
    most_used_pitch = {
      pt <- pitch_type[!is.na(pitch_type) & nzchar(pitch_type)]
      if (length(pt) == 0) NA_character_ else names(which.max(table(pt)))
    },
    whiff_rate = round(whiff_rate_per_swing(description), 3)
  ), by = zone]
  combined <- merge(zone_grid(), zone_performance, by = "zone", all.x = TRUE)
  combined[is.na(most_used_pitch), most_used_pitch := "No Data"]
  combined[]
}

#' Per-pitch-type summary table
#' @noRd
pitcher_pitch_summary <- function(data) {
  data <- as.data.table(data)
  data <- data[!is.na(pitch_type) & nzchar(pitch_type)]
  pitch_stats <- data[, .(
    pitches = .N,
    avg_velocity = round(mean(release_speed, na.rm = TRUE), 1),
    whiff_rate = round(whiff_rate_per_swing(description), 3)
  ), by = pitch_type]
  pitch_stats[, usage := round(pitches / sum(pitches), 3)]
  setcolorder(pitch_stats, c("pitch_type", "pitches", "usage", "avg_velocity", "whiff_rate"))
  pitch_stats[order(-avg_velocity)]
}

#' Build the report for one pitcher's data: draw the tables, return the zone plot
#' @noRd
pitcher_report <- function(data, player_name) {
  ## === GENERAL STATS ===
  pa <- pa_summary(data)
  general_stats <- data.table(
    Metric = c("Batters Faced", "Walk Rate", "Strikeout Rate", "Whiff Rate (per swing)"),
    Value = c(pa$pa, round(pa$bb_rate, 3), round(pa$k_rate, 3),
              round(whiff_rate_per_swing(data$description), 3))
  )

  ## === PITCH-SPECIFIC STATS ===
  pitch_stats <- pitcher_pitch_summary(data)

  ## === STRIKE ZONE PLOT ===
  combined_zone_data <- pitcher_zone_summary(data)
  max_whiff <- suppressWarnings(max(combined_zone_data$whiff_rate, na.rm = TRUE))
  upper <- max(0.4, ceiling(max(max_whiff, 0) * 10) / 10)
  combined_zone_data[, label := paste0(
    "Zone ", zone, "\n", most_used_pitch, "\n",
    ifelse(is.na(whiff_rate), "No swings", paste0(round(100 * whiff_rate), "% whiff"))
  )]
  combined_zone_data[, text_color := ifelse(!is.na(whiff_rate) & whiff_rate > upper / 2,
                                            "white", "black")]

  combined_plot <- ggplot(combined_zone_data, aes(x = x, y = y)) +
    geom_tile(aes(fill = whiff_rate), color = "black", alpha = 0.9) +
    geom_text(aes(label = label, color = text_color), size = 3.5, fontface = "bold") +
    scale_color_identity() +
    scale_fill_gradientn(
      colors = c("white", "lightblue", "blue", "darkblue"),
      name = "Whiff Rate\n(per swing)",
      limits = c(0, upper),
      na.value = "grey85"
    ) +
    scale_x_continuous(breaks = 1:3, labels = c("3B side", "Middle", "1B side")) +
    scale_y_continuous(breaks = 1:3, labels = c("Low", "Middle", "High")) +
    labs(
      title = paste("Pitch Performance by Zone for", player_name),
      subtitle = "Statcast zones 1-9, catcher's view",
      x = "Horizontal Location (catcher's view)",
      y = "Vertical Location"
    ) +
    coord_fixed() +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      panel.grid = element_blank()
    )

  ## === DISPLAY GENERAL STATS AS A TABLE ===
  general_table <- tableGrob(general_stats, rows = NULL)
  pitch_table <- tableGrob(pitch_stats, rows = NULL)

  # Arrange and display the tables
  grid.arrange(
    general_table,
    pitch_table,
    top = textGrob(paste("Pitcher Stats for", player_name), gp = gpar(fontsize = 16, fontface = "bold")),
    ncol = 1
  )

  # Return plot for further use
  return(combined_plot)
}
