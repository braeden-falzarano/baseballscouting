#' Generate Hitter Performance Report
#'
#' This function fetches Statcast data for a given hitter or list of hitters, calculates key performance metrics,
#' and generates a strike zone heatmap displaying the average exit velocity in the central zone.
#'
#' @param player_ids A vector of numeric player IDs representing the hitters to analyze.
#' @param start_date A character string representing the start date of the data range in the format "YYYY-MM-DD".
#' @param end_date A character string representing the end date of the data range in the format "YYYY-MM-DD".
#' @param game_types Character vector of Baseball Savant game type codes to include. Default
#'   `c("R", "PO")` is regular season and postseason; add `"S"` to include spring training.
#'
#' @return For a single ID, a `ggplot` object showing the central strike zone heatmap with average exit velocity
#' (or `NULL`, invisibly, when no data is found). For several IDs, a named list with one such plot per hitter,
#' named by player ID. Additionally, a table summarizing general hitting statistics is drawn for each hitter.
#'
#' @details
#' The function performs the following for each hitter separately:
#' - Fetches player names using the MLB Stats API.
#' - Fetches Statcast data from Baseball Savant for the hitter and date range. Only `start_date` and
#'   `end_date` restrict the dates; there is no season filter.
#' - Identifies batted balls: plate appearances ending in a hit, an in-play out, a fielder's choice,
#'   an error or a sacrifice.
#' - Calculates key metrics such as average exit velocity, launch angle, hard-hit rate, strikeout rate
#'   and walk rate.
#' - Creates a strike zone heatmap focusing on the central strike zone.
#'
#' Definitions:
#' - Plate appearances are rows with a non-empty `events` value, excluding baserunning events and
#'   `truncated_pa`.
#' - Hard-hit rate = batted balls with exit velocity of 95 mph or more / batted balls with a recorded
#'   exit velocity.
#' - Strikeout rate = (`strikeout` + `strikeout_double_play`) / PA. Walk rate = (`walk` + `intent_walk`) / PA.
#'
#' The heatmap covers `plate_x` from -1 to 1 ft and `plate_z` from 1 to 3.5 ft in 0.5 ft cells,
#' drawn from the catcher's view (negative `plate_x` is the third-base side).
#'
#' @note This function requires an active internet connection to fetch data from the MLB Stats API and Baseball Savant.
#'
#' @examples
#' \donttest{
#' # Example usage:
#' hitter_ids <- c(668804)  # Bryan Reynolds
#' start_date <- "2024-01-01"
#' end_date <- "2024-12-31"
#' generate_hitter(player_ids = hitter_ids, start_date = start_date, end_date = end_date)
#' }
#'
#' @import data.table
#' @import ggplot2
#' @importFrom gridExtra grid.arrange tableGrob
#' @importFrom grid textGrob gpar
#'
#' @export
generate_hitter <- function(player_ids, start_date, end_date, game_types = c("R", "PO")) {
  # Fetch player names
  player_mapping <- fetch_player_names(player_ids)

  plots <- lapply(player_ids, function(id) {
    data <- tryCatch(
      fetch_statcast_data(id, start_date, end_date, "batter", game_types),
      error = function(e) {
        message(conditionMessage(e))
        NULL
      }
    )
    if (is.null(data)) {
      message("No Statcast data available for hitter ", id, ".")
      return(NULL)
    }
    hitter_report(data, player_name_or(player_mapping, id, "Unknown Hitter"))
  })

  if (length(player_ids) == 1) {
    if (is.null(plots[[1]])) return(invisible(NULL))
    return(plots[[1]])
  }
  stats::setNames(plots, as.character(player_ids))
}

#' Batted-ball rows: PAs ending in a hit or an in-play non-hit
#' @noRd
batted_balls <- function(data) {
  data <- as.data.table(data)
  pa <- is_plate_appearance(data$events)
  bb_type <- if ("bb_type" %in% names(data)) data$bb_type else rep(NA_character_, nrow(data))
  outcome <- map_pa_outcome(data$events, bb_type)
  data[pa & outcome %in% c(hit_outcomes, "flyout", "groundout")]
}

#' Hard-hit rate among batted balls with a recorded exit velocity
#' @noRd
hard_hit_rate <- function(launch_speed, threshold = 95) {
  tracked <- !is.na(launch_speed)
  if (!any(tracked)) return(NA_real_)
  sum(launch_speed[tracked] >= threshold) / sum(tracked)
}

#' Build the report for one hitter's data: draw the table, return the zone plot
#' @noRd
hitter_report <- function(data, player_name) {
  # Filter data to include only balls in play
  bip_data <- batted_balls(data)

  if (nrow(bip_data) == 0) {
    message("No balls in play data available for ", player_name, ".")
    return(invisible(NULL))
  }

  ## === GENERAL STATS ===
  pa <- pa_summary(data)
  general_stats <- data.table(
    Metric = c("Plate Appearances", "Avg Exit Velocity", "Avg Launch Angle", "Hard Hit Rate",
               "Strikeout Rate", "Walk Rate"),
    Value = c(pa$pa,
              round(mean(bip_data$launch_speed, na.rm = TRUE), 1),
              round(mean(bip_data$launch_angle, na.rm = TRUE), 1),
              round(hard_hit_rate(bip_data$launch_speed), 3),
              round(pa$k_rate, 3),
              round(pa$bb_rate, 3))
  )

  ## === STRIKE ZONE PLOT ===
  # Central strike zone: plate_x -1 to 1 ft, plate_z 1 to 3.5 ft, 0.5 ft cells
  zoomed_data <- copy(bip_data)
  zoomed_data[, grid_x := cut(plate_x, breaks = seq(-1, 1, by = 0.5))]
  zoomed_data[, grid_z := cut(plate_z, breaks = seq(1, 3.5, by = 0.5))]
  zoomed_data <- zoomed_data[!is.na(grid_x) & !is.na(grid_z) & !is.na(launch_speed)]

  # Calculate average exit velocity and count of batted balls for each grid cell
  grid_summary <- zoomed_data[, .(
    avg_exit_velocity = mean(launch_speed),
    num_pitches = .N
  ), by = .(grid_x, grid_z)]

  if (nrow(grid_summary) == 0) {
    message("Zoomed grid data is not available for plotting.")
    return(invisible(NULL))
  }

  # Generate the heatmap for exit velocity in the central strike zone
  zone_plot <- ggplot(grid_summary, aes(x = grid_x, y = grid_z, fill = avg_exit_velocity)) +
    geom_tile() +
    geom_text(aes(label = round(avg_exit_velocity, 1)), color = "black", size = 4) +
    scale_fill_viridis_c(name = "Avg Exit Velocity", option = "plasma") +
    scale_x_discrete(drop = FALSE) +
    scale_y_discrete(drop = FALSE) +
    labs(
      title = paste("Central Strike Zone Exit Velocity for", player_name),
      x = "Horizontal Position, ft (catcher's view; negative = 3B side)",
      y = "Vertical Position, ft"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray90", linetype = "dashed")
    )

  ## === DISPLAY GENERAL STATS AS A TABLE ===
  general_table <- tableGrob(general_stats, rows = NULL)

  # Arrange and display the table as a plot
  grid.arrange(
    general_table,
    top = textGrob(paste("Hitter Stats for", player_name), gp = gpar(fontsize = 16, fontface = "bold")),
    ncol = 1
  )

  # Return zone plot for further use
  return(zone_plot)
}
