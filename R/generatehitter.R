#' Generate Hitter Performance Report
#'
#' This function fetches Statcast data for a given hitter or list of hitters, calculates key performance metrics,
#' and generates a strike zone heatmap displaying the average exit velocity in the central zone.
#'
#' @param player_ids A vector of numeric player IDs representing the hitters to analyze.
#' @param start_date A character string representing the start date of the data range in the format "YYYY-MM-DD".
#' @param end_date A character string representing the end date of the data range in the format "YYYY-MM-DD".
#'
#' @return A `ggplot` object showing the central strike zone heatmap with average exit velocity.
#' Additionally, tables summarizing general hitting statistics are displayed in the viewer.
#'
#' @details
#' The function performs the following:
#' - Fetches player names using the MLB Stats API.
#' - Fetches Statcast data from Baseball Savant for the specified hitter(s) and date range.
#' - Filters data to include only balls in play.
#' - Calculates key metrics such as average exit velocity, launch angle, hard-hit rate, and strikeout rate.
#' - Creates a strike zone heatmap focusing on the central strike zone.
#'
#' The results include a heatmap visualizing average exit velocity in different strike zone regions and
#' a table summarizing key performance metrics.
#'
#' @note This function requires an active internet connection to fetch data from the MLB Stats API and Baseball Savant.
#'
#' @examples
#' # Example usage:
#' hitter_ids <- c(668804)  # Bryan Reynolds
#' start_date <- "2024-01-01"
#' end_date <- "2024-12-31"
#' generate_hitter(player_ids = hitter_ids, start_date = start_date, end_date = end_date)
#'
#' @import data.table
#' @import ggplot2
#' @import httr
#' @import jsonlite
#' @importFrom gridExtra grid.arrange tableGrob
#' @importFrom grid textGrob gpar
#' @importFrom kableExtra kable
#'
#' @export
generate_hitter <- function(player_ids, start_date, end_date) {
  # Fetch player names
  fetch_player_names <- function(player_ids) {
    player_info <- lapply(player_ids, function(player_id) {
      tryCatch({
        url <- paste0("https://statsapi.mlb.com/api/v1/people/", player_id)
        response <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
        name <- response$people$fullName
        return(data.frame(player_id = player_id, player_name = name, stringsAsFactors = FALSE))
      }, error = function(e) {
        return(data.frame(player_id = player_id, player_name = NA, stringsAsFactors = FALSE))
      })
    })
    player_mapping <- rbindlist(player_info, fill = TRUE)
    return(player_mapping)
  }

  # Fetch Statcast data
  fetch_statcast_data <- function(player_ids, start_date, end_date, player_type) {
    player_data_list <- lapply(player_ids, function(player_id) {
      tryCatch({
        url <- paste0(
          "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
          "&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7CPO%7CS%7C",
          "&hfC&hfSea=2024%7C&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=",
          "&hfSA=&player_type=", player_type,
          "&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=",
          "&", player_type, "s_lookup%5B%5D=", player_id,
          "&game_date_gt=", start_date, "&game_date_lt=", end_date,
          "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0",
          "&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed",
          "&sort_order=desc&min_abs=0&type=details"
        )
        message(paste("Fetching data for player", player_id, "..."))
        data <- fread(url, fill = TRUE)
        if (ncol(data) < 1) stop("No valid data found.")
        data$player_id <- player_id
        return(data)
      }, error = function(e) {
        message(paste("Error fetching data for player", player_id, ":", e$message))
        return(NULL)
      })
    })
    combined_data <- rbindlist(player_data_list, fill = TRUE)
    return(combined_data)
  }

  # Fetch player names
  player_mapping <- fetch_player_names(player_ids)

  # Fetch Statcast data
  data <- fetch_statcast_data(player_ids, start_date, end_date, "batter")
  if (is.null(data) || nrow(data) == 0) {
    cat("No Statcast data available for this hitter.\n")
    return(NULL)
  }

  # Filter data to include only balls in play
  bip_data <- data[events %in% c(
    "single", "double", "triple", "home_run", "field_out",
    "groundout", "flyout", "lineout", "force_out", "sac_fly", "sac_bunt"
  )]

  if (nrow(bip_data) == 0) {
    cat("No balls in play data available for this player.\n")
    return()
  }

  # Get player name
  player_name <- player_mapping[player_id == data$player_id[1], player_name]
  if (is.na(player_name)) player_name <- "Unknown Hitter"

  ## === GENERAL STATS ===
  total_plate_appearances <- sum(data$events %in% c(
    "single", "double", "triple", "home_run", "strikeout", "walk", "hit_by_pitch",
    "field_out", "groundout", "flyout", "lineout", "sac_fly", "sac_bunt", "force_out"
  ))

  avg_exit_velocity <- round(mean(bip_data$launch_speed, na.rm = TRUE), 1)
  avg_launch_angle <- round(mean(bip_data$launch_angle, na.rm = TRUE), 1)
  hard_hit_rate <- round(sum(bip_data$launch_speed >= 95, na.rm = TRUE) / nrow(bip_data), 3)
  strikeout_rate <- round(sum(data$events == "strikeout") / total_plate_appearances, 3)

  general_stats <- data.table(
    Metric = c("Avg Exit Velocity", "Avg Launch Angle", "Hard Hit Rate", "Strikeout Rate"),
    Value = c(avg_exit_velocity, avg_launch_angle, hard_hit_rate, strikeout_rate)
  )

  ## === STRIKE ZONE PLOT ===
  # Define grid bins for plate position
  bip_data[, grid_x := cut(plate_x, breaks = seq(-2.5, 2.5, by = 0.5), include.lowest = TRUE)]
  bip_data[, grid_z := cut(plate_z, breaks = seq(0, 5, by = 0.5), include.lowest = TRUE)]

  # Filter data for the specific central strike zone range
  zoomed_data <- bip_data[
    grid_x %in% c("(-1,-0.5]", "(-0.5,0]", "(0,0.5]") &
      grid_z %in% c("(1,1.5]", "(1.5,2]", "(2,2.5]", "(2.5,3]", "(3,3.5]")
  ]

  # Calculate average exit velocity and count of pitches for each grid in the zoomed range
  grid_summary <- zoomed_data[, .(
    avg_exit_velocity = mean(launch_speed, na.rm = TRUE),
    num_pitches = .N
  ), by = .(grid_x, grid_z)]

  if (nrow(grid_summary) == 0) {
    cat("\nZoomed grid data is not available for plotting.\n")
    return()
  }

  # Generate the heatmap for exit velocity in the central strike zone
  zone_plot <- ggplot(grid_summary, aes(x = grid_x, y = grid_z, fill = avg_exit_velocity)) +
    geom_tile() +
    geom_text(aes(label = round(avg_exit_velocity, 1)), color = "black", size = 4) +
    scale_fill_viridis_c(name = "Avg Exit Velocity", option = "plasma") +
    labs(
      title = paste("Central Strike Zone Exit Velocity for", player_name),
      x = "Horizontal Position of the Strike Zone",
      y = "Vertical Position of the Strike Zone"
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

  ## === DISPLAY ZONE PLOT ===
  #print(zone_plot)

  # Return zone plot for further use
  return(zone_plot)
}

