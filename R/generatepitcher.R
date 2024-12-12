#' Generate Pitcher Performance Report
#'
#' This function fetches Statcast data for a given pitcher or list of pitchers, calculates key performance metrics,
#' and generates a strike zone heatmap displaying the most frequently used pitch type and whiff rate for each zone.
#'
#' @param player_ids A vector of numeric player IDs representing the pitchers to analyze.
#' @param start_date A character string representing the start date of the data range in the format "YYYY-MM-DD".
#' @param end_date A character string representing the end date of the data range in the format "YYYY-MM-DD".
#'
#' @return A `ggplot` object showing the strike zone heatmap with the most frequently used pitch type and whiff rate for each zone.
#' Additionally, tables summarizing general pitching statistics and pitch-specific statistics are displayed in the viewer.
#'
#' @details
#' The function performs the following steps:
#' - **Fetches player names** using the MLB Stats API.
#' - **Fetches Statcast data** from Baseball Savant for the specified pitcher(s) and date range.
#' - **Calculates general statistics** such as walk rate and strikeout rate.
#' - **Generates pitch-specific statistics** such as the average velocity for each pitch type.
#' - **Creates a strike zone heatmap** visualizing the most frequently used pitch type and the whiff rate in each strike zone location.
#'
#' The results include a strike zone heatmap and summary tables for general and pitch-specific statistics.
#'
#' @note This function requires an active internet connection to fetch data from the MLB Stats API and Baseball Savant.
#'
#' @examples
#' # Example usage:
#' pitcher_ids <- c(605483)  # Gerrit Cole
#' start_date <- "2024-01-01"
#' end_date <- "2024-12-31"
#' generate_pitcher(player_ids = pitcher_ids, start_date = start_date, end_date = end_date)
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
generate_pitcher <- function(player_ids, start_date, end_date) {
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
  data <- fetch_statcast_data(player_ids, start_date, end_date, "pitcher")
  if (is.null(data) || nrow(data) == 0) {
    cat("No Statcast data available for this pitcher.\n")
    return(NULL)
  }

  # Get player name
  player_name <- player_mapping[player_id == data$player_id[1], player_name]
  if (is.na(player_name)) player_name <- "Unknown Pitcher"

  ## === GENERAL STATS ===
  total_plate_appearances <- sum(data$events %in% c(
    "single", "double", "triple", "home_run", "strikeout", "walk", "hit_by_pitch",
    "field_out", "groundout", "flyout", "lineout", "sac_fly", "sac_bunt", "force_out"
  ))

  walk_rate <- round(sum(data$events == "walk") / total_plate_appearances, 3)
  strikeout_rate <- round(sum(data$events == "strikeout") / total_plate_appearances, 3)

  general_stats <- data.table(
    Metric = c("Walk Rate", "Strikeout Rate"),
    Value = c(walk_rate, strikeout_rate)
  )

  ## === PITCH-SPECIFIC STATS ===
  pitch_stats <- data[, .(
    avg_velocity = round(mean(release_speed, na.rm = TRUE), 1)
  ), by = pitch_type]
  pitch_stats <- pitch_stats[order(-avg_velocity)]

  ## === STRIKE ZONE PLOT ===
  # Define zone mapping
  zone_mapping <- data.table(
    zone = 1:9,
    x = rep(1:3, each = 3),
    y = rep(3:1, times = 3)
  )

  # Calculate performance metrics for each zone
  zone_performance <- data[, .(
    most_used_pitch = names(which.max(table(pitch_type))),
    whiff_rate = round(sum(description == "swinging_strike") / .N, 2)
  ), by = zone]

  combined_zone_data <- merge(zone_mapping, zone_performance, by = "zone", all.x = TRUE)
  combined_zone_data[is.na(most_used_pitch), most_used_pitch := "No Data"]

  # Create strike zone plot
  combined_plot <- ggplot(combined_zone_data, aes(x = x, y = y)) +
    geom_tile(aes(fill = whiff_rate), color = "black", alpha = 0.9) +
    geom_text(
      aes(label = paste0("Pitch: ", most_used_pitch)),
      size = 3.5, color = "white", fontface = "bold"
    ) +
    scale_fill_gradientn(
      colors = c("white", "lightblue", "blue", "darkblue"),
      name = "Whiff Rate",
      limits = c(0, 0.2)
    ) +
    labs(
      title = paste("Pitch Performance by Zone for", player_name),
      x = "Horizontal Location (Grid)",
      y = "Vertical Location (Grid)"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14)
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

  ## === DISPLAY STRIKE ZONE PLOT ===
  #print(combined_plot)

  # Return plot for further use
  return(combined_plot)
}

