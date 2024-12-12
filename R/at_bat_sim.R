#' Simulate At-Bat Outcomes Between a Pitcher and a Batter
#'
#' This function simulates the outcomes of at-bats between a specified pitcher and batter
#' based on historical data retrieved from Statcast. The function uses kernel density
#' estimation (KDE) to model pitch locations and adjusts probabilities based on
#' batter performance data.
#'
#' @param pitcher_id Numeric. The ID of the pitcher (Statcast pitcher ID).
#' @param batter_id Numeric. The ID of the batter (Statcast batter ID).
#' @param start_date Character. The start date of the data retrieval period in "YYYY-MM-DD" format.
#' @param end_date Character. The end date of the data retrieval period in "YYYY-MM-DD" format.
#' @param num_simulations Numeric. The number of at-bat simulations to perform. Default is 1000.
#' @param num_cores Numeric. The number of CPU cores to use for parallel processing. Default is 2.
#'
#' @return A data frame summarizing the simulated probabilities of each at-bat outcome.
#'
#' @details
#' The function retrieves historical data for the specified pitcher and batter from Statcast,
#' segments pitch locations into bins, and calculates success probabilities for the batter
#' by pitch location. Using KDE, pitch locations are sampled for each simulation, and outcomes
#' are determined based on both pitcher and batter data.
#'
#' The simulation process is parallelized for efficiency, utilizing the specified number of cores.
#'
#' @import baseballr
#' @import tidyr
#' @import dplyr
#' @import MASS
#' @import parallel
#'
#' @examples
#' # Simulate at-bats between Gerrit Cole and Mike Trout in the 2022 season
#' at_bat_sim(
#'   pitcher_id = 543037,  # Gerrit Cole
#'   batter_id = 545361,   # Mike Trout
#'   start_date = "2022-04-01",
#'   end_date = "2022-10-01",
#'   num_simulations = 10000,
#'   num_cores = 2
#' )
#'
#' @export

at_bat_sim <- function(pitcher_id, batter_id, start_date, end_date, num_simulations = 1000, num_cores = 2) {
  set.seed(42)

  # Ensure num_cores does not exceed the number of available cores
  max_cores <- parallel::detectCores(logical = TRUE)
  if (num_cores > max_cores) {
    warning(paste("Requested", num_cores, "cores, but only", max_cores, "are available. Adjusting to", max_cores, "cores."))
    num_cores <- max_cores
  }
  if (num_cores < 2) {
    warning("Parallelization is disabled; running in serial mode.")
    num_cores <- 1
  }

  # Step 1: Retrieve data for pitcher and batter
  pitcher_data <- statcast_search_pitchers(
    start_date = start_date,
    end_date = end_date,
    pitcherid = pitcher_id
  )

  batter_data <- statcast_search_batters(
    start_date = start_date,
    end_date = end_date,
    batterid = batter_id
  )

  # Prepare data: bin pitch locations
  x_bins <- seq(-2, 2, by = 0.5)
  z_bins <- seq(0, 5, by = 0.5)

  pitcher_data <- pitcher_data %>%
    mutate(plate_x_bin = cut(plate_x, breaks = x_bins, include.lowest = TRUE),
           plate_z_bin = cut(plate_z, breaks = z_bins, include.lowest = TRUE))

  batter_data <- batter_data %>%
    mutate(plate_x_bin = cut(plate_x, breaks = x_bins, include.lowest = TRUE),
           plate_z_bin = cut(plate_z, breaks = z_bins, include.lowest = TRUE))

  # Calculate batter success probabilities
  batter_success_by_location <- batter_data %>%
    group_by(pitch_type, plate_x_bin, plate_z_bin) %>%
    summarize(
      n_at_bats = n(),
      n_hits = sum(events %in% c('single', 'double', 'triple', 'home_run'), na.rm = TRUE),
      smoothed_hit_prob = (n_hits + 1) / (n_at_bats + 2),
      .groups = 'drop'
    )

  # KDE for pitch locations
  pitcher_kde <- kde2d(pitcher_data$plate_x, pitcher_data$plate_z, n = 50)

  sample_pitch_location <- function() {
    idx <- sample(1:length(pitcher_kde$z), size = 1, prob = pitcher_kde$z)
    grid <- expand.grid(plate_x = pitcher_kde$x, plate_z = pitcher_kde$y)
    return(grid[idx, ])
  }

  # Expanded outcomes and base probabilities
  outcome_probs <- data.frame(
    Outcome = c("strikeout", "walk", "single", "double", "triple", "home_run", "flyout", "groundout"),
    BaseRate = c(0.23, 0.08, 0.15, 0.05, 0.005, 0.03, 0.20, 0.24)
  )

  # Precompute adjusted probabilities
  adjusted_probs <- batter_success_by_location %>%
    rowwise() %>%  # Ensures operations are row-by-row
    mutate(
      adjusted_hit_prob = smoothed_hit_prob * outcome_probs$BaseRate[3] / sum(outcome_probs$BaseRate[3:5])
    ) %>%
    ungroup()

  simulate_at_bat <- function() {
    # Sample pitch type
    selected_pitch_type <- pitcher_data %>%
      group_by(pitch_type) %>%
      summarize(pitch_type_prob = n() / nrow(pitcher_data), .groups = 'drop') %>%
      slice_sample(n = 1, weight_by = pitch_type_prob) %>%
      pull(pitch_type)

    # Sample pitch location
    selected_location <- sample_pitch_location()

    # Match batter performance data
    batter_perf <- adjusted_probs %>%
      filter(pitch_type == selected_pitch_type,
             plate_x_bin == cut(selected_location$plate_x, breaks = x_bins, include.lowest = TRUE),
             plate_z_bin == cut(selected_location$plate_z, breaks = z_bins, include.lowest = TRUE))

    # Determine outcome probabilities
    if (nrow(batter_perf) == 0) {
      outcome <- "strikeout"
    } else {
      probs <- outcome_probs$BaseRate
      outcome <- sample(outcome_probs$Outcome, size = 1, prob = probs)
    }

    return(outcome)
  }

  if (num_cores > 1) {
    # Initialize parallel cluster
    cl <- parallel::makeCluster(num_cores)

    # Export required libraries, functions, and data to the cluster
    parallel::clusterEvalQ(cl, library(dplyr))
    parallel::clusterEvalQ(cl, library(MASS))
    parallel::clusterExport(
      cl,
      varlist = c("pitcher_data", "adjusted_probs", "outcome_probs",
                  "sample_pitch_location", "x_bins", "z_bins", "pitcher_kde"),
      envir = environment()
    )

    # Parallelized simulations
    results <- parallel::parLapply(cl, 1:num_simulations, function(i) simulate_at_bat())

    # Stop the cluster
    parallel::stopCluster(cl)
  } else {
    # Serial execution fallback
    results <- lapply(1:num_simulations, function(i) simulate_at_bat())
  }

  # Summarize results
  results <- unlist(results)
  outcome_counts <- table(results)
  probabilities <- outcome_counts / sum(outcome_counts)
  df <- data.frame(Outcome = names(probabilities), Probability = as.numeric(probabilities))
  print(df)
}
