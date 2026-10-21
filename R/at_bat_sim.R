#' Simulate At-Bat Outcomes Between a Pitcher and a Batter
#'
#' This function simulates plate appearances between a specified pitcher and batter
#' using their Statcast data. Outcome probabilities combine the batter's, the pitcher's
#' and the league's outcome rates with the odds-ratio (generalized log5) method, and are
#' then adjusted for the batter's success against the sampled pitch type and location.
#'
#' @param pitcher_id Numeric. The ID of the pitcher (Statcast / MLBAM pitcher ID).
#' @param batter_id Numeric. The ID of the batter (Statcast / MLBAM batter ID).
#' @param start_date Character. The start date of the data retrieval period in "YYYY-MM-DD" format.
#' @param end_date Character. The end date of the data retrieval period in "YYYY-MM-DD" format.
#' @param num_simulations Numeric. The number of plate appearances to simulate. Default is 1000.
#' @param num_cores Numeric. The number of CPU cores to use for parallel processing. Default is 2.
#' @param seed Optional integer. When supplied, results are reproducible, including across
#'   different values of `num_cores`. Default `NULL` draws a seed from the current R session,
#'   so calling [set.seed()] beforehand also makes results reproducible.
#' @param league_rates Optional named numeric vector of league outcome rates with names
#'   `strikeout`, `walk`, `single`, `double`, `triple`, `home_run`, `flyout`, `groundout`.
#'   It is normalized to sum to 1. Default `NULL` uses an approximate MLB average (see Details).
#' @param prior_pa Numeric. Prior strength, in plate appearances, used to shrink each
#'   player's outcome rates toward `league_rates`. Default is 50.
#' @param location_prior_pa Numeric. Prior strength, in plate appearances, used to shrink
#'   the batter's hit rate in each pitch type and location bin toward his pitch-type rate,
#'   and his pitch-type rate toward his overall rate. Default is 20.
#' @param multiplier_range Numeric vector of length 2. The location/pitch-type multiplier
#'   applied to hit outcomes is clipped to this range. Default is `c(0.5, 2)`.
#' @param game_types Character vector of Baseball Savant game type codes to include.
#'   Default `c("R", "PO")` is regular season and postseason; add `"S"` for spring training.
#' @param verbose Logical. If `TRUE`, prints sample sizes and the pre-location outcome rates.
#'   Default is `FALSE`.
#'
#' @return A data frame with one row per outcome and columns `Outcome` and `Probability`
#' (the share of simulated plate appearances ending in that outcome; sums to 1). The
#' attribute `"matchup"` holds the inputs to the simulation: plate-appearance counts, the
#' shrunken batter and pitcher rates, the league rates and the combined (pre-location) rates.
#'
#' @details
#' **Data.** Pitch-level Statcast data for both players is downloaded from Baseball Savant
#' for the date window. Rates are computed per plate appearance (PA): one row per PA, the
#' row whose `events` is non-empty.
#'
#' **Outcome mapping.** Statcast `events` are mapped to eight outcomes:
#' - `strikeout`, `strikeout_double_play` become **strikeout**.
#' - `walk`, `intent_walk` and `hit_by_pitch` become **walk**. Hit-by-pitch is folded in
#'   because it has the same result for the batter (he takes first base).
#' - `single`, `double`, `triple`, `home_run` map to themselves.
#' - Other in-play outs, fielder's choices, reached-on-error and sacrifices
#'   (`field_out`, `force_out`, `grounded_into_double_play`, `double_play`, `triple_play`,
#'   `fielders_choice`, `fielders_choice_out`, `field_error`, `sac_fly`, `sac_bunt`,
#'   `sac_fly_double_play`, `sac_bunt_double_play`) become **groundout** when
#'   `bb_type == "ground_ball"` and **flyout** otherwise (fly ball, line drive, popup).
#' - Excluded: `catcher_interf`, and rows that do not complete a PA (`truncated_pa`,
#'   `caught_stealing_*`, `pickoff_*`, `stolen_base_*`, `wild_pitch`, `passed_ball`,
#'   `other_out`, `other_advance`, `runner_double_play`, `game_advisory`, `ejection`).
#'
#' **Combining batter and pitcher.** Each player's outcome counts are shrunk toward the
#' league rates by adding `prior_pa` pseudo-PAs at league rates:
#' \eqn{\tilde p(o) = (n_o + k\,p_{lg}(o)) / (n + k)}. The two are combined with the
#' odds-ratio (generalized log5) rule
#' \eqn{p(o) \propto p_{bat}(o)\,p_{pit}(o) / p_{lg}(o)}, normalized to sum to 1.
#'
#' The default league baseline is an approximate, constant MLB average per PA:
#' strikeout 0.23, walk 0.08, single 0.15, double 0.05, triple 0.005, home run 0.03,
#' flyout 0.20, groundout 0.24, normalized from a total of 0.985 to 1. It is not
#' season-specific. Supply `league_rates` to use your own.
#'
#' **Pitch type and location.** For each simulated PA a pitch type is sampled from the
#' pitcher's mix of PA-ending pitches, and a location from a 2D kernel density estimate
#' ([MASS::kde2d()], 50 x 50 grid, uniform jitter within the grid cell) of his PA-ending
#' pitches of that type. Pitch types with fewer than 30 PA-ending pitches use the KDE of all
#' his PA-ending pitches. The batter's hit rate (hits per PA) is computed in 0.5 ft bins of
#' `plate_x` (-2 to 2) and `plate_z` (0 to 5) for each pitch type, shrunk toward his rate on
#' that pitch type, which is shrunk toward his overall rate (both with `location_prior_pa`).
#' The four hit outcomes are multiplied by `bin rate / overall rate`, clipped to
#' `multiplier_range`, and the probabilities are renormalized. When the batter has no PAs in a
#' bin, the bin rate equals his pitch-type rate; when he has never ended a PA on that pitch
#' type, or the location falls outside the bins, the multiplier is 1. A missing bin never
#' produces a strikeout. Pitch types must match exactly (for example, `SI` and `FF` are
#' different pitches).
#'
#' **Main simplification.** Each simulated PA is decided by one sampled decisive pitch. The
#' count and pitch sequencing are not modeled, and there are no park, handedness or
#' platoon adjustments beyond what is already in each player's own data. The location
#' adjustment is relative to the batter's own average, so a pitcher's location tendencies
#' are partly reflected both in his outcome rates and in the multiplier.
#'
#' **Reproducibility.** Simulations run in fixed chunks of 1000 PAs, and each chunk gets
#' its own L'Ecuyer-CMRG random-number stream derived from `seed`
#' ([parallel::nextRNGStream()]). Because the streams belong to the chunks rather than to
#' the workers, the same `seed` gives identical results for any `num_cores`. The global
#' random-number state is restored when the function returns.
#'
#' A warning is issued when either player has fewer than 50 PAs in the window.
#'
#' @importFrom MASS kde2d
#'
#' @examples
#' \donttest{
#' # Simulate plate appearances between Gerrit Cole and Mike Trout in the 2022 season
#' at_bat_sim(
#'   pitcher_id = 543037,  # Gerrit Cole
#'   batter_id = 545361,   # Mike Trout
#'   start_date = "2022-04-01",
#'   end_date = "2022-10-01",
#'   num_simulations = 10000,
#'   num_cores = 2,
#'   seed = 42
#' )
#' }
#'
#' @export
at_bat_sim <- function(pitcher_id, batter_id, start_date, end_date, num_simulations = 1000,
                       num_cores = 2, seed = NULL, league_rates = NULL, prior_pa = 50,
                       location_prior_pa = 20, multiplier_range = c(0.5, 2),
                       game_types = c("R", "PO"), verbose = FALSE) {
  if (length(pitcher_id) != 1 || length(batter_id) != 1) {
    stop("at_bat_sim() takes one pitcher_id and one batter_id.", call. = FALSE)
  }
  num_simulations <- as.integer(num_simulations)
  if (is.na(num_simulations) || num_simulations < 1) {
    stop("num_simulations must be a positive number.", call. = FALSE)
  }

  # Step 1: Retrieve data for pitcher and batter
  pitcher_data <- fetch_statcast_data(pitcher_id, start_date, end_date, "pitcher", game_types)
  batter_data <- fetch_statcast_data(batter_id, start_date, end_date, "batter", game_types)

  # Step 2: Precompute rates, KDEs and lookup tables once
  model <- build_matchup_model(pitcher_data, batter_data, league_rates = league_rates,
                               prior_pa = prior_pa, location_prior_pa = location_prior_pa,
                               multiplier_range = multiplier_range)

  if (model$pitcher_pa < 50) {
    warning("Pitcher ", pitcher_id, " has only ", model$pitcher_pa,
            " plate appearances in this window; his rates lean heavily on the league prior.",
            call. = FALSE)
  }
  if (model$batter_pa < 50) {
    warning("Batter ", batter_id, " has only ", model$batter_pa,
            " plate appearances in this window; his rates lean heavily on the league prior.",
            call. = FALSE)
  }
  if (verbose) {
    message("Pitcher PAs: ", model$pitcher_pa, "; batter PAs: ", model$batter_pa)
    message("Combined rates before location adjustment:\n",
            paste(sprintf("  %-9s %.3f", names(model$base_probs), model$base_probs), collapse = "\n"))
  }

  # Step 3: Simulate
  run_matchup_simulation(model, num_simulations, num_cores, seed)
}

## === MODEL CONSTRUCTION ===

default_league_rates <- function() {
  rates <- c(strikeout = 0.23, walk = 0.08, single = 0.15, double = 0.05, triple = 0.005,
             home_run = 0.03, flyout = 0.20, groundout = 0.24)
  rates / sum(rates)
}

check_league_rates <- function(league_rates) {
  if (is.null(league_rates)) return(default_league_rates())
  if (!is.numeric(league_rates) || is.null(names(league_rates)) ||
      !setequal(names(league_rates), sim_outcomes)) {
    stop("league_rates must be a numeric vector named: ",
         paste(sim_outcomes, collapse = ", "), ".", call. = FALSE)
  }
  league_rates <- league_rates[sim_outcomes]
  if (any(is.na(league_rates)) || any(league_rates <= 0)) {
    stop("league_rates must all be positive.", call. = FALSE)
  }
  league_rates / sum(league_rates)
}

#' Count PA outcomes
#' @return Named integer vector over the 8 simulation outcomes.
#' @noRd
outcome_counts <- function(data) {
  pa <- is_plate_appearance(data$events)
  bb_type <- if ("bb_type" %in% names(data)) data$bb_type[pa] else rep(NA_character_, sum(pa))
  outcome <- map_pa_outcome(data$events[pa], bb_type)
  counts <- table(factor(outcome, levels = sim_outcomes))
  stats::setNames(as.integer(counts), sim_outcomes)
}

#' Shrink outcome counts toward league rates (additive smoothing)
#' @noRd
shrink_rates <- function(counts, league_rates, prior_pa) {
  (counts + prior_pa * league_rates) / (sum(counts) + prior_pa)
}

#' Odds-ratio (generalized log5) combination of batter and pitcher rates
#' @noRd
combine_odds_ratio <- function(batter_rates, pitcher_rates, league_rates) {
  p <- batter_rates * pitcher_rates / league_rates
  p / sum(p)
}

# PA-ending pitches with a usable pitch type and location.
decisive_pitches <- function(data) {
  keep <- is_plate_appearance(data$events) & !is.na(data$pitch_type) &
    nzchar(as.character(data$pitch_type)) & !is.na(data$plate_x) & !is.na(data$plate_z)
  data.frame(
    pitch_type = as.character(data$pitch_type[keep]),
    plate_x = as.numeric(data$plate_x[keep]),
    plate_z = as.numeric(data$plate_z[keep]),
    is_hit = as.character(data$events[keep]) %in% hit_outcomes,
    stringsAsFactors = FALSE
  )
}

#' 2D KDE of pitch locations with a precomputed sampling grid
#' @noRd
location_kde <- function(x, z, lims, n_grid = 50) {
  kde <- kde2d(x, z, n = n_grid, lims = lims)
  prob <- pmax(as.vector(kde$z), 0)
  grid <- expand.grid(plate_x = kde$x, plate_z = kde$y)
  list(prob = prob / sum(prob), grid_x = grid$plate_x, grid_z = grid$plate_z,
       dx = diff(kde$x[1:2]), dz = diff(kde$y[1:2]))
}

#' Batter hit-rate multipliers by pitch type and location bin
#'
#' Hierarchical shrinkage: bin rate -> pitch-type rate -> overall rate.
#' @return A list of (x bins) x (z bins) multiplier matrices, one per pitch type.
#' @noRd
location_multipliers <- function(batter_pitches, pitch_types, x_bins, z_bins,
                                 location_prior_pa, multiplier_range) {
  nx <- length(x_bins) - 1
  nz <- length(z_bins) - 1
  ones <- matrix(1, nx, nz)
  overall <- mean(batter_pitches$is_hit)
  if (nrow(batter_pitches) == 0 || overall == 0) {
    return(stats::setNames(rep(list(ones), length(pitch_types)), pitch_types))
  }
  k <- location_prior_pa
  xi <- cut(batter_pitches$plate_x, x_bins, include.lowest = TRUE, labels = FALSE)
  zi <- cut(batter_pitches$plate_z, z_bins, include.lowest = TRUE, labels = FALSE)
  mults <- lapply(pitch_types, function(pt) {
    rows <- batter_pitches$pitch_type == pt
    pt_rate <- (sum(batter_pitches$is_hit[rows]) + k * overall) / (sum(rows) + k)
    in_grid <- rows & !is.na(xi) & !is.na(zi)
    hits <- matrix(0, nx, nz)
    pas <- matrix(0, nx, nz)
    if (any(in_grid)) {
      cells <- cbind(xi[in_grid], zi[in_grid])
      for (r in seq_len(nrow(cells))) {
        pas[cells[r, 1], cells[r, 2]] <- pas[cells[r, 1], cells[r, 2]] + 1
        hits[cells[r, 1], cells[r, 2]] <- hits[cells[r, 1], cells[r, 2]] +
          batter_pitches$is_hit[in_grid][r]
      }
    }
    bin_rate <- (hits + k * pt_rate) / (pas + k)
    pmin(pmax(bin_rate / overall, multiplier_range[1]), multiplier_range[2])
  })
  stats::setNames(mults, pitch_types)
}

#' Precompute everything the simulation loop needs
#' @noRd
build_matchup_model <- function(pitcher_data, batter_data, league_rates = NULL, prior_pa = 50,
                                location_prior_pa = 20, multiplier_range = c(0.5, 2),
                                min_kde_pitches = 30) {
  if (!is.numeric(multiplier_range) || length(multiplier_range) != 2 ||
      multiplier_range[1] <= 0 || multiplier_range[1] > 1 || multiplier_range[2] < 1) {
    stop("multiplier_range must be c(low, high) with 0 < low <= 1 <= high.", call. = FALSE)
  }
  if (!is.numeric(prior_pa) || prior_pa < 0 || !is.numeric(location_prior_pa) ||
      location_prior_pa < 0) {
    stop("prior_pa and location_prior_pa must be non-negative numbers.", call. = FALSE)
  }
  league <- check_league_rates(league_rates)

  pitcher_counts <- outcome_counts(pitcher_data)
  batter_counts <- outcome_counts(batter_data)
  pitcher_rates <- shrink_rates(pitcher_counts, league, prior_pa)
  batter_rates <- shrink_rates(batter_counts, league, prior_pa)
  base_probs <- combine_odds_ratio(batter_rates, pitcher_rates, league)

  # Pitch-type usage and location KDEs from the pitcher's PA-ending pitches
  pitcher_pitches <- decisive_pitches(pitcher_data)
  if (nrow(pitcher_pitches) < 2) {
    stop("Not enough pitch-location data for the pitcher in this window.", call. = FALSE)
  }
  usage <- table(pitcher_pitches$pitch_type)
  usage <- stats::setNames(as.numeric(usage) / sum(usage), names(usage))
  lims <- c(range(pitcher_pitches$plate_x) + c(-0.5, 0.5),
            range(pitcher_pitches$plate_z) + c(-0.5, 0.5))
  all_kde <- location_kde(pitcher_pitches$plate_x, pitcher_pitches$plate_z, lims)
  kdes <- lapply(names(usage), function(pt) {
    rows <- pitcher_pitches$pitch_type == pt
    if (sum(rows) < min_kde_pitches) return(all_kde)
    tryCatch(location_kde(pitcher_pitches$plate_x[rows], pitcher_pitches$plate_z[rows], lims),
             error = function(e) all_kde)
  })

  # Batter's location multipliers for each of the pitcher's pitch types
  x_bins <- seq(-2, 2, by = 0.5)
  z_bins <- seq(0, 5, by = 0.5)
  mults <- location_multipliers(decisive_pitches(batter_data), names(usage), x_bins, z_bins,
                                location_prior_pa, multiplier_range)

  list(
    pitcher_pa = sum(is_plate_appearance(pitcher_data$events)),
    batter_pa = sum(is_plate_appearance(batter_data$events)),
    league_rates = league, pitcher_rates = pitcher_rates, batter_rates = batter_rates,
    base_probs = base_probs, usage = usage, kdes = kdes, mults = unname(mults),
    x_bins = x_bins, z_bins = z_bins,
    hit_cols = match(hit_outcomes, sim_outcomes)
  )
}

## === SIMULATION ===

# Simulate one chunk of PAs on its own RNG stream. Uses only base/stats so it can run on
# cluster workers that do not have this package loaded; its environment is reset to the
# global environment before it is sent to workers.
run_sim_chunk <- function(chunk, model) {
  RNGkind("L'Ecuyer-CMRG")
  assign(".Random.seed", chunk$rng_seed, envir = globalenv())
  n <- chunk$n
  pt <- sample.int(length(model$usage), n, replace = TRUE, prob = model$usage)
  mult <- rep(1, n)
  for (t in seq_along(model$usage)) {
    idx <- which(pt == t)
    if (length(idx) == 0) next
    kde <- model$kdes[[t]]
    cell <- sample.int(length(kde$prob), length(idx), replace = TRUE, prob = kde$prob)
    x <- kde$grid_x[cell] + stats::runif(length(idx), -kde$dx / 2, kde$dx / 2)
    z <- kde$grid_z[cell] + stats::runif(length(idx), -kde$dz / 2, kde$dz / 2)
    # (a, b] bins with the first bin closed, matching cut(include.lowest = TRUE)
    xi <- findInterval(x, model$x_bins, left.open = TRUE, rightmost.closed = TRUE)
    zi <- findInterval(z, model$z_bins, left.open = TRUE, rightmost.closed = TRUE)
    ok <- xi >= 1 & xi < length(model$x_bins) & zi >= 1 & zi < length(model$z_bins)
    mult[idx[ok]] <- model$mults[[t]][cbind(xi[ok], zi[ok])]
  }
  probs <- matrix(model$base_probs, n, length(model$base_probs), byrow = TRUE)
  probs[, model$hit_cols] <- probs[, model$hit_cols] * mult
  probs <- probs / rowSums(probs)
  cum <- t(apply(probs, 1, cumsum))
  u <- stats::runif(n)
  outcome <- pmin(1 + rowSums(u > cum), length(model$base_probs))
  tabulate(outcome, nbins = length(model$base_probs))
}

#' Split simulations into fixed chunks, each with its own L'Ecuyer-CMRG stream
#' @noRd
make_sim_chunks <- function(num_simulations, seed, chunk_size = 1000L) {
  sizes <- rep(chunk_size, num_simulations %/% chunk_size)
  if (num_simulations %% chunk_size > 0) sizes <- c(sizes, num_simulations %% chunk_size)
  RNGkind("L'Ecuyer-CMRG")
  set.seed(seed)
  s <- get(".Random.seed", envir = globalenv())
  lapply(sizes, function(n) {
    chunk <- list(n = n, rng_seed = s)
    s <<- parallel::nextRNGStream(s)
    chunk
  })
}

#' Run the simulation and summarize outcome frequencies
#' @noRd
run_matchup_simulation <- function(model, num_simulations, num_cores = 2, seed = NULL) {
  if (is.null(seed)) seed <- sample.int(.Machine$integer.max, 1L)

  # Restore the caller's RNG kind and state on exit
  old_kind <- RNGkind()
  had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = globalenv()) else NULL
  on.exit({
    do.call(RNGkind, as.list(old_kind))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = globalenv())
    } else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)

  chunks <- make_sim_chunks(num_simulations, seed)

  # Ensure num_cores does not exceed the number of available cores or chunks
  max_cores <- parallel::detectCores(logical = TRUE)
  if (!is.na(max_cores) && num_cores > max_cores) {
    warning("Requested ", num_cores, " cores, but only ", max_cores,
            " are available. Adjusting to ", max_cores, " cores.", call. = FALSE)
    num_cores <- max_cores
  }
  num_cores <- max(1L, min(as.integer(num_cores), length(chunks)))

  worker <- run_sim_chunk
  environment(worker) <- globalenv()
  if (num_cores > 1) {
    cl <- parallel::makeCluster(num_cores)
    on.exit(parallel::stopCluster(cl), add = TRUE)
    parallel::clusterSetRNGStream(cl, seed)
    counts <- parallel::parLapply(cl, chunks, worker, model = model)
  } else {
    counts <- lapply(chunks, worker, model = model)
  }

  # Summarize results
  counts <- Reduce(`+`, counts)
  df <- data.frame(Outcome = sim_outcomes, Probability = counts / sum(counts),
                   stringsAsFactors = FALSE)
  attr(df, "matchup") <- model[c("pitcher_pa", "batter_pa", "batter_rates",
                                 "pitcher_rates", "league_rates", "base_probs")]
  return(df)
}
