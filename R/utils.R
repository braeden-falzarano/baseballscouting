# Internal helpers shared by generate_pitcher(), generate_hitter() and at_bat_sim().

#' @importFrom utils globalVariables
NULL

utils::globalVariables(c(
  "description", "events", "game_date", "launch_speed", "launch_angle",
  "pitch_type", "plate_x", "plate_z", "player_id", "player_name",
  "release_speed", "zone", "x", "y", "whiff_rate", "most_used_pitch",
  "grid_x", "grid_z", "avg_exit_velocity", "label", "text_color", "pitches",
  "usage", "avg_velocity", "."
))

## === EVENT DEFINITIONS ===

# The 8 outcomes reported by at_bat_sim(), in display order.
sim_outcomes <- c("strikeout", "walk", "single", "double", "triple",
                  "home_run", "flyout", "groundout")
hit_outcomes <- c("single", "double", "triple", "home_run")

# Values of `events` that end a pitch row without completing a plate
# appearance (baserunning plays, truncated PAs, administrative rows).
non_pa_events <- c(
  "truncated_pa", "game_advisory", "ejection", "wild_pitch", "passed_ball",
  "other_advance", "other_out", "runner_double_play"
)
non_pa_prefixes <- "^(caught_stealing|pickoff|stolen_base)"

# PA events that are in-play non-hits; split into groundout/flyout by bb_type.
in_play_out_events <- c(
  "field_out", "force_out", "grounded_into_double_play", "double_play",
  "triple_play", "fielders_choice", "fielders_choice_out", "field_error",
  "sac_fly", "sac_bunt", "sac_fly_double_play", "sac_bunt_double_play"
)

# Pitch descriptions used for whiff rate (per swing). foul_tip counts as a whiff,
# which reproduces Baseball Savant's Whiff% exactly.
whiff_descriptions <- c("swinging_strike", "swinging_strike_blocked", "foul_tip",
                        "missed_bunt", "swinging_pitchout")
contact_swing_descriptions <- c("foul", "foul_bunt", "bunt_foul_tip",
                                "foul_pitchout", "hit_into_play")

#' Identify rows that complete a plate appearance
#'
#' A row is a PA when `events` is non-empty, non-NA and is not a baserunning
#' or administrative event (see `non_pa_events`).
#' @param events Character vector of Statcast `events`.
#' @return Logical vector.
#' @noRd
is_plate_appearance <- function(events) {
  events <- as.character(events)
  !is.na(events) & nzchar(events) &
    !(events %in% non_pa_events) & !grepl(non_pa_prefixes, events)
}

#' Map Statcast events to simulation outcomes
#'
#' Hit-by-pitch is folded into "walk" (same base/out result for the batter).
#' Catcher's interference and anything unrecognised map to NA so they are
#' dropped from outcome rates (they still count as PAs in report denominators).
#' @param events Character vector of Statcast `events`.
#' @param bb_type Character vector of Statcast `bb_type`, same length.
#' @return Character vector of outcomes (or NA).
#' @noRd
map_pa_outcome <- function(events, bb_type) {
  events <- as.character(events)
  bb_type <- as.character(bb_type)
  out <- rep(NA_character_, length(events))
  out[events %in% c("strikeout", "strikeout_double_play")] <- "strikeout"
  out[events %in% c("walk", "intent_walk", "hit_by_pitch")] <- "walk"
  is_hit <- events %in% hit_outcomes
  out[is_hit] <- events[is_hit]
  in_play <- events %in% in_play_out_events
  out[in_play] <- ifelse(!is.na(bb_type[in_play]) & bb_type[in_play] == "ground_ball",
                         "groundout", "flyout")
  out
}

#' Count PAs, strikeouts and walks
#'
#' @param data Statcast pitch-level data with an `events` column.
#' @return A list with `pa`, `strikeouts`, `walks` (unintentional + intentional),
#'   `k_rate` and `bb_rate`.
#' @noRd
pa_summary <- function(data) {
  ev <- as.character(data$events)[is_plate_appearance(data$events)]
  pa <- length(ev)
  k <- sum(ev %in% c("strikeout", "strikeout_double_play"))
  bb <- sum(ev %in% c("walk", "intent_walk"))
  list(
    pa = pa, strikeouts = k, walks = bb,
    k_rate = if (pa > 0) k / pa else NA_real_,
    bb_rate = if (pa > 0) bb / pa else NA_real_
  )
}

#' Whiff rate per swing
#' @param description Character vector of Statcast pitch `description`.
#' @return Whiffs / swings, or NA when there are no swings.
#' @noRd
whiff_rate_per_swing <- function(description) {
  whiffs <- sum(description %in% whiff_descriptions)
  swings <- whiffs + sum(description %in% contact_swing_descriptions |
                           startsWith(as.character(description), "hit_into_play_"))
  if (swings == 0) NA_real_ else whiffs / swings
}

## === DATA ACCESS ===

savant_game_types <- c("R", "PO", "F", "D", "L", "W", "S", "A", "E")

#' Build a Baseball Savant Statcast CSV search URL
#'
#' No season filter is applied; `start_date` and `end_date` control the range.
#' @noRd
build_savant_url <- function(player_id, start_date, end_date, player_type,
                             game_types = c("R", "PO")) {
  player_type <- match.arg(player_type, c("pitcher", "batter"))
  bad <- setdiff(game_types, savant_game_types)
  if (length(bad) > 0) {
    stop("Unknown game_types: ", paste(bad, collapse = ", "),
         ". Use any of: ", paste(savant_game_types, collapse = ", "), ".", call. = FALSE)
  }
  start_date <- format(as.Date(start_date))
  end_date <- format(as.Date(end_date))
  paste0(
    "https://baseballsavant.mlb.com/statcast_search/csv?all=true",
    "&hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=",
    "&hfGT=", paste0(game_types, "%7C", collapse = ""),
    "&hfC=&hfSea=&hfSit=&hfOuts=&opponent=&pitcher_throws=&batter_stands=",
    "&hfSA=&player_type=", player_type,
    "&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=",
    "&", player_type, "s_lookup%5B%5D=", player_id,
    "&game_date_gt=", start_date, "&game_date_lt=", end_date,
    "&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0",
    "&group_by=name&sort_col=pitches&player_event_sort=h_launch_speed",
    "&sort_order=desc&min_abs=0&type=details"
  )
}

#' Fetch pitch-level Statcast data for one player from Baseball Savant
#'
#' @return A data.table with one row per pitch. Errors if the request fails or
#'   Savant returns no rows.
#' @noRd
fetch_statcast_data <- function(player_id, start_date, end_date, player_type,
                                game_types = c("R", "PO"), timeout_sec = 120) {
  url <- build_savant_url(player_id, start_date, end_date, player_type, game_types)
  message("Fetching Statcast data for ", player_type, " ", player_id, " ...")
  response <- tryCatch(
    httr::GET(url, httr::timeout(timeout_sec)),
    error = function(e) {
      stop("Could not reach Baseball Savant for ", player_type, " ", player_id,
           ": ", conditionMessage(e), call. = FALSE)
    }
  )
  if (httr::http_error(response)) {
    stop("Baseball Savant returned HTTP ", httr::status_code(response),
         " for ", player_type, " ", player_id, ".", call. = FALSE)
  }
  body <- httr::content(response, as = "text", encoding = "UTF-8")
  data <- if (nzchar(trimws(body))) {
    suppressWarnings(fread(text = body, fill = TRUE, showProgress = FALSE))
  } else {
    data.table()
  }
  if (nrow(data) == 0 || !all(c("pitch_type", "events", "description") %in% names(data))) {
    stop("Baseball Savant returned no Statcast rows for ", player_type, " ", player_id,
         " between ", start_date, " and ", end_date,
         " (game types: ", paste(game_types, collapse = ", "), ").", call. = FALSE)
  }
  set(data, j = "player_id", value = player_id)
  data[]
}

#' Look up player full names from the MLB Stats API
#'
#' @return A data.table with `player_id` and `player_name` (NA on failure).
#' @noRd
fetch_player_names <- function(player_ids, timeout_sec = 30) {
  player_info <- lapply(player_ids, function(id) {
    name <- tryCatch({
      url <- paste0("https://statsapi.mlb.com/api/v1/people/", id)
      response <- httr::GET(url, httr::timeout(timeout_sec))
      parsed <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      parsed$people$fullName[1]
    }, error = function(e) NA_character_)
    if (is.null(name)) name <- NA_character_
    data.table(player_id = id, player_name = name)
  })
  rbindlist(player_info, fill = TRUE)
}

# Fixed-decimal text for report tables, so counts and rates can share a column.
format_stat <- function(x, digits) {
  if (is.na(x)) "NA" else formatC(x, format = "f", digits = digits)
}

player_name_or <- function(player_mapping, id, fallback) {
  name <- player_mapping$player_name[match(id, player_mapping$player_id)]
  if (length(name) == 0 || is.na(name)) fallback else name
}
