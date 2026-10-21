test_that("plate appearances exclude empty, NA and baserunning events", {
  ev <- c("single", "", NA, "caught_stealing_2b", "pickoff_1b", "truncated_pa",
          "grounded_into_double_play", "fielders_choice", "field_error",
          "strikeout_double_play", "intent_walk", "sac_fly_double_play", "catcher_interf")
  expect_equal(
    is_plate_appearance(ev),
    c(TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("events map to the eight simulation outcomes", {
  ev <- c("strikeout", "strikeout_double_play", "walk", "intent_walk", "hit_by_pitch",
          "single", "double", "triple", "home_run",
          "field_out", "field_out", "grounded_into_double_play", "sac_fly", "force_out",
          "catcher_interf", "caught_stealing_2b")
  bb <- c(NA, NA, NA, NA, NA, "line_drive", "fly_ball", "line_drive", "fly_ball",
          "ground_ball", "popup", "ground_ball", "fly_ball", NA, NA, NA)
  expect_equal(
    map_pa_outcome(ev, bb),
    c("strikeout", "strikeout", "walk", "walk", "walk",
      "single", "double", "triple", "home_run",
      "groundout", "flyout", "groundout", "flyout", "flyout",
      NA, NA)
  )
})

test_that("pa_summary counts every PA-ending event in the denominator", {
  d <- data.frame(events = c("strikeout", "strikeout_double_play", "walk", "intent_walk",
                             "grounded_into_double_play", "fielders_choice", "field_error",
                             "double_play", "single", "hit_by_pitch", "",
                             "caught_stealing_2b", NA))
  s <- pa_summary(d)
  expect_equal(s$pa, 10)
  expect_equal(s$strikeouts, 2)
  expect_equal(s$walks, 2)
  expect_equal(s$k_rate, 0.2)
  expect_equal(s$bb_rate, 0.2)
})

test_that("pa_summary matches a hand count on fixture data", {
  d <- pitcher_fixture()
  ev <- d$events[d$events != ""]
  s <- pa_summary(d)
  expect_equal(s$pa, length(ev))
  expect_equal(s$strikeouts, sum(ev %in% c("strikeout", "strikeout_double_play")))
})

test_that("whiff rate is per swing, with foul tips counted as whiffs", {
  desc <- c("swinging_strike", "swinging_strike_blocked", "foul_tip", "foul", "foul",
            "hit_into_play", "ball", "called_strike", "blocked_ball", "foul_bunt")
  # 3 whiffs / (3 whiffs + 2 fouls + 1 in play + 1 foul bunt) swings
  expect_equal(whiff_rate_per_swing(desc), 3 / 7)
  expect_true(is.na(whiff_rate_per_swing(c("ball", "called_strike"))))
})

test_that("Savant URL has no season filter and uses the requested window", {
  url <- build_savant_url(543037, "2023-04-01", "2023-10-01", "pitcher")
  expect_match(url, "hfSea=&", fixed = TRUE)
  expect_false(grepl("2024", url))
  expect_match(url, "game_date_gt=2023-04-01", fixed = TRUE)
  expect_match(url, "game_date_lt=2023-10-01", fixed = TRUE)
  expect_match(url, "hfGT=R%7CPO%7C&", fixed = TRUE)
  expect_match(url, "pitchers_lookup%5B%5D=543037", fixed = TRUE)
  expect_match(build_savant_url(1, "2024-01-01", "2024-12-31", "batter", c("R", "S")),
               "hfGT=R%7CS%7C&", fixed = TRUE)
  expect_error(build_savant_url(1, "2024-01-01", "2024-12-31", "pitcher", "X"), "Unknown game_types")
})
