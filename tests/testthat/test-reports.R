test_that("zone grid is laid out left to right, top to bottom (catcher's view)", {
  g <- zone_grid()
  at <- function(z) unlist(g[g$zone == z, c("x", "y")], use.names = FALSE)
  expect_equal(at(1), c(1, 3))  # top-left
  expect_equal(at(3), c(3, 3))  # top-right
  expect_equal(at(5), c(2, 2))
  expect_equal(at(7), c(1, 1))  # bottom-left
  expect_equal(at(9), c(3, 1))  # bottom-right
})

test_that("zone summary reports most-used pitch and whiff per swing", {
  d <- data.frame(
    zone = c(1, 1, 1, 1, 5, 5, 14),
    pitch_type = c("FF", "FF", "SL", "FF", "CH", "CH", "FF"),
    description = c("swinging_strike", "foul", "ball", "hit_into_play",
                    "called_strike", "ball", "swinging_strike")
  )
  s <- pitcher_zone_summary(d)
  expect_equal(nrow(s), 9)
  expect_equal(s[zone == 1]$most_used_pitch, "FF")
  expect_equal(s[zone == 1]$whiff_rate, round(1 / 3, 3))
  expect_true(is.na(s[zone == 5]$whiff_rate))
  expect_equal(s[zone == 2]$most_used_pitch, "No Data")
})

test_that("hard-hit rate ignores batted balls without exit velocity", {
  expect_equal(hard_hit_rate(c(100, 90, NA, 96, NA)), 2 / 3)
  expect_true(is.na(hard_hit_rate(c(NA_real_, NA_real_))))
})

test_that("batted balls include errors and fielder's choices but not K or BB", {
  d <- data.frame(events = c("single", "field_error", "fielders_choice", "strikeout",
                             "walk", "", "grounded_into_double_play"),
                  bb_type = c("line_drive", "ground_ball", "ground_ball", NA, NA, NA, "ground_ball"))
  expect_equal(batted_balls(d)$events,
               c("single", "field_error", "fielders_choice", "grounded_into_double_play"))
})

test_that("report functions work offline with mocked data", {
  p <- pitcher_fixture()
  b <- batter_fixture()
  local_mocked_bindings(
    fetch_player_names = function(ids, ...) data.table::data.table(player_id = ids, player_name = "Test Player"),
    fetch_statcast_data = function(player_id, start_date, end_date, player_type, ...) {
      if (player_id == 0) stop("Baseball Savant returned no Statcast rows")
      if (player_type == "pitcher") p else b
    }
  )
  pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_s3_class(generate_pitcher(543037, "2022-04-01", "2022-05-31"), "ggplot")
  expect_s3_class(generate_hitter(545361, "2022-04-01", "2022-05-31"), "ggplot")

  multi <- suppressMessages(generate_pitcher(c(543037, 0), "2022-04-01", "2022-05-31"))
  expect_type(multi, "list")
  expect_named(multi, c("543037", "0"))
  expect_s3_class(multi[["543037"]], "ggplot")
  expect_null(multi[["0"]])

  expect_message(none <- generate_hitter(0, "2022-04-01", "2022-05-31"), "No Statcast data")
  expect_null(none)
})
