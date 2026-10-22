# Regenerates the fixture CSVs used by the offline tests. Needs internet; not run by tests.
# Run from the package root: Rscript tests/testthat/fixtures/make-fixtures.R
devtools::load_all(quiet = TRUE)
cols <- c("pitch_type", "game_date", "game_type", "release_speed", "plate_x", "plate_z",
          "zone", "description", "events", "bb_type", "launch_speed", "launch_angle",
          "batter", "pitcher", "at_bat_number", "pitch_number")
pitcher <- fetch_statcast_data(543037, "2022-04-01", "2022-05-31", "pitcher")  # Gerrit Cole
batter <- fetch_statcast_data(545361, "2022-04-01", "2022-05-31", "batter")    # Mike Trout
data.table::fwrite(pitcher[, ..cols], "tests/testthat/fixtures/pitcher_543037_2022_apr_may.csv")
data.table::fwrite(batter[, ..cols], "tests/testthat/fixtures/batter_545361_2022_apr_may.csv")
