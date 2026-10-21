read_fixture <- function(name) {
  data.table::fread(test_path("fixtures", name), showProgress = FALSE)
}
pitcher_fixture <- function() read_fixture("pitcher_543037_2022_apr_may.csv")
batter_fixture <- function() read_fixture("batter_545361_2022_apr_may.csv")
