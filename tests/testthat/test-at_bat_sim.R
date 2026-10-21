test_that("default league rates are the normalized BaseRate table", {
  base <- c(0.23, 0.08, 0.15, 0.05, 0.005, 0.03, 0.20, 0.24)
  lr <- check_league_rates(NULL)
  expect_named(lr, sim_outcomes)
  expect_equal(sum(lr), 1)
  expect_equal(unname(lr), base / 0.985)
  expect_error(check_league_rates(c(strikeout = 1)), "named")
})

test_that("shrinkage adds prior_pa pseudo-PAs at league rates", {
  league <- check_league_rates(NULL)
  counts <- stats::setNames(c(10L, 0L, 0L, 0L, 0L, 0L, 0L, 0L), sim_outcomes)
  shrunk <- shrink_rates(counts, league, prior_pa = 50)
  expect_equal(sum(shrunk), 1)
  expect_equal(shrunk[["strikeout"]], (10 + 50 * league[["strikeout"]]) / 60)
  expect_equal(shrunk[["walk"]], 50 * league[["walk"]] / 60)
  expect_equal(shrink_rates(counts, league, prior_pa = 0), counts / 10)
  # No data at all -> league rates
  expect_equal(shrink_rates(counts * 0L, league, 50), league)
})

test_that("odds-ratio combination matches a hand-computed case", {
  league <- stats::setNames(rep(1 / 8, 8), sim_outcomes)
  batter <- stats::setNames(c(0.25, rep(0.125, 5), 0.0625, 0.0625), sim_outcomes)
  pitcher <- stats::setNames(c(0.25, rep(0.125, 6), 0), sim_outcomes)
  p <- combine_odds_ratio(batter, pitcher, league)
  expect_equal(sum(p), 1)
  # 8 * b * p = (0.5, 0.125 x5, 0.0625, 0); total 1.1875 = 19/16
  expect_equal(unname(p), c(8, 2, 2, 2, 2, 2, 1, 0) / 19)
  # A league-average batter returns the pitcher's rates
  expect_equal(combine_odds_ratio(league, pitcher, league), pitcher)
})

test_that("matchup model uses both players and sums to 1", {
  m <- build_matchup_model(pitcher_fixture(), batter_fixture())
  expect_equal(sum(m$base_probs), 1)
  expect_equal(sum(m$usage), 1)
  expect_true(all(vapply(m$kdes, function(k) abs(sum(k$prob) - 1) < 1e-12, logical(1))))
  expect_true(all(vapply(m$mults, function(x) all(x >= 0.5 & x <= 2), logical(1))))

  # A batter with no hits at all gets fewer hits than Trout against the same pitcher
  weak <- batter_fixture()
  weak$events[weak$events %in% c("single", "double", "triple", "home_run")] <- "strikeout"
  m_weak <- build_matchup_model(pitcher_fixture(), weak)
  expect_gt(m_weak$base_probs[["strikeout"]], m$base_probs[["strikeout"]] + 0.05)
  expect_lt(sum(m_weak$base_probs[hit_outcomes]), sum(m$base_probs[hit_outcomes]))
})

test_that("missing batter location data never defaults to strikeout", {
  b <- batter_fixture()
  b$plate_x <- NA_real_
  m <- build_matchup_model(pitcher_fixture(), b)
  expect_true(all(vapply(m$mults, function(x) all(x == 1), logical(1))))
  res <- run_matchup_simulation(m, 20000, num_cores = 1, seed = 1)
  expect_equal(res$Probability, unname(m$base_probs), tolerance = 0.02)
})

test_that("simulation output has the documented shape and sums to 1", {
  m <- build_matchup_model(pitcher_fixture(), batter_fixture())
  res <- run_matchup_simulation(m, 2500, num_cores = 1, seed = 42)
  expect_s3_class(res, "data.frame")
  expect_named(res, c("Outcome", "Probability"))
  expect_equal(res$Outcome, sim_outcomes)
  expect_equal(sum(res$Probability), 1)
  expect_named(attr(res, "matchup"), c("pitcher_pa", "batter_pa", "batter_rates",
                                       "pitcher_rates", "league_rates", "base_probs"))
})

test_that("seeded runs are reproducible and leave the caller's RNG untouched", {
  m <- build_matchup_model(pitcher_fixture(), batter_fixture())
  a <- run_matchup_simulation(m, 3000, num_cores = 1, seed = 7)
  b <- run_matchup_simulation(m, 3000, num_cores = 1, seed = 7)
  c <- run_matchup_simulation(m, 3000, num_cores = 1, seed = 8)
  expect_identical(a$Probability, b$Probability)
  expect_false(identical(a$Probability, c$Probability))

  set.seed(123)
  expected <- stats::runif(1)
  set.seed(123)
  run_matchup_simulation(m, 100, num_cores = 1, seed = 1)
  expect_identical(stats::runif(1), expected)

  # seed = NULL follows set.seed()
  set.seed(99)
  d <- run_matchup_simulation(m, 1500, num_cores = 1)
  set.seed(99)
  e <- run_matchup_simulation(m, 1500, num_cores = 1)
  expect_identical(d$Probability, e$Probability)
})

test_that("seeded runs are identical across core counts", {
  skip_on_cran()
  skip_if(parallel::detectCores() < 2, "needs 2 cores")
  m <- build_matchup_model(pitcher_fixture(), batter_fixture())
  one <- run_matchup_simulation(m, 4500, num_cores = 1, seed = 2024)
  two <- run_matchup_simulation(m, 4500, num_cores = 2, seed = 2024)
  expect_identical(one$Probability, two$Probability)
})

test_that("at_bat_sim() runs end to end and warns on small samples", {
  p <- pitcher_fixture()
  b <- batter_fixture()
  local_mocked_bindings(
    fetch_statcast_data = function(player_id, start_date, end_date, player_type, ...) {
      if (player_type == "pitcher") p else b[1:150]
    }
  )
  expect_warning(
    res <- at_bat_sim(543037, 545361, "2022-04-01", "2022-05-31",
                      num_simulations = 1000, num_cores = 1, seed = 3),
    "Batter 545361 has only"
  )
  expect_equal(sum(res$Probability), 1)
  expect_error(at_bat_sim(c(1, 2), 3, "2022-04-01", "2022-05-31"), "one pitcher_id")
})
