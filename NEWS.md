# baseballscouting 0.1.0

## Simulation (`at_bat_sim()`)

* Outcome probabilities now depend on the batter, the pitcher, and the sampled
  pitch type and location. Previously every matchup drew from the same fixed
  league distribution, and a batter with no data in the sampled location bin
  always struck out.
* Batter and pitcher per-PA outcome rates are shrunk toward league rates and
  combined with the odds-ratio (generalized log5) method. Hit outcomes are then
  scaled by the batter's smoothed hit rate on the sampled pitch type and location.
* Rates are per plate appearance (the old code divided by pitches).
* The default league baseline is normalized to sum to 1 (it summed to 0.985).
* Results are no longer fixed by a hard-coded `set.seed(42)`. With `seed`, results
  are reproducible and identical for any `num_cores`.
* The function returns the data frame instead of printing it. The returned data
  frame lists all eight outcomes in a fixed order and carries a `"matchup"`
  attribute with the inputs.
* New optional arguments: `seed`, `league_rates`, `prior_pa`, `location_prior_pa`,
  `multiplier_range`, `game_types`, `verbose`.
* Data now comes from the same Baseball Savant request as the report functions.
  `baseballr::statcast_search_*()` (baseballr 1.6.0) fails against the current
  Savant CSV, so `baseballr` is no longer a dependency.
* Warns when either player has fewer than 50 PAs in the window.

## Reports (`generate_pitcher()`, `generate_hitter()`)

* Dates control the data range. A hard-coded 2024 season filter is removed.
* Spring training is excluded by default. New optional `game_types` argument.
* The pitcher strike-zone heat map orientation is fixed: zone 1 is top-left,
  from the catcher's view.
* Whiff rate is whiffs per swing, matching Baseball Savant's Whiff%. The color
  scale no longer clips at 0.2.
* K% and BB% use every completed plate appearance as the denominator. Intentional
  walks count as walks.
* Several player IDs now return a named list of per-player plots. Previously the
  data was blended under the first player's name.
* Hard-hit rate excludes batted balls with no recorded exit velocity.
* The hitter central-zone heat map now covers -1 to 1 ft horizontally. The
  (0.5, 1] column was previously dropped.
* The general-stats tables add batters faced or plate appearances, pitcher whiff
  rate, pitch usage and hitter walk rate.

## Other

* Fixed the `generate_pitcher()` example ID for Gerrit Cole (543037).
* Network examples are wrapped in `\donttest{}`.
* Added an offline testthat suite with fixture data.
* Removed unused dependencies: `baseballr`, `dplyr`, `tidyr`, `kableExtra`.
