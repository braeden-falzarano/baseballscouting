# Simulate At-Bat Outcomes Between a Pitcher and a Batter

This function simulates plate appearances between a specified pitcher
and batter using their Statcast data. Outcome probabilities combine the
batter's, the pitcher's and the league's outcome rates with the
odds-ratio (generalized log5) method, and are then adjusted for the
batter's success against the sampled pitch type and location.

## Usage

``` r
at_bat_sim(
  pitcher_id,
  batter_id,
  start_date,
  end_date,
  num_simulations = 1000,
  num_cores = 2,
  seed = NULL,
  league_rates = NULL,
  prior_pa = 50,
  location_prior_pa = 20,
  multiplier_range = c(0.5, 2),
  game_types = c("R", "PO"),
  verbose = FALSE
)
```

## Arguments

- pitcher_id:

  Numeric. The ID of the pitcher (Statcast / MLBAM pitcher ID).

- batter_id:

  Numeric. The ID of the batter (Statcast / MLBAM batter ID).

- start_date:

  Character. The start date of the data retrieval period in "YYYY-MM-DD"
  format.

- end_date:

  Character. The end date of the data retrieval period in "YYYY-MM-DD"
  format.

- num_simulations:

  Numeric. The number of plate appearances to simulate. Default is 1000.

- num_cores:

  Numeric. The number of CPU cores to use for parallel processing.
  Default is 2.

- seed:

  Optional integer. When supplied, results are reproducible, including
  across different values of `num_cores`. Default `NULL` draws a seed
  from the current R session, so calling
  [`set.seed()`](https://rdrr.io/r/base/Random.html) beforehand also
  makes results reproducible.

- league_rates:

  Optional named numeric vector of league outcome rates with names
  `strikeout`, `walk`, `single`, `double`, `triple`, `home_run`,
  `flyout`, `groundout`. It is normalized to sum to 1. Default `NULL`
  uses an approximate MLB average (see Details).

- prior_pa:

  Numeric. Prior strength, in plate appearances, used to shrink each
  player's outcome rates toward `league_rates`. Default is 50.

- location_prior_pa:

  Numeric. Prior strength, in plate appearances, used to shrink the
  batter's hit rate in each pitch type and location bin toward the
  batter's pitch-type rate, and the pitch-type rate toward the batter's
  overall rate. Default is 20.

- multiplier_range:

  Numeric vector of length 2. The location/pitch-type multiplier applied
  to hit outcomes is clipped to this range. Default is `c(0.5, 2)`.

- game_types:

  Character vector of Baseball Savant game type codes to include.
  Default `c("R", "PO")` is regular season and postseason; add `"S"` for
  spring training.

- verbose:

  Logical. If `TRUE`, prints sample sizes and the pre-location outcome
  rates. Default is `FALSE`.

## Value

A data frame with one row per outcome and columns `Outcome` and
`Probability` (the share of simulated plate appearances ending in that
outcome; sums to 1). The attribute `"matchup"` holds the inputs to the
simulation: plate-appearance counts, the shrunken batter and pitcher
rates, the league rates and the combined (pre-location) rates.

## Details

**Data.** Pitch-level Statcast data for both players is downloaded from
Baseball Savant for the date window. Rates are computed per plate
appearance (PA): one row per PA, the row whose `events` is non-empty.

**Outcome mapping.** Statcast `events` are mapped to eight outcomes:

- `strikeout`, `strikeout_double_play` become **strikeout**.

- `walk`, `intent_walk` and `hit_by_pitch` become **walk**. Hit-by-pitch
  is folded in because it has the same result for the batter (they reach
  first base).

- `single`, `double`, `triple`, `home_run` map to themselves.

- Other in-play outs, fielder's choices, reached-on-error and sacrifices
  (`field_out`, `force_out`, `grounded_into_double_play`, `double_play`,
  `triple_play`, `fielders_choice`, `fielders_choice_out`,
  `field_error`, `sac_fly`, `sac_bunt`, `sac_fly_double_play`,
  `sac_bunt_double_play`) become **groundout** when
  `bb_type == "ground_ball"` and **flyout** otherwise (fly ball, line
  drive, popup).

- Excluded: `catcher_interf`, and rows that do not complete a PA
  (`truncated_pa`, `caught_stealing_*`, `pickoff_*`, `stolen_base_*`,
  `wild_pitch`, `passed_ball`, `other_out`, `other_advance`,
  `runner_double_play`, `game_advisory`, `ejection`).

**Combining batter and pitcher.** Each player's outcome counts are
shrunk toward the league rates by adding `prior_pa` pseudo-PAs at league
rates: \\\tilde p(o) = (n_o + k\\p\_{lg}(o)) / (n + k)\\. The two are
combined with the odds-ratio (generalized log5) rule \\p(o) \propto
p\_{bat}(o)\\p\_{pit}(o) / p\_{lg}(o)\\, normalized to sum to 1.

The default league baseline is an approximate, constant MLB average per
PA: strikeout 0.23, walk 0.08, single 0.15, double 0.05, triple 0.005,
home run 0.03, flyout 0.20, groundout 0.24, normalized from a total of
0.985 to 1. It is not season-specific. Supply `league_rates` to use your
own.

**Pitch type and location.** For each simulated PA a pitch type is
sampled from the pitcher's mix of PA-ending pitches, and a location from
a 2D kernel density estimate
([`MASS::kde2d()`](https://rdrr.io/pkg/MASS/man/kde2d.html), 50 x 50
grid, uniform jitter within the grid cell) of the pitcher's PA-ending
pitches of that type. Pitch types with fewer than 30 PA-ending pitches
use the KDE of all the pitcher's PA-ending pitches. The batter's hit
rate (hits per PA) is computed in 0.5 ft bins of `plate_x` (-2 to 2) and
`plate_z` (0 to 5) for each pitch type, shrunk toward the batter's rate
on that pitch type, which is shrunk toward the overall rate (both with
`location_prior_pa`). The four hit outcomes are multiplied by
`bin rate / overall rate`, clipped to `multiplier_range`, and the
probabilities are renormalized. When the batter has no PAs in a bin, the
bin rate equals the pitch-type rate; when the batter has never ended a
PA on that pitch type, or the location falls outside the bins, the
multiplier is 1. A missing bin never produces a strikeout. Pitch types
must match exactly (for example, `SI` and `FF` are different pitches).

**Main simplification.** Each simulated PA is decided by one sampled
decisive pitch. The count and pitch sequencing are not modeled, and
there are no park, handedness or platoon adjustments beyond what is
already in each player's own data. The location adjustment is relative
to the batter's own average, so a pitcher's location tendencies are
partly reflected both in the pitcher's outcome rates and in the
multiplier.

**Reproducibility.** Simulations run in fixed chunks of 1000 PAs, and
each chunk gets its own L'Ecuyer-CMRG random-number stream derived from
`seed`
([`parallel::nextRNGStream()`](https://rdrr.io/r/parallel/RngStream.html)).
Because the streams belong to the chunks rather than to the workers, the
same `seed` gives identical results for any `num_cores`. The global
random-number state is restored when the function returns.

A warning is issued when either player has fewer than 50 PAs in the
window.

## Examples

``` r
# \donttest{
# Simulate plate appearances between Gerrit Cole and Mike Trout in the 2022 season
at_bat_sim(
  pitcher_id = 543037,  # Gerrit Cole
  batter_id = 545361,   # Mike Trout
  start_date = "2022-04-01",
  end_date = "2022-10-01",
  num_simulations = 10000,
  num_cores = 2,
  seed = 42
)
#> Fetching Statcast data for pitcher 543037 ...
#> Fetching Statcast data for batter 545361 ...
#>     Outcome Probability
#> 1 strikeout      0.3593
#> 2      walk      0.0941
#> 3    single      0.0757
#> 4    double      0.0414
#> 5    triple      0.0016
#> 6  home_run      0.0903
#> 7    flyout      0.2381
#> 8 groundout      0.0995
# }
```
