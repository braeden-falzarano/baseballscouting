# Generate Hitter Performance Report

This function fetches Statcast data for a given hitter or list of
hitters, calculates key performance metrics, and generates a strike zone
heatmap displaying the average exit velocity in the central zone.

## Usage

``` r
generate_hitter(player_ids, start_date, end_date, game_types = c("R", "PO"))
```

## Arguments

- player_ids:

  A vector of numeric player IDs representing the hitters to analyze.

- start_date:

  A character string representing the start date of the data range in
  the format "YYYY-MM-DD".

- end_date:

  A character string representing the end date of the data range in the
  format "YYYY-MM-DD".

- game_types:

  Character vector of Baseball Savant game type codes to include.
  Default `c("R", "PO")` is regular season and postseason; add `"S"` to
  include spring training.

## Value

For a single ID, a `ggplot` object showing the central strike zone
heatmap with average exit velocity (or `NULL`, invisibly, when no data
is found). For several IDs, a named list with one such plot per hitter,
named by player ID. Additionally, a table summarizing general hitting
statistics is drawn for each hitter.

## Details

The function performs the following for each hitter separately:

- Fetches player names using the MLB Stats API.

- Fetches Statcast data from Baseball Savant for the hitter and date
  range. Only `start_date` and `end_date` restrict the dates; there is
  no season filter.

- Identifies batted balls: plate appearances ending in a hit, an in-play
  out, a fielder's choice, an error or a sacrifice.

- Calculates key metrics such as average exit velocity, launch angle,
  hard-hit rate, strikeout rate and walk rate.

- Creates a strike zone heatmap focusing on the central strike zone.

Definitions:

- Plate appearances are rows with a non-empty `events` value, excluding
  baserunning events and `truncated_pa`.

- Hard-hit rate = batted balls with exit velocity of 95 mph or more /
  batted balls with a recorded exit velocity.

- Strikeout rate = (`strikeout` + `strikeout_double_play`) / PA. Walk
  rate = (`walk` + `intent_walk`) / PA.

The heatmap covers `plate_x` from -1 to 1 ft and `plate_z` from 1 to 3.5
ft in 0.5 ft cells, drawn from the catcher's view (negative `plate_x` is
the third-base side).

## Note

This function requires an active internet connection to fetch data from
the MLB Stats API and Baseball Savant.

## Examples

``` r
# \donttest{
# Example usage:
hitter_ids <- c(668804)  # Bryan Reynolds
start_date <- "2024-01-01"
end_date <- "2024-12-31"
generate_hitter(player_ids = hitter_ids, start_date = start_date, end_date = end_date)
#> Fetching Statcast data for batter 668804 ...


# }
```
