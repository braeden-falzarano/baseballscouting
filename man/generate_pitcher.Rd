% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generatepitcher.R
\name{generate_pitcher}
\alias{generate_pitcher}
\title{Generate Pitcher Performance Report}
\usage{
generate_pitcher(player_ids, start_date, end_date)
}
\arguments{
\item{player_ids}{A vector of numeric player IDs representing the pitchers to analyze.}

\item{start_date}{A character string representing the start date of the data range in the format "YYYY-MM-DD".}

\item{end_date}{A character string representing the end date of the data range in the format "YYYY-MM-DD".}
}
\value{
A \code{ggplot} object showing the strike zone heatmap with the most frequently used pitch type and whiff rate for each zone.
Additionally, tables summarizing general pitching statistics and pitch-specific statistics are displayed in the viewer.
}
\description{
This function fetches Statcast data for a given pitcher or list of pitchers, calculates key performance metrics,
and generates a strike zone heatmap displaying the most frequently used pitch type and whiff rate for each zone.
}
\details{
The function performs the following steps:
\itemize{
\item \strong{Fetches player names} using the MLB Stats API.
\item \strong{Fetches Statcast data} from Baseball Savant for the specified pitcher(s) and date range.
\item \strong{Calculates general statistics} such as walk rate and strikeout rate.
\item \strong{Generates pitch-specific statistics} such as the average velocity for each pitch type.
\item \strong{Creates a strike zone heatmap} visualizing the most frequently used pitch type and the whiff rate in each strike zone location.
}

The results include a strike zone heatmap and summary tables for general and pitch-specific statistics.
}
\note{
This function requires an active internet connection to fetch data from the MLB Stats API and Baseball Savant.
}
\examples{
# Example usage:
pitcher_ids <- c(605483)  # Gerrit Cole
start_date <- "2024-01-01"
end_date <- "2024-12-31"
generate_pitcher(player_ids = pitcher_ids, start_date = start_date, end_date = end_date)

}
