% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ips.R
\name{load_ips}
\alias{load_ips}
\title{IPS - Amazon Social Progress Index}
\usage{
load_ips(
  dataset = "all",
  raw_data = FALSE,
  time_period = c(2014, 2018, 2021, 2023),
  language = "eng"
)
}
\arguments{
\item{dataset}{A dataset name ("all", "life_quality", "sanit_habit", "violence", "educ", "communic", "mortality", or "deforest")}

\item{raw_data}{A \code{boolean} setting the return of raw (\code{TRUE}) or processed (\code{FALSE}) data.}

\item{time_period}{Year to download. Can be 2014, 2018, 2021, 2023, or a vector with some combination thereof}

\item{language}{A \code{string} that indicates in which language the data will be returned. Portuguese ("pt") and English ("eng") are supported.}
}
\value{
A \code{tibble}.
}
\description{
Loads information on the social and environmental performance of the Legal Amazon.
}
\examples{
\dontrun{
# Download raw data from 2014
data <- load_ips(dataset = "all", raw_data = TRUE, time_period = 2014)

# Download treated deforest data from 2018 in portuguese
data <- load_ips(
  dataset = "deforest", raw_data = FALSE,
  time_period = 2018, language = "pt"
)
}

}
