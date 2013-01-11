\name{seq.swat.output}
\alias{seq.swat.output}
\title{Split single SWAT output listing}
\usage{
  \method{seq}{swat.output} (x, cio, each,
    has.totals = TRUE, ...)
}
\arguments{
  \item{x}{a \code{"swat.output"} object, esentially a
  \code{"data.frame"} with data from \file{output.xxx}}

  \item{cio}{a list object returned by
  \code{\link{read.cio}}}

  \item{each}{a number of entities reported for a time
  unit, e.g. number of SUBbasins}

  \item{has.totals}{whether there are overall simulation
  totals. E.g., output.rsv misses those at least in SWAT
  rev. 488}

  \item{...}{ignored}
}
\description{
  This function splits data from \file{output.xxx} with
  SWAT results and generate date sequence along rows.
}
\details{
  SWAT interleaves monthly output with annual ones and has
  simulation totals at the end for some cases. This
  function splits those and generates reasomable date
  objects in \var{DAY} column for all time steps
  (\var{IPRINT}) and \var{MON} & \var{YEAR} for daily
  output.

  Annual simulation output usually contains totals, while
  daily output has neither monthly, nor annual, nor
  simulation totals.
}
\note{
  Handling of "missing" totals might be handled elsewhere
  in future versions so no \code{has.totals} would be used.
}

