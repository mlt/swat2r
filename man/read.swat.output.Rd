\name{read.swat.output}
\alias{read.hru}
\alias{read.hru.default}
\alias{read.hru.list}
\alias{read.rch}
\alias{read.rch.default}
\alias{read.rch.list}
\alias{read.rsv}
\alias{read.rsv.default}
\alias{read.rsv.list}
\alias{read.sed}
\alias{read.sed.default}
\alias{read.sed.list}
\alias{read.sub}
\alias{read.sub.default}
\alias{read.sub.list}
\alias{read.swat.output}
\alias{read.swat.output.list}
\title{Read SWAT output}
\usage{
  read.swat.output(...)

  \method{read.swat.output}{list} (cio, what,
    header = TRUE, file, groups = NA, ...)

  read.hru(...)

  \method{read.hru}{default} (...)

  \method{read.hru}{list} (cio, groups, ...)

  read.rch(...)

  \method{read.rch}{default} (...)

  \method{read.rch}{list} (cio, groups, ...)

  read.rsv(...)

  \method{read.rsv}{default} (...)

  \method{read.rsv}{list} (cio, groups, ...)

  read.sed(...)

  \method{read.sed}{default} (...)

  \method{read.sed}{list} (cio, groups, header = FALSE,
    ...)

  read.sub(...)

  \method{read.sub}{default} (...)

  \method{read.sub}{list} (cio, groups, ...)
}
\arguments{
  \item{...}{other parameters passed further to
  corresponding functions.}

  \item{cio}{a list as returned by \code{\link{read.cio}}}

  \item{what}{one of the \code{c("hru", "rch", "rsv",
  "sed", "sub")}}

  \item{header}{Whether standard SWAT header with version
  information presents. \file{output.sed} is available only
  from SWAT 2009+ . In revision 488, it has no header. Call
  as \code{read.swat.output(cio, "sed", FALSE)} for now.}

  \item{file}{overrides default file name. Can be a
  \code{\link{connection}} to read from archives.}

  \item{groups}{number of FORTRAN IO format groups to
  proceed. E.g., for output.hru from rev 488,
  \code{group=4} would imply \code{c("A4", "I4", "A9",
  "3F5")}. See source code for details. Can also be a
  character as \var{out.type} for \code{\link{read_sub}}
  and alike.}
}
\value{
  \code{read.swat.output} function (and all wrappers
  besides \pkg{SWAT2R} related) returns a \code{list} of
  \code{data.frame} objects with the following elements

  \item{daily}{not NULL if \code{IPRINT==1} in
  \file{file.cio}} \item{monthly}{not NULL if
  \code{IPRINT==0} in \file{file.cio}} \item{annual}{annual
  totals} \item{totals}{simulation totals, except for
  \file{output.rsv}}
}
\description{
  These functions read \file{output.xxx} files produced by
  various revisions of SWAT.
}
\details{
  SWAT revision is being determined from standard header
  (except for \file{output.sed}). Based on revision and
  output result requested, columns to be read are
  determined. Column names are read from corresponding SWAT
  output file with column names adjusted to be
  syntactically valid R names,e.g., \code{#} and \code{/}
  are converted to \code{.}.

  \code{data.frame} read from SWAT output file is split
  into \emph{daily}/\emph{monthly} (if any), \emph{annual},
  and simulation \emph{totals} (if any) \code{data.frame}s.
  Based on dates of simulation (as in \file{file.cio}),
  extra column \emph{DAY} with sequential time steps is
  appended for ease of plotting and further processing.
  Time step is chosen based on \emph{IPRINT} variable in
  \file{file.cio}. See reference for details.

  \emph{MON} column in \emph{annual} \code{data.frame} is
  renamed to \emph{YEAR}. \emph{MON} is set to actual month
  for \emph{daily} results instead of day of a year.

  There are convenience wrapper functions available that
  take care of \code{what} and \code{header}.

  \code{read.hru} reads \file{output.hru}.

  \code{read.hru.default} is a wrapper for
  \code{\link{read_hru}}.

  \code{read.hru.list} is a wrapper around
  \code{read.swat.output} to read \file{output.hru}.

  \code{read.rch} reads \file{output.rch}.

  \code{read.rch.default} is a wrapper for
  \code{\link{read_rch}}.

  \code{read.rch.list} is a wrapper around
  \code{read.swat.output} to read \file{output.rch}.

  \code{read.rsv} reads \file{output.rsv}.

  \code{read.rsv.default} is a wrapper for
  \code{\link{read_rsv}}.

  \code{read.rsv.list} is a wrapper around
  \code{read.swat.output} to read \file{output.rsv}.

  \code{read.sed} reads \file{output.sed}.

  \code{read.sed.default} is a wrapper for
  \code{\link{read_sed}}.

  \code{read.sed.list} is a wrapper around
  \code{read.swat.output} to read \file{output.sed}.

  \code{read.sub} reads \file{output.sub}.

  \code{read.sub.default} is a wrapper for
  \code{\link{read_sub}}.

  \code{read.sub.list} is a wrapper around
  \code{read.swat.output} to read \file{output.sub}.
}
\note{
  SWAT version that produced a file being read is detected
  from file's header at this moment. For \file{output.sed},
  there is no standard SWAT output header at least for SWAT
  2009 Rev.488. Perhaps in the future version detection
  will migrate elsewhere and version will be an element of
  \code{cio} list.

  More work needs to be done on fortran IO groups.
}
\examples{
\dontrun{
library(SWAT2R)
library(ggplot2)
# read packaged main input file from SWAT 2009 model
cio.name <- system.file("swat2009-rev488/file.cio", package="SWAT2R")
cio <- read.cio(cio.name)
# read compressed output.rch
rch.name <- system.file("swat2009-rev488/output.rch.bz2", package="SWAT2R")
rch.con <- bzfile(rch.name)
rch <- read.rch(cio, file=rch.con)
# plot sediment delivered to sub-basin outlets
ggplot(subset(rch$annual, RCH \%in\% c(3,6)),
    aes(y=SED_OUTtons, x=as.factor(RCH))) +
    geom_boxplot()

# read packaged main input file from SWAT 2012 model
cio2.name <- system.file("swat2012-rev535/file.cio", package="SWAT2R")
cio2 <- read.cio(cio2.name)
# read compressed output.hru
hru.name <- system.file("swat2012-rev535/output.hru.bz2", package="SWAT2R")
hru.con <- bzfile(hru.name)
hru <- read.hru(cio2, file=hru.con)

# For the sake of details visualization, let's plot
# - simulation-average monthly
#   - surface runoff
#   - and tile flow
# - in sub-basin 7
# - for all land uses
# - between March and October
# - of 2006-2008.

# massage data
library(reshape)
hru2 <- within( subset(hru$monthly, SUB == 7 &
                       2006 <= YEAR & YEAR <= 2008 & 3 <= MON & MON <= 10,
                       select=c(LULC, SURQ_CNTmm, QTILEmm, MON, YEAR)), {
               mon.name <- factor(month.name[MON], month.name)
           })
hru2.long <- melt(hru2, c("YEAR", "MON", "mon.name", "LULC"))

# plot
ggplot(hru2.long, aes(y=value, colour=variable, x=LULC)) +
    geom_boxplot() +
    facet_wrap(~ mon.name) +
    labs(y="mm") +
    theme_bw() \%+replace\% theme(legend.position=c(.9,.1))
}
}
\author{
  Mikhail Titov \email{mlt@gmx.us}
}
\references{
  J. G. Arnold, J. R. Kiniry, R. Srinivasan, J. R.
  Williams, E. B. Haney, and S. L. Neitsch, "Soil and Water
  Assessment Tool Input/Output File Documentation Version
  2009," Texas Water Resources Institute, Technical Report
  TR-365, 2011.
}
\seealso{
  \code{\link{read_hru}}

  \code{\link{read_rch}}

  \code{\link{read_rsv}}

  \code{\link{read_sed}}

  \code{\link{read_sub}}
}

