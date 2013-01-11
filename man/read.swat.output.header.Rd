\name{read.swat.output.header}
\alias{read.swat.output.header}
\title{Parse common SWAT output header}
\usage{
  read.swat.output.header(con)
}
\arguments{
  \item{con}{a (file) connection opened by
  \code{\link{read.swat.output}}}
}
\value{
  This function returns a list \item{date}{SWAT executable
  build date} \item{version}{major version of SWAT}
  \item{rev}{numeric revision of SWAT code that produced
  output}
}
\description{
  Internally used function to parse standard SWAT output
  header.
}
\details{
  Returns version of SWAT that produced a file in question
  among other things.
}
\author{
  Mikhail Titov \email{mlt@gmx.us}
}

