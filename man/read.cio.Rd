\name{read.cio}
\alias{read.cio}
\title{Read Master Watershed File}
\usage{
  read.cio(name = "file.cio")
}
\arguments{
  \item{name}{path to \file{file.cio}}
}
\value{
  Function returns a \code{list} with important variables
  read from \file{file.cio}. This list can be used as an
  input to \code{\link{read.swat.output}} and alike.
}
\description{
  Read main SWAT input file \file{file.cio}
}
\details{
  The list returned can be supplied to the number of
  functions like \code{\link{read.sub}} to read
  \file{output.sub} and alike
}

