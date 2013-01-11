\name{read.swat.cup.extract}
\alias{read.swat.cup.extract}
\title{Read series extracted by SWAT-CUP (excluding McMc)}
\usage{
  read.swat.cup.extract(cio, method = "SUFI2", file, ...)
}
\arguments{
  \item{cio}{a list returned by \code{\link{read.cio}}}

  \item{file}{file to read. Can be one of the following
  \enumerate{ \item{base file name}{, a character string
  like \verb{FLOW_OUT_1.txt}. In this case \var{method} is
  considered to build a path relative to \file{file.cio}}
  \item{full path name} \item{connection}
  \item{missing}{content of all files named in
  \file{METHOD.IN/var_names.txt} is returned as a list} }}

  \item{method}{subfolder name in SWAT-CUP project without
  "folder extension" IN/OUT, e.g., \verb{Para_Sol}}

  \item{...}{reserved}
}
\value{
  This function returns a list of data.frames with
  simulation results, or list of lists of data.frames if
  \var{file} is missing.

  cio <- read.cio("../file.cio") x <-
  read.swat.cup.extract(cio, file="FLOW_OUT_6.txt")
}
\description{
  This function reads in variables extracted by SWAT-CUP
  from various runs.
}
\details{
  Tested with version 4.x
}
\author{
  Mikhail Titov \email{mlt@gmx.us}
}

