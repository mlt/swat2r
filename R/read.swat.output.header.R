##' Internally used function to parse standard SWAT output header.
##'
##' Returns version of SWAT that produced a file in question among
##' other things.
##' @title Parse common SWAT output header
##' @param con a (file) connection opened by
##' \code{\link{read.swat.output}}
##' @return This function returns a list
##' \item{date}{SWAT executable build date}
##' \item{version}{major version of SWAT}
##' \item{rev}{numeric revision of SWAT code that produced output}
##' @author Mikhail Titov \email{mlt@@gmx.us}
read.swat.output.header <-
    function (con)
{
    header <- readLines(con, 8)
    pattern <- "\\s+SWAT (\\w+ \\d+ \\d+)\\s+VER (\\d+)/Rev[^0-9]+(\\d+)"
    ver.matrix <- stringr::str_match(header[2], pattern)
    if (!is.na(ver.matrix[1,1]))
        return (
        list(date = as.Date(ver.matrix[2], "%b %d %Y"),
             version = as.numeric(ver.matrix[3]),
             rev = as.numeric(ver.matrix[4]))
        )
    ## SWAT 2005?
    pattern <- "\\s+SWAT\\s+(\\w+) '(\\d+)\\s+VERSION(\\d+)"
    ver.matrix <- stringr::str_match(header[2], pattern)
    mnths <- list(Sept = "September")
    if (!is.na(ver.matrix[1,1]))
        return (
        list(date = as.Date(paste(1, mnths[[ver.matrix[1,2]]], ver.matrix[1,3]), "%d %b %y"),
             version = as.numeric(ver.matrix[4]),
             rev = 481)         # Change if formats are much different
        )
    stop("Unknown SWAT revision")
}
