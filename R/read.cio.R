##' Read main SWAT input file \file{file.cio}
##'
##' The list returned can be supplied to the number of functions like
##' \code{\link{read.sub}} to read \file{output.sub} and alike
##' @title Read Master Watershed File
##' @param name path to \file{file.cio}
##' @return Function returns a \code{list} with important variables
##' read from \file{file.cio}. This list can be used as an input to
##' \code{\link{read.swat.output}} and alike.
##' @importFrom stringr str_match_all
##' @export
read.cio <-
    function (name = "file.cio")
{
    cio.lines <- readLines(name)

    vars <- c("NBYR", "IYR", "IDAF", "IPRINT", "NYSKIP", "ILOG") # add more
    vars.re <- sprintf("(\\d+)\\s+[|]\\s+(%s)", paste(vars, collapse="|"))
    m <- stringr::str_match_all(cio.lines, vars.re)

    mm <- m[sapply(m, length) > 0]
    out <- sapply(mm, function(x) {
        l <- list()
        l[ x[3] ] <- as.numeric(x[2])
        l
        })

    steps <- c("monthly", "daily", "annual")
    within(out, {
        from <- as.Date(paste(IYR + NYSKIP, IDAF), "%Y %j")
        tstep <- factor(steps[IPRINT+1], levels=steps)
        dir <- dirname(name)
    })
}

utils::globalVariables(c("IYR", "NYSKIP", "IDAF", "IPRINT"))
