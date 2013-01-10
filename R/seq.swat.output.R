##' This function splits data from \file{output.xxx} with SWAT results
##' and generate date sequence along rows.
##'
##' SWAT interleaves monthly output with annual ones and has
##' simulation totals at the end for some cases. This function splits
##' those and generates reasomable date objects in \var{DAY} column
##' for all time steps (\var{IPRINT}) and \var{MON} & \var{YEAR} for
##' daily output.
##'
##' Annual simulation output usually contains totals, while daily
##' output has neither monthly, nor annual, nor simulation totals.
##' @title Split single SWAT output listing
##' @param x a \code{"swat.output"} object, esentially a
##' \code{"data.frame"} with data from \file{output.xxx}
##' @param cio a list object returned by \code{\link{read.cio}}
##' @param each a number of entities reported for a time unit,
##' e.g. number of SUBbasins
##' @param has.totals whether there are overall simulation
##' totals. E.g., output.rsv misses those at least in SWAT rev. 488
##' @param ... ignored
##' @note Handling of "missing" totals might be handled elsewhere in
##' future versions so no \code{has.totals} would be used.
##' @rdname seq.swat.output
##' @method seq swat.output
seq.swat.output <- function(x, cio, each, has.totals=TRUE, ...) {
    len <- ifelse(has.totals & cio$tstep != 'daily', each, 0)
    totals <- tail(x, len)
    x.rest <- head(x, length(x[,1]) - len)

    switch(cio$tstep,
       {
           g <- split(x.rest, ifelse(x.rest$MON<13, 'monthly', 'annual'))
           monthly <-
               within(g$monthly, {
                   DAY <- rep(seq(cio$from, by='month',
                                  length.out=nrow(g$monthly)/each),
                              each=each)
                   YEAR <- as.numeric(format(DAY, "%Y"))
               })
           names(g$annual)[names(g$annual) == 'MON'] <- 'YEAR'
           list(monthly=monthly, annual=g$annual, totals=totals)
       },
       {
           daily <-
               within(x.rest, {
                   DAY <- rep(seq(cio$from, by='day',
                                  length.out=nrow(x.rest)/each),
                              each=each)
                   YEAR <- as.numeric(format(DAY, "%Y"))
                   MON <- as.numeric(format(DAY, "%m"))
               })
           list(daily=daily)
       },
       {
           x.rest$YEAR <- x.rest$MON    # some may rely on MON, keep it
           list(annual=x.rest, totals=totals)
       }
           )
}
