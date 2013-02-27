##' These functions read \file{output.xxx} files produced by various
##' revisions of SWAT.
##'
##' SWAT revision is being determined from standard header (except for
##' \file{output.sed}). Based on revision and output result requested,
##' columns to be read are determined. Column names are read from
##' corresponding SWAT output file with column names adjusted to be
##' syntactically valid R names,e.g., \code{#} and \code{/} are
##' converted to \code{.}.
##'
##' \code{data.frame} read from SWAT output file is split into
##' \emph{daily}/\emph{monthly} (if any), \emph{annual}, and
##' simulation \emph{totals} (if any) \code{data.frame}s. Based on
##' dates of simulation (as in \file{file.cio}), extra column
##' \emph{DAY} with sequential time steps is appended for ease of
##' plotting and further processing. Time step is chosen based on
##' \emph{IPRINT} variable in \file{file.cio}. See reference for
##' details.
##'
##' \emph{MON} column in \emph{annual} \code{data.frame} is renamed to
##' \emph{YEAR}. \emph{MON} is set to actual month for \emph{daily}
##' results instead of day of a year.
##'
##' There are convenience wrapper functions
##' available that take care of \code{what} and \code{header}.
##' @title Read SWAT output
##' @return \code{read.swat.output} function (and all wrappers besides
##' \pkg{SWAT2R} related) returns a \code{list} of \code{data.frame}
##' objects with the following elements
##'
##' \item{daily}{not NULL if \code{IPRINT==1} in \file{file.cio}}
##' \item{monthly}{not NULL if \code{IPRINT==0} in \file{file.cio}}
##' \item{annual}{annual totals}
##' \item{totals}{simulation totals, except for \file{output.rsv}}
##' @param ... other parameters passed further to corresponding
##' functions.
##' @author Mikhail Titov \email{mlt@@gmx.us}
##' @references J. G. Arnold, J. R. Kiniry, R. Srinivasan,
##' J. R. Williams, E. B. Haney, and S. L. Neitsch, "Soil and Water
##' Assessment Tool Input/Output File Documentation Version 2009,"
##' Texas Water Resources Institute, Technical Report TR-365, 2011.
##' @examples
##' \dontrun{
##' library(SWAT2R)
##' library(ggplot2)
##' # read packaged main input file from SWAT 2009 model
##' cio.name <- system.file("swat2009-rev488/file.cio", package="SWAT2R")
##' cio <- read.cio(cio.name)
##' # read compressed output.rch
##' rch.name <- system.file("swat2009-rev488/output.rch.bz2", package="SWAT2R")
##' rch.con <- bzfile(rch.name)
##' rch <- read.rch(cio, file=rch.con)
##' # plot sediment delivered to sub-basin outlets
##' ggplot(subset(rch$annual, RCH %in% c(3,6)),
##'     aes(y=SED_OUTtons, x=as.factor(RCH))) +
##'     geom_boxplot()
##'
##' # read packaged main input file from SWAT 2012 model
##' cio2.name <- system.file("swat2012-rev535/file.cio", package="SWAT2R")
##' cio2 <- read.cio(cio2.name)
##' # read compressed output.hru
##' hru.name <- system.file("swat2012-rev535/output.hru.bz2", package="SWAT2R")
##' hru.con <- bzfile(hru.name)
##' hru <- read.hru(cio2, file=hru.con)
##'
##' # For the sake of details visualization, let's plot
##' # - simulation-average monthly
##' #   - surface runoff
##' #   - and tile flow
##' # - in sub-basin 7
##' # - for all land uses
##' # - between March and October
##' # - of 2006-2008.
##'
##' # massage data
##' library(reshape)
##' hru2 <- within( subset(hru$monthly, SUB == 7 &
##'                        2006 <= YEAR & YEAR <= 2008 & 3 <= MON & MON <= 10,
##'                        select=c(LULC, SURQ_CNTmm, QTILEmm, MON, YEAR)), {
##'                mon.name <- factor(month.name[MON], month.name)
##'            })
##' hru2.long <- melt(hru2, c("YEAR", "MON", "mon.name", "LULC"))
##'
##' # plot
##' ggplot(hru2.long, aes(y=value, colour=variable, x=LULC)) +
##'     geom_boxplot() +
##'     facet_wrap(~ mon.name) +
##'     labs(y="mm") +
##'     theme_bw() %+replace% theme(legend.position=c(.9,.1))
##' }
##' @export
read.swat.output <- function(...) UseMethod("read.swat.output")

##' @param cio a list as returned by \code{\link{read.cio}}
##' @param what one of the \code{c("hru", "rch", "rsv", "sed",
##' "sub")}
##' @param header Whether standard SWAT header with version
##' information presents. \file{output.sed} is available only from
##' SWAT 2009+ . In revision 488, it has no header. Call as
##' \code{read.swat.output(cio, "sed", FALSE)} for now.
##' @param file overrides default file name. Can be a
##' \code{\link{connection}} to read from archives.
##' @param groups number of FORTRAN IO format groups to proceed. E.g.,
##' for output.hru from rev 488, \code{group=4} would imply
##' \code{c("A4", "I4", "A9", "3F5")}. See source code for
##' details. Can also be a character as \var{out.type} for
##' \code{\link{read_sub}} and alike.
##' @note SWAT version that produced a file being read is detected
##' from file's header at this moment. For \file{output.sed}, there is
##' no standard SWAT output header at least for SWAT 2009
##' Rev.488. Perhaps in the future version detection will migrate
##' elsewhere and version will be an element of \code{cio} list.
##'
##' More work needs to be done on fortran IO groups.
##' @rdname read.swat.output
##' @importFrom stringr str_trim
##' @method read.swat.output list
##' @export
read.swat.output.list <-
    function (cio, what, header=TRUE, file, groups=NA, ...)
{
    if (missing(file))
        file <- sprintf("%s/output.%s", cio$dir, what)
    if (is.character(file)) {
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if (!isOpen(file, "rt")) {
        open(file, "rt")
        on.exit(close(file))
    }

    ver <- if(header) {   # can't use ifelse since dealing with a list
        read.swat.output.header(file)
    } else {
        list(rev=481)
    }
    if (ver$rev < 481)
        stop("Revisions below 481 are not supported yet. Consider read_xxx functions.")

    ## if at some point it will change, migrate it to fmt
    what.col <- switch(what,
                       "hru" = "HRU",
                       "rch" = "RCH",
                       "rsv" = "RES",
                       "sed" = "RCH",
                       "sub" = "SUB"
                       )
    formats <- list(
               hru = list(
               "481" = list(
               names = c("A4", "A4", "A9", "3A5", "24A10", "43A10", "2A10", "5A10"),
               data  = c("A4", "I4", "A9", "3F5", "24F10", "43A10", "2F11", "5F10"),
               groups = c(Q=5)),
               "535" = list( # only 3 extra columns for tile flow
               names = c("A4", "A5", "X", "A9", "3A5", "67A10", "2A10", "8A10"),
               data = c("A4", "I5", "X", "A9", "3F5", "67F10", "2F11", "8F10"),
               groups = c(Q=5))
               ),
               rch = list(
               "481" = list(
               names = c("6X", "A4", "A9", "A6", "5A12", "38A12"),
               data = c("6X", "I4", "I9", "F6", "5F12", "38F12"),
               groups = c(Q=5))
               ),
               rsv = list(
               "481" = list(
               names = c("6X","A8", "A5", "41A12"),
               data = c("6X", "I8", "F5", "41F12"))
               ),
               sed = list(
               "481" = list(
               names = c("6X", "A4", "A9", "A6", "20A12"),
               data = c("6X", "I4", "A9", "F6", "20F12"))
               ),
               sub = list(
               "481" = list(
               names = c("6X","A4", "A9", "A5", "10A10", "9A10", "A10", "2A10"),
               data = c("6X","I4", "I9", "F5", "10F10", "9F10", "F11", "2F10"),
               groups = c(Q=5)),
               "510" = list(
               names = c("6X","A4", "A9", "A5", "10A10", "9A10", "A10", "3A10"),
               data = c("6X","I4", "I9", "F5", "10F10", "9F10", "F11", "3F10"),
               groups = c(Q=5))
               )
               )

    what.fmt <- formats[[what]]
    fmt <- what.fmt[[ findInterval(ver$rev, as.numeric(names(what.fmt))) ]]
    if (!missing(groups)) {
        if (is.character(groups))
            groups <- fmt$groups[[groups]]
        fmt <- lapply(fmt, head, groups)
    }

    col.names <- stringr::str_trim(as.character(
                                   read.fortran(file, fmt$names, comment.char="", n=1)
                                   ))
    colClasses <- if("GIS" %in% col.names) c(GIS='factor') else NA
    x <- read.fortran(file, fmt$data, col.names = col.names, comment.char="",
                      as.is=FALSE, colClasses=colClasses)
    class(x) <- c("data.frame", "swat.output", what)
    ## close(file)
    ## It is likely the case for all output.rsv no matter of revision.
    ## If something else needs to be tweaked, it shall go to fmt list.
    has.totals <- ifelse("rsv" == what, FALSE, TRUE)
    ## FIXME: bad naming?
    seq(x, cio, max(x[, what.col]), has.totals)
}

##' @details \code{read.hru} reads \file{output.hru}.
##' @rdname read.swat.output
##' @export
read.hru <- function(...) UseMethod("read.hru")

##' @details \code{read.hru.default} is a wrapper for
##' \code{\link{read_hru}}.
##' @seealso \code{\link{read_hru}}
##' @rdname read.swat.output
##' @method read.hru default
##' @export
read.hru.default <- function(...) read_hru(...)

##' @details \code{read.hru.list} is a wrapper around \code{read.swat.output}
##' to read \file{output.hru}.
##' @rdname read.swat.output
##' @method read.hru list
##' @export
read.hru.list <- function(cio, groups, ...)
    read.swat.output(cio, what="hru", groups=groups, ...)


##' @details \code{read.rch} reads \file{output.rch}.
##' @rdname read.swat.output
##' @export
read.rch <- function(...) UseMethod("read.rch")

##' @details \code{read.rch.default} is a wrapper for
##' \code{\link{read_rch}}.
##' @seealso \code{\link{read_rch}}
##' @rdname read.swat.output
##' @method read.rch default
##' @export
read.rch.default <- function(...) read_rch(...)

##' @details \code{read.rch.list} is a wrapper around \code{read.swat.output}
##' to read \file{output.rch}.
##' @rdname read.swat.output
##' @method read.rch list
##' @export
read.rch.list <- function(cio, groups, ...) read.swat.output(cio, "rch", groups=groups, ...)


##' @details \code{read.rsv} reads \file{output.rsv}.
##' @rdname read.swat.output
##' @export
read.rsv <- function(...) UseMethod("read.rsv")

##' @details \code{read.rsv.default} is a wrapper for
##' \code{\link{read_rsv}}.
##' @seealso \code{\link{read_rsv}}
##' @rdname read.swat.output
##' @method read.rsv default
##' @export
read.rsv.default <- function(...) read_rsv(...)

##' @details \code{read.rsv.list} is a wrapper around \code{read.swat.output}
##' to read \file{output.rsv}.
##' @rdname read.swat.output
##' @method read.rsv list
##' @export
read.rsv.list <- function(cio, groups, ...)
    read.swat.output(cio, what="rsv", groups=groups, ...)


##' @details \code{read.sed} reads \file{output.sed}.
##' @rdname read.swat.output
##' @export
read.sed <- function(...) UseMethod("read.sed")

##' @details \code{read.sed.default} is a wrapper for
##' \code{\link{read_sed}}.
##' @seealso \code{\link{read_sed}}
##' @rdname read.swat.output
##' @method read.sed default
##' @export
read.sed.default <- function(...) read_sed(...)

##' @details \code{read.sed.list} is a wrapper around \code{read.swat.output}
##' to read \file{output.sed}.
##' @rdname read.swat.output
##' @method read.sed list
##' @export
read.sed.list <- function(cio, groups, header=FALSE, ...)
    read.swat.output(cio, what="sed", header=header, groups=groups, ...)


##' @details \code{read.sub} reads \file{output.sub}.
##' @rdname read.swat.output
##' @export
read.sub <- function(...) UseMethod("read.sub")

##' @details \code{read.sub.default} is a wrapper for
##' \code{\link{read_sub}}.
##' @seealso \code{\link{read_sub}}
##' @rdname read.swat.output
##' @method read.sub default
##' @export
read.sub.default <- function(...) read_sub(...)

##' @details \code{read.sub.list} is a wrapper around \code{read.swat.output}
##' to read \file{output.sub}.
##' @rdname read.swat.output
##' @method read.sub list
##' @export
read.sub.list <- function(cio, groups, ...)
    read.swat.output(cio, what="sub", groups=groups, ...)
