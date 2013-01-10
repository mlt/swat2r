##' This function reads in variables extracted by SWAT-CUP from
##' various runs.
##'
##' Tested with version 4.x
##' @title Read series extracted by SWAT-CUP (excluding McMc)
##' @param cio a list returned by \code{\link{read.cio}}
##' @param file file to read. Can be one of the following
##' \enumerate{
##' \item{base file name}{, a character string like
##' \verb{FLOW_OUT_1.txt}. In this case \var{method} is considered to
##' build a path relative to \file{file.cio}}
##' \item{full path name}
##' \item{connection}
##' \item{missing}{content of all files named in \file{METHOD.IN/var_names.txt} is returned as a list}
##' }
##' @param method subfolder name in SWAT-CUP project without "folder
##' extension" IN/OUT, e.g., \verb{Para_Sol}
##' @param ... reserved
##' @return This function returns a list of data.frames with
##' simulation results, or list of lists of data.frames if \var{file} is missing.
##'
##' cio <- read.cio("../file.cio")
##' x <- read.swat.cup.extract(cio, file="FLOW_OUT_6.txt")
##' @author Mikhail Titov \email{mlt@@gmx.us}
##' @export
read.swat.cup.extract <-
    function(cio, method="SUFI2", file, ...) {
    if (!missing(file)) {
        read.swat.cup.extract.file(cio, method, file, ...)
    } else {
        vars.file <- sprintf("%s/%s.IN/var_file_name.txt", cio$dir, method)
        vars <- readLines(vars.file)
        sapply(as.list(vars), function(x) {
            out <- list()
            out[[x]] <- read.swat.cup.extract.file(cio, method, x, ...)
            out
           })
    }
}

read.swat.cup.extract.file <-
    function(cio, method="SUFI2", file, ...) {
    if (is.character(file)) {
        base <- grepl("\\\\|/", file)
        if (length(base)>0)
            file <- sprintf("%s/%s.OUT/%s", cio$dir, method, file)
        file <- file(file, "rt")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("'file' must be a character string or connection")
    if (!isOpen(file, "rt")) {
        open(file, "rt")
        on.exit(close(file))
    }

    x <- read.table(file, col.names=c("doy", "value"), row.names=NULL, fill=TRUE)
    idx <- which(is.na(x[,2]))
    x.rest <- within(na.omit(x), {
        date <- cio$from + doy - 1
    })
    g <- cut(as.numeric(rownames(x.rest)), c(idx, nrow(x)), c(x[idx,1]))
    split(x.rest, g)
}

utils::globalVariables(c("doy"))
