#!/bin/sh
## http://stackoverflow.com/a/4523805/673826
##
## Must be run from the parent of the package directory (no options
## to change target of check or tarball!?!)

PACKAGE="swat2r"
VERSION=$(awk -F": +" '/^Version/ { print $2 }' ${PACKAGE}/DESCRIPTION)

PATH=${PATH}:/c/Programs/R/bin

rm -rf ${PACKAGE}.out
R --no-restore --slave <<EOR
  library(roxygen2)
  roxygenize(package.dir="${PACKAGE}",
             roxygen.dir="${PACKAGE}.out")
EOR

R CMD build ${PACKAGE}.out

rm -rf ${PACKAGE}.Rcheck
R CMD check ${PACKAGE}_${VERSION}.tar.gz
R CMD INSTALL ${PACKAGE}_${VERSION}.tar.gz
