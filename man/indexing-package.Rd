\name{indexing-package}
\alias{indexing-package}
\alias{indexing}
\docType{package}
\title{
Database Tools for R
}
\description{
The \pkg{indexing} package aims to provide advanced
database tools, such as on-demand data access, compression,
and data indexing (sorted, RLE, bitmaps) based on standard \R objects
or \R object types. The
idea is to allow for data.frame semantics for larger than
memory objects, while reducing search costs.
 The \code{indexing} package is an extension
of column-oriented database design for use with R objects, though
the underlying on-disk representation is language agnostic.
}
\details{
\tabular{ll}{
Package: \tab indexing\cr
Type: \tab Package\cr
Version: \tab 0.1-0\cr
Date: \tab 2010-05-10\cr
License: \tab GPL-3\cr
LazyLoad: \tab no\cr
}
The general design of \pkg{indexing} is that a special
\sQuote{IndexEnv} object is populated with
columns of data through either the \code{createIndex} or
\code{loadIndex} functions.  All columns are indexed
by default (i.e. sorted to allow for binary search speed
in locating records), though data-only columns are also
supported. The columns are maintained in binary representation
on disk, and a reference to them is created via a call to
\code{seqfile} in \pkg{indexing} or 
\code{mmap} from the \pkg{mmap} package.  This data
is then available on demand, but does not reside
in memory; allowing for datasets many times available RAM to
be loaded into these \pkg{indexing} environments while
preserving finite memory resources for actual data manipulation.

The newly populated environment must be thought of as a
data.frame that can't fit into memory.  It allows for very \R-like
slicing, though there are a few differences at the user-level.

The first and most obvious is that there is no way to simply
extract all the rows.  This is by design, as the primary value of 
indexing over other R solutions is to make use of data that cannot
otherwise be loaded into memory.  Secondly, while the data is organized
by column, each having the same number of rows with a distinct ordering,
there is no such relation in the horizontal direction.  That is, columns
have no notion of location.  Attempting to extract column number 3 is ambiguous
and will fail. All columns must be named.  A third difference is that
only supplying the \sQuote{i} argument will return an object of class
\code{rowid}, which will print the number of hits the particular \sQuote{i}
query matches.  If a comma is supplied, all columns will be returned in an
in-memory data.frame object.  See the examples at for more detail.

The final user-level code difference is in the semantics of searching.
The general structure is with the \sQuote{i} argument matching the row
selection criteria required (in SQL, the \sQuote{where} clause), and the
\sQuote{j} argument analogous to the SQL \sQuote{select} clause.  This is
similar to data.frame behavior with one major exception: all arguments
are evaluated within the indexed environment first. This allows for
easier interactive usage, since column names and criteria for subsetting
can be based solely on the data within the environment, or even in the .GlobalEnv.

For further details of the internals of \pkg{indexing}, as well as performance
tuning and advanced features, please see the rest of the documentation.
}
\author{
Jeffrey A. Ryan

Maintainer: <jeffrey.ryan@insightalgo.com>
}
\references{
The indexing package is inspired by previous works in R and in general
database literature, including \pkg{data.table}, \pkg{ff},
\pkg{bigmemory}, \pkg{bit} and countless others.
}
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
cwd <- getwd()
tmpdir <- tempdir()
setwd(tmpdir)
a <- rnorm(100)
b <- sample(1000L, 100)
createIndex(a)
createIndex(b)
db <- indexenv()

db

db[1]
db[a > 2]
db[a > 2, ]
db[a > 2 | a < -2, ]
db[a > 2, b]

setwd(cwd)

}
