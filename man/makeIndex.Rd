\name{makeIndex}
\alias{makeIndex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Create Query Index
}
\description{
Index an R object in memory and save to disk. This is an S3
method with methods for common types in R.
}
\usage{
makeIndex(x, column = NULL, envir = .IndexEnv, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\R object to be indexed.  Atomic types or can be a type
that has a \code{makeIndex} method.
}
  \item{column}{
Optional column name. If omitted, it is derived from the \code{x}
argument using \code{deparse(substitute(x))}.
}
  \item{envir}{
The internal database object used by indexing.
}
  \item{\dots}{
additional arguments to methods.
}
}
\details{
The index used is calculated by calling the base
\R function \code{order} on the object to be indexed. This
results in a vector of positions that would convert \code{x}
into a sorted vector. The ordered and sorted objects are then
written to disk in binary form, and are read back in using
the \pkg{mmap} package.  This last step is used to
minimize the in-core footprint, while maximizing performance.

It is possible to use all in-memory objects, though the performance
gain is negligible, and in many tests turns out to be negative.

The objects used by \pkg{indexing} are stored in the \code{envir}
specified to makeIndex. Internal to this are objects named
according to the column name derived from the object symbol, or
specified with \code{column}. This object is a list
including components for the \pkg{mmap} objects corresponding
to the original data \sQuote{d}, the sorted data \sQuote{s},
and the order of the data \sQuote{o}.  Additionally, depending
on type there may be components for run-length encoded sorted values
\sQuote{rle} and/or levels (for character data converted to factors)
\sQuote{l}.

If the function uses \pkg{mmap} internally (currently the only way),
the internall components of the \code{envir} are written to disk so
the may be subsequently read back in via \code{mmap}. This process
creates three to five files on disk.  The are named according to the following
scheme: 

Original data is saved as
 \sQuote{column_data.bin}, 
\sQuote{column_sorted.bin}, 
\sQuote{column_ordered.bin}, 
\sQuote{column_rle.bin}, 
\sQuote{column_levels.bin}.

The final environment object that is set-up or eventually modified during
subseqeunt calls to \code{makeIndex} on new columns is used by
\sQuote{searchIndex} internally to find matching data for a given
criteria in k * O(log(n)) time, where \code{k} is dependent on the
number of criteria inherent in the query.
}
\value{
Called for its side-effects of indexing an object and storing
the index and related components in an R session.
}
\references{
index wikipedia
}
\author{
Jeffrey A. Ryan
}
\note{
As with all \pkg{indexing} code, the design, interface and implementation
is subject to getting better (read: change).  Please be patient.
}

\seealso{
\code{\link{searchIndex}}
}
\examples{
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
