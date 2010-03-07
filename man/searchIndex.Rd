\name{searchIndex}
\alias{searchIndex}
\alias{"[.indexed_db"}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Search Indexed Data for Value
}
\description{
Underlying tool to find matching values based on
boolean algebra criteria.
}
\usage{
searchIndex(column, 
            x,
            type = "=",
            SIZE = 1e+05,
            env = .IndexEnv,
            range = FALSE)
}
\arguments{
  \item{column}{
The column name as a character vector of length one.
}
  \item{x}{
The value to apply boolean algebra \code{type} to.
This value is expected to be of length one (scalar), but
if of length two will be interpretted as the beginning and
end of a range to search.  Range queries are handled
differently to achieve disk-io and search efficiency.
}
  \item{type}{
Boolean algebra operator to apply to the column
and x value.  This can be one of \sQuote{"=="}
\sQuote{"=="},
\sQuote{"="},
\sQuote{"!="},
\sQuote{"<="},
\sQuote{">="}.
\sQuote{">"},
\sQuote{"<"}.
These correspond to their expected behavior.
In the case that \code{x} is of length two (a range query),
this is automatically converted to \code{c(">=","<=")}. This
translates to the endpoints of the range being included
in the search.  If this isn't desired, one can specify
the required enpoint handling to override.
}
  \item{SIZE}{
Used to initially subset the object into
a sparse representation in-memory.  This is then used
with the binary search algorithm to find the appropriate
dense subset index to perform a secondary search on. The
rationale is to make on-disk objects (mmap) behave
as standard R objects, to alleviate specific case handling.
}
  \item{env}{
The environment of the data objects (internal indexing database)
}
  \item{range}{
Used internally. Not to be set by the caller.
}
}
\details{
This is the internal workhorse function for all index
queries.  It operates in approximately O(log(n)) time,
with the final cost being O(n) with the length of the resulting
matches.

At present the code makes use of \pkg{mmap} for out-of-core
data storage. This allows for effectivly unlimited sized vectors
to be searched without the overhead of loading into memory.
}
\value{
A vector of positions that match the criteria specified. This
is classed \sQuote{rowid} for further processing.

In future releases this may optionally be a bitmap index
to speed up boolean AND/OR operations.
}
\references{
mmap, indexing wikipedia?
}
\author{
Jeffrey A. Ryan
}
\note{
While this function is exposed to the user, querying the
index is more direct and \R-like from the \sQuote{"[.indexed_db"}
subsetting mechanism which allows for better expression and
complex evaluation.
}

\section{Warning }{
This function, interface, and behavior is very \sQuote{alpha}.
User beware!
}

\seealso{
%%\code{\link{help}}
}
\examples{
pwd <- getwd()
setwd(tempdir())
a <- sample(100L, 1000, TRUE)
createIndex(a)
searchIndex('a', 33)
a[searchIndex('a', 33)]
rm(a)
setwd(pwd)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ manip }
