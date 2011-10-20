\name{sumHPD}
\alias{sumHPD}
\title{
Summarize a hive plot data object
}
\description{
This function summarizes a \code{\link{HivePlotData}} object in a convenient form.
}
\usage{
sumHPD(HPD, plot.list = FALSE, tex = FALSE)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{HivePlotData}.}
  \item{plot.list}{
Logical; should a list of plotting tasks be created?
}
  \item{tex}{
Logical; should the \code{plot.list} be formatted for LaTeX?
}
}
\value{
If \code{plot.list = TRUE}, a data frame containing a list of the edges to be draw in a format suitable for troubleshooting a plot.  If \code{tex = TRUE} as well, the data frame will be in a format suitable for pasting into a LaTeX document.  The data frame will contain rows describing each edge to be drawn with the following columns: node 1 id, node 1 axis, node 1 label, node 1 radius, then the same info for node 2, then the edge weight and the edge color.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
set.seed(55)
test <- ranHiveData(nx = 4, ne = 5, desc = "Tiny 4D data set")
out <- sumHPD(test, plot.list = TRUE)
print(out)
}
\keyword{ utilities }
