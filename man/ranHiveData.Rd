\name{ranHiveData}
\alias{ranHiveData}
\title{
Generate random hive plot data
}
\description{
This function generates random data sets which can be used to make a hive plot.
}
\usage{
ranHiveData(nx = 4, nn = nx * 15, ne = nx * 15, rad = 1:100,
	ns = 1:3, ew = 1:3, nc = brewer.pal(5, "Set1"),
	ec = brewer.pal(5, "Set1"), axis.cols = brewer.pal(nx, "Set1"),
	desc = NULL, center.hole = 0.1, verbose = FALSE)
}
\arguments{
  \item{nx}{
An integer giving the number of axes to be created (\code{2 =< nx =< 6}).
}
  \item{nn}{
An integer giving the number of nodes to be created.  This is an initial number which may be reduced during clean up.  See Details.
}
  \item{ne}{
An integer giving the number of edges to be created.  This is an initial number which may be reduced during clean up.  See Details.
}
  \item{rad}{
Numeric; a range of values that will be used as node radius values (the position of the node along the axis).
}
  \item{ns}{
Numeric; a range of values that will be used as the node sizes.
}
  \item{ew}{
Numeric; a range of values that will be used as the edge weights.
}
  \item{nc}{
A vector of valid color names giving the node colors.
}
  \item{ec}{
A vector of valid color names giving the edge colors.
}
  \item{axis.cols}{
A vector of valid color names to be used to color the axes; \code{length(axis.cols) must = nx}.
}
  \item{desc}{
Character; a description of the data set.
}
  \item{center.hole}{
Numeric; used by \code{\link{centerHole}} to create the hole or gap near the origin of the hive plot.  \code{0 < center.hole <= 1}.  This is not checked and values outside this range may cause an error.
}
  \item{verbose}{
Logical; If \code{TRUE}, the generation, processing and final result is reported to the console.
}
}
\details{
After the function creates an initial set of random nodes and edges, these are cleaned up by removing the following cases (which the rest of \code{HiveR} is not intended to handle at this time): duplicated nodes, nodes that are not part of any edge, edges that begin and end on the same point, edges that begin and end on the same axis, and finally, for \code{nx = 5 or 6}, edges that begin and end on colinear axes.  Most of these don't cause an error, but produce some ugly results.

For the arguments \code{rad, ns, ew, nc} and \code{ec}, the values given are sampled randomly (with replacement) and assigned to particular nodes or edges.

Finally, a check is made (via \code{\link{chkHPD}}) to verify that the number of axes requested were actually created: for small data sets, there may not be any nodes on a particular axis.  If this is the case, a warning is issued.
}
\value{
An object of S3 class \code{\link{HivePlotData}}.
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}

\examples{
test4 <- ranHiveData(nx = 4)
str(test4)
}
\keyword{ datagen }
