\name{plotHive}
\alias{plotHive}
\alias{plot3dHive}
\title{
Create a hive plot (2D or 3D)
}
\description{
These functions plot a \code{HivePlotData} object in either 2D or 3D, depending upon which function is called.
}
\usage{
plotHive(HPD, dr.nodes = TRUE, method = "abs", ...)
plot3dHive(HPD, dr.nodes = TRUE, method = "abs", ...)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{\link{HivePlotData}}.
}
  \item{dr.nodes}{
Logical; if \code{TRUE} nodes will be drawn.
}
  \item{method}{
Character.  One of \code{c("abs", "norm", "rank")}.  If \code{"norm" or "rank"} this info passed to \code{\link{manipAxis}}.  For \code{"norm"} the node radii are normalized to the range 0\ldots1, so that each axis has the same length.  If \code{"rank"}, the node radii are replaced by their rank using \code{ties.method = "first"}.
}
  \item{\dots}{
Additional parameters to be passed downstream.
}
}
\details{
\code{plotHive} uses \code{grid} graphics to produce a 2D hive plot in a style similar to the original concept.  \code{plot3dHive} produces a 3D hive plot using \code{\link{rgl}} graphics.
}
\value{
None.  Side effect is a plot.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
test3 <- ranHiveData(nx = 3)
test4 <- ranHiveData(nx = 4)
plotHive(test3)
# The following must be run interactively:
\dontrun{
plot3dHive(test4)
}
}
\keyword{ plot }
\keyword{ hplot }
\keyword{ dynamic }
