\name{centerHole}
\alias{centerHole}
\title{
Add the center hole to a hive plot data set prior to plotting
}
\description{
Computes a number, in native units, to be added to all nodes of a \code{HivePlotData} object in order to create the center hole or gap.
}
\usage{
centerHole(HPD)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{\link{HivePlotData}}.
}
}
\details{
The length of each axis in \code{HPD} is found (in native dimensions), and the length of the shortest axis is multiplied by \code{HPD$center.hole}.  Hence the hole or gap is a fraction of the shortest axis.  This also affects the starting position of the axes.  \code{0 < HPD$center.hole <= 1} or there may be problems (untested territory).
}
\value{
A numeric value to be added to the radius of all nodes prior to plotting; creates the center hole or gap.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
test3 <- ranHiveData(nx = 3)
plotHive(test3)
test3$center.hole <- 0.05 # half the default
plotHive(test3)
}
\keyword{ utilities }
