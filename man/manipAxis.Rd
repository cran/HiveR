\name{manipAxis}
\alias{manipAxis}
\title{
Modify the display of axes and nodes in a hive plot
}
\description{
This function modifies the node positions (radii) along each axis in a \code{HivePlotData} object.  A typical use is to convert the radii from the native/absolute values in the original object to either a normalized value (0\ldots1) or to a ranked value.  Other modifications, such as scaling one axis relative to the others, can be added relatively easily.
}
\usage{
manipAxis(HPD, method)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{HivePlotData}. }
  \item{method}{
One of \code{c("rank", "norm")} giving the type of modification to be made.
}
}
\details{
The rank method uses \code{ties.method = "first"} so that each node gets a unique radius.
}
\value{
A modified \code{HivePlotData} object.
}
\references{
\url{http://academic.depauw.edu/~hanson/HiveR/HiveR.html}
}
\author{
Bryan A. Hanson, DePauw University. \email{hanson@depauw.edu}
}
\examples{
set.seed(55)
test3 <- ranHiveData(nx = 3)
plotHive(test3)
plotHive(test3, method = "rank")
plotHive(test3, method = "norm")
}
\keyword{ utilities }
