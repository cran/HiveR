\name{manipAxis}
\alias{manipAxis}
\title{
Modify the display of axes and nodes in a hive plot
}
\description{
This function modifies the node positions (radii) along each axis in a \code{HivePlotData} object.  A typical use is to convert the radii from the native/absolute values in the original object to either a normalized value (0\ldots1) or to a ranked value.  Other modifications can be added relatively easily.
}
\usage{
manipAxis(HPD, method, action = NULL)
}
\arguments{
  \item{HPD}{
An object of S3 class \code{HivePlotData}. }
  \item{method}{
One of \code{c("rank", "norm", "scale", "invert", "ranknorm")} giving the type of modification to be made.
}
  \item{action}{
A numeric vector of the same length as the number of axes.  Used if \code{method = c("scale", "invert")}.  For \code{scale} node radii will be multiplied by the corresponding value in this argument.  For \code{invert}, if \code{action = -1}, the given axis will be inverted.
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
