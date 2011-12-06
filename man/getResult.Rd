\name{getResult}
\alias{getResult}
\alias{getResult,CallbackResult-method}
\title{Generic function to retrieve result from asynchronous callback handler.}
\description{
  This function is intended to be used with an asynchronous
  callback handler.  As events are generated, the callback
  handler is invoked. At the end of the event source,
  the managing function can return the results
  by invoking this function on the
  callback object.  This works for \code{\link{CallbackResult-class}}
  objects.

  This generic function can be extended to provide different
  ways to fetch/compute the result.
  The method for \code{\link{CallbackResult-class}}
  uses the information about which variable(s) to extract
  from the function's environment to find the appropriate value.
}
\usage{
getResult(obj, ...)
}
\arguments{
  \item{obj}{the callback object. In our use, this is an object of class
  \code{\link{CallbackResult-class}}.}
  \item{\dots}{any additional arguments.}
}
\value{
  An arbitrary R value.
}
\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{DocSpeller}}
  \code{\link{collectWords}}
  \code{\link{spellDoc}}
}
\examples{
}
\keyword{IO}
\concept{callback}
\concept{asynchronous events}


