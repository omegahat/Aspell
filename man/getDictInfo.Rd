\name{getDictInfo}
\alias{getDictInfo}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get information about the available dictionaries.}
\description{
  This function queries the specified configuration
  and retrieves information about the different
  dictionaries that are available to it.
}
\usage{
getDictInfo(conf = createSpellConfig(), el = createDictInfo)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conf}{the configuration object whose dictionaries are to be queried}
  \item{el}{a function that converts the dictionary information into an
    R object.  See \code{Aspell:::createDictInfo}.
  }
}
\value{
  A named list with entries created via the function
  given by \code{el}.  By default, these are
  objects of class \code{DictInfo}.
}
\references{
  For a list of available dictionaries,
  see \url{http://aspell.sourceforge.net/man-html/Supported.html}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 \code{\link{setSpellConfig}}
}
\examples{
  d = getDictInfo()
  d[[1]]
  names(d)
  sapply(d, function(x) x@code)
}
\keyword{IO}

