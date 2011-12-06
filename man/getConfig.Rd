\name{getConfig}
\alias{getConfig}
\title{Retrieve the configuration for the given speller}
\description{
  This function provides a way to get the configuration
  for a given speller object, of class
  \code{\link{AspellSpeller-class}}.
  This allows us to query the modules, dictionaries,
  etc. having created a speller object.
}
\usage{
getConfig(speller, class = "AspellConfig")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speller}{an object of class \code{\link{AspellSpeller-class}}}
  \item{class}{the name of the class  which will be instantiated with
    the pointer to the C-level configuration.  This allows the
    caller to specify derived classes.}
}
\value{
  An object of class \code{class} that has a reference
  to the C-level data structure.
}

\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{createSpellConfig}}
}
\examples{
}
\keyword{IO}


