\name{getSpellInfo}
\alias{getSpellInfo}
\title{Get meta-information about the Aspell configuration object}
\description{
  This returns information about the different options that are
  in the specified Aspell configuration object along
  with details about the type of the value.
  This information is useful for the end user exploring the
  aspell library and its features.  It is also used
  in the R code to interface to the library.
}
\usage{
getSpellInfo(conf = createSpellConfig(), extra = TRUE, el =
               createKeyInfo, order = TRUE)
}

\arguments{
  \item{conf}{the \code{\link{AspellConfig-class}} object whose options
  are to be queried.}
  \item{extra}{a logical value indicating whether to return information
  about the \dQuote{extra} keys in the option list.}
\item{el}{a function that takes 6 arguments corresponding
  to the details of the option meta data and should return
  an object to represent this.
  See \code{Aspell:::createKeyInfo}.}
\item{order}{a logical value indicating whether
  the elements should be sorted by name or not.
  If you are calling this to merely get the names of the
  elements, this allows you to avoid the overhead of sorting the elements.}
}
\details{
  This loops over each element in the options
  list of the \code{\link{AspellConfig-class}}
  object at the C-level and calls the
  function given by \code{el}
  to create an R object to represent it.
}
\value{
  This returns a named list in which each
  element describes the option information
  for that key.

  If \code{el} is \code{NULL}, only the names
  of the options are returned.
}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{getSpellConfig}}
}
\examples{

}
\keyword{IO}


