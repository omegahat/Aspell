\name{saveWordLists}
\alias{saveWordLists}
\title{Save the session, personal and main word lists for the speller.}
\description{
  This function commits the updated word lists for the
  given speller that have been accumulated as the speller
  has been used.
  Users can have added new \dQuote{acceptable} words
  for the speller and different replacement words
  to further train the speller.
  This function provides the facilities to allow these changes
  to persist to other sessions.
}
\usage{
saveWordLists(speller)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speller}{the \code{\link{AspellSpeller-class}} instance
   whose word lists are to be saved.}
}

\value{
 A logical value indicating whether the word lists were saved.
}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{addToList}}
  \code{\link{addCorrection}}
}
\examples{

}
\keyword{IO}


