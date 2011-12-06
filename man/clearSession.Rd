\name{clearSession}
\alias{clearSession}
\title{Clear the details from the current spelling session}
\description{
  This function clears the specified speller instance
  of all information it has collected during the
  (spelling) session and make it is behave as if
  it were a new speller.
  
  One can also create a new speller. The advantage of
  clearing it is that the options are still present.
}
\usage{
clearSession(speller)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speller}{the \code{\link{AspellSpeller-class}} object which is to be cleared.}
}
\value{
  An integer value indicating whether the operation was successful
   (non-zero) or not (0).
 }

 \references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{getSpeller}}  
 \code{\link{AspellSpeller-class}}
}
\examples{
  sp = getSpeller()
  aspell("duncan", FALSE, speller = sp)
  addToList("duncan", speller = sp)

   # Should be treated as spelled correctly now.
  aspell("duncan", FALSE, speller = sp)

    # Will throw error if a WritableDict since method is unimplemented.
  try(clearSession(sp))

  # Should be mis-spelled again.
  aspell("duncan", FALSE, speller = sp)

}
\keyword{IO}
\keyword{programming}
\concept{text manipulation}


