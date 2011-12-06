\name{SpellingReplacement-class}
\docType{class}
\alias{SpellingReplacement-class}

\title{Representation of a corrected word for a document.}
\description{
  This class is used to represent a potential change or subsitution
  in a document  for a spelling error.
  It includes the original mis-spelled word, the correct
  word that should replace it and the location within
  the document of the original word.
  The location is the offset and the length, giving the precise
  sequence of bytes that must be removed and substituted.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("SpellingReplacement", ...)}.

}
\section{Slots}{
  \describe{
    \item{\code{correction}:}{Object of class \code{"character"}. The
      correct word to replace the mis-spelled one.}
    \item{\code{word}:}{Object of class \code{"character"}. The
      original, mis-spelled word.}
    \item{\code{location}:}{Object of class \code{\link{AspellToken-class}}. The
      location for the substitution.}
  }
}
\section{Methods}{
No methods defined with class "SpellingReplacement" in the signature.
}
\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{spellDoc}}
  \code{\link{DocSpeller}}  
}
\examples{

}
\keyword{classes}
