\name{AspellToken-class}
\docType{class}
\alias{AspellToken-class}

\title{Representation of a location of a word in a document as offset/index and length}
\description{
  This class is used to represent the location of a word in a document.
  It gives the offset from the beginning of the string that represents
  the document and the length of the substring.
  This is the form that libaspell identifies a mis-spelled word within
  a document as it iterates over the content and identifies mis-spelled words.
}
\section{Objects from the Class}{
  This is typically not created directly, but rather as part of
  the document speller callback function.
}
\section{Slots}{
  \describe{
    \item{\code{offset}:}{Object of class \code{"integer"}. This gives
      the index of the start of the word of interest.  This is 1-based,
      i.e. starts from position 1.  This is in characters.}
    \item{\code{length}:}{Object of class \code{"integer"}.
      This gives the number of characters in the word, so
      the substring of interest is from  \code{offset} to \code{offset + length}.
    }
  }
}
\section{Methods}{
No methods defined with class "AspellToken" in the signature.
}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}
\seealso{
  \code{\link{spellDoc}}
}
\examples{

}
\keyword{classes}

