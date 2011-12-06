\name{docChecker}
\alias{docChecker}
\alias{Next}
\alias{process}
\alias{reset}
\title{Collection of functions to spell check a document.}
\description{
  This is a collection of functions that provide an interface
  to spell checking an entire document.
  One first creates a document checker via \code{docChecker}.
  The \code{reset} ensures that the checker is ready to start
  on a new document.
  \code{process} starts the checking of a new document.
  The function \code{Next} is used after the processing
  is started to get the \dQuote{next} mis-spelled word.
}
\usage{
docChecker(speller = getSpeller(), class = "AspellDocChecker")
reset(checker)
Next(checker)
process(checker, text) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speller}{the sp}
  \item{class}{the name of the class that is used to create the document
  spell checker. This is used to allow constructors for derived classes
  to use this function and for tose to specify the desired class.}
  \item{checker}{an object of class \code{\link{AspellDocChecker-class}}
    that provides the streaming through the text.}
  \item{text}{a character vector giving the text whose words are to be spell-checked.}
}
\value{
  These functions are more  of interest for their side-effects.
  \code{Next} returns a token giving the location in the text of the
  documentation of the next mis-spelled word.
}

\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
\code{\link{aspell}}
}
\examples{

}
\keyword{IO}
\concept{text manipulation}
