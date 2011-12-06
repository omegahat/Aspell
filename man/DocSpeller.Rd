\name{DocSpeller}
\alias{DocSpeller}
\alias{collectWords}
\title{Generator function for spell-checking words in a document.}
\description{
  These functions  a function that returns a \code{CallbackResult}
  for spell-checking words in a document and managing the
  mis-spellings, etc. This is used by
  \code{\link{spellDoc}}.
  The particular callback function that it returns
  is an interactive 
}
\usage{
DocSpeller(correct = TRUE, class = "CallbackResult", elClass = "SpellingReplacement")
collectWords(class = "CallbackResult")
}
\arguments{
  \item{correct}{a logical value. If \code{TRUE} this causes the
    callback function to tell the speller about the word the user
    selects as the alternative for the mis-spelled word.
    In this way, the speller can learn for future occurrences of this word.
    If this is not desirable, use \code{FALSE} and there is no feedback
    to the checker.
  }
  \item{class}{the name of the class of the returned object.  This
    allows the caller to provide the named of a derived class and still
    use this constructor function.}
  \item{elClass}{the name of the class to use when creating
      objects for describing the mis-spelled word, its location and the
      choice of alternative.}
}
\details{
 
}
\value{
  An object of class \code{class}
  which, by default,  is \code{\link{CallbackResult-class}}.
}

\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org/Aspell}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{aspell}}  
 \code{\link{spellDoc}}
 \code{\link{CallbackResult-class}} 
}
\examples{

}
\keyword{IO}
\concept{text manipulation}


