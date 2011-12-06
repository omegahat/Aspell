\name{aspell}
\alias{aspell}
\alias{spell}
\alias{aspell,connection-method}
\alias{aspell,ANY-method}
\title{Check the spelling of one or more words}
\description{
  This is a function that checks the spelling of one
  or more words using the aspell library.
  The simple form merely returns a logical value
  for each of the words indicating whether it is considered
  to be  spelled correctly.
  If \code{suggests} is \code{TRUE}, a collection of
  possible alternative correct words
  are returned for each word that is potentially misspelled.

  The methods for this generic function
  provide facilities for processing an entire document.
}
\usage{
aspell(words, suggests = FALSE, speller = getSpeller())
}
\arguments{
  \item{words}{a character vector giving the words as elements}
  \item{suggests}{a logical value indicating whether to return
    alternative spellings for each word, or if \code{FALSE},
    merely a logical vector indicating whether each word was misspelled
    or not.}
  \item{speller}{the speller object of class \code{\link{AspellSpeller-class}}}
}

\value{
  If \code{suggests} is true, a list whose
  elements parallel that of the initial collection of words
  being spelled. Each element in the list is a character vector
  containing suggested alternative spellings for the given word.
  A list is returned regardless of whether the word was actually
  spelled correctly. If it is correctly spelled, it will be the first
  word in the list (?).

  If \code{suggets} is \code{FALSE}, a logical vector
  indicating whether the corresponding element of \code{words}
  was spelled correctly (\code{TRUE}) or not (\code{FALSE}).
}

\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org/Aspell}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{getSpeller}}
}
\examples{

  aspell("foo", FALSE)

  aspell(c("coloer", "foo"), FALSE)

  # Get alternatives back.
  aspell(c("coloer", "foo"))

}
\keyword{IO}
