\name{addCorrection}
\alias{addCorrection}
\alias{addToList}
\title{Add words to spelling dictionary}
\description{
  These functions provide facilities to add words
  to the speller's dictionary.
  \code{addCorrection} specifies
  the corrected version of a \dQuote{word}
  so that it can be used as a possible suggestion
  in the future.

  \code{addToList} allows the caller to augment
  the existing dictionary, either for this session
  or in the personal dictionary.
}
\usage{
addCorrection(speller, ..., .words, .pairs)
addToList(words, speller, session = TRUE)
}

\arguments{
  \item{\dots}{a named collection of words.  The names are the mistakes
  and the associated values are the desired corrections.}
\item{.words}{a named character vector of words. This is provided for
   programmatic access to this function and mirrors the values specified
   in \dots.}
 \item{words}{a character vector whose elements are  to be added to the
   list of acceptable (correctly spelled) words.}
 \item{.pairs}{a character vector that allows the caller to specify the
   mis-spelled/correct word pairs directly as a character vector
   without having to arrange them into a named character vector of
   correct words.  This should have even length or an error will be
   generated.  The elements are incorrect followed by correct
   pairs of words.
  } 
  \item{speller}{the \code{\link{AspellSpeller-class}} object which
  represents the speller and it state.}
  \item{session}{a logical value.  \code{TRUE} indicates that the
    words should be added to the session dictionary.
    Alternatively, \code{FALSE} means that they should be stored in the
   personal dictionary.  These will not be saved for future sessions
   unless a command is explicitly given. See \code{\link{saveWordLists}}.
 }
}
\details{
  These call \code{aspell_add_to_personal}
  \code{aspell_add_to_session} and
  \code{aspell_speller_store_replacement}.
}
\value{
  An integer vector with as many elements as
  there are words.
}
\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{getSpeller}}
  \code{\link{aspell}}
}
\examples{

 sp = getSpeller()

  # The 3 calls are equivalent, but provide different
  # interfaces.
 addCorrection(sp, "duncn" = "duncan")
 addCorrection(sp, .words = c(duncn = "duncan"))
 addCorrection(sp, .pairs = c("duncn", "duncan"))
}
\keyword{IO}

