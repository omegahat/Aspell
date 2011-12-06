\name{getWordList}
\alias{getWordList}
\title{Retrieve the collections of words stored by the speller.}
\description{
  The speller maintains 3 different collections of
  words that it uses when performing spell checks
  and making suggestions  for mis-spelled words.
  These three word lists are termed personal, session
  and main.  This function allows one to access
  any of these for the given speller.
  
}
\usage{
getWordList(speller = getSpeller(), which = names(WordListEnum))
}
\arguments{
  \item{speller}{the speller whose lists of words are to be queried.}
  \item{which}{the name of the word list to be retrieved.
    Users should specify the entry by name rather than integer
    value for clarity in their code. See \code{Aspell:::WordListEnum}}
}

\value{
  If one word list is requested, this returns
  a character vector.
  If more than one word list is requested, a named list is returned
  giving the character vectors for each requested word list.
  If the list is not accessible, we returned \code{NULL}.
}

\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{getSpeller}}
  \code{\link{addToList}}
  \code{\link{saveWordLists}}
}
\examples{

}
\keyword{IO}
\concept{text manipulation}


