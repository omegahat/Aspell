\name{getSpeller}
\alias{getSpeller}
\title{Create a new speller object}
\description{
  To check the spelling of a word, one needs a speller instance.
  A speller instance has information about which dictionaries
  to use, what filters to apply to eliminate words,
  what suggestsions to make for misspelled words, etc.
  Each speller instance is independent of the others
  and this allows  one to use separate spellers
  for each document or
  reuse an existing speller for multiple documents.
  This function returns an opaque reference to the
  C-level speller instance.
}
\usage{
getSpeller(conf = createSpellConfig(), class = "AspellSpeller")
}
\arguments{
  \item{conf}{the configuration object to use for this speller.
    As with spellers, one might create a single configuration
    and reuse it to create multiple spellers.}
  \item{class}{the name of the class that is instantiated with the
    external pointer inserted into its \code{ref} slot.
    This allows the caller to create different classes of objects
    easily rather than by coercion after the creation.}
}
\value{
 An object of class \code{\link{AspellSpeller-class}}.
}


\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{aspell}}
  \code{\link{getSpellConfig}}
  \code{\link{createSpellConfig}}  
}
\examples{
 speller  = getSpeller()

 words = unlist(strsplit(as.character(packageDescription("Aspell")),  "[[:space:][:punct:]]"))
 words = unique(words[words != ""])

 check = aspell(words, FALSE, speller = speller)

  # proportion of words spelled correctly
 sum(check)/length(check)

 words[!check]
}
\keyword{IO}
\keyword{programming}

