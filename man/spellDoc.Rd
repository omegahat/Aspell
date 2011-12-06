\name{spellDoc}
\alias{spellDoc}
\alias{spellDoc,ANY,missing-method}
\alias{spellDoc,connection,ANY-method}
\alias{spellDoc,ANY,function-method}
\title{Basic interface to spell check document}
\description{
  This is a simple and primitive interface to using the
  aspell library for spell checking an entire document.
  This allows the caller to provide a function to respond
  to each mis-spelled word.  This function can attempt to predict
  the appropriate word or can prompt the user to select a word.
  These two approaches are possible using the basic mechanisms
  in R and Aspell.

  The methods for this generic function work for connections
  by \dQuote{slurping} the entire text via \code{\link[base]{readLines}}
  and passing the resulting  string into the regular \code{spellDoc}
  function.
}
\usage{
spellDoc(doc, wordHandler = DocSpeller(), checker = docChecker(speller), speller = getSpeller(conf), conf = createSpellConfig())
}
\arguments{
  \item{doc}{the identifier for  a document.  This is the name of
    a file, text giving the contents of the file or a connection object
    from which we can read the contents via \code{\link[base]{readLines}}.}
  \item{wordHandler}{a function that is called for each word that aspell
    identifies as mis-spelled.
    The function is called with the word (perhaps with surrounding
    whitespace), the token giving the offset and length of the word
    with respect ot the document,
    the text of the document itself, and the speller object
    which can be used to lookup suggestions for the mis-spelled words.
  }
  \item{checker}{an object of class \code{\link{AspellDocChecker-class}}
    which can be used across multiple documents.
    This is helpful when we cumulate predictions, personal dictionary words, etc.
     as we can use the same checker for a session.
   }
   \item{speller}{a \code{\link{AspellSpeller-class}} object that is used
     to create the \code{checker} if necessary.
     If the checker is provided in the call, this is not used.}
   \item{conf}{a \code{\link{AspellConfig-class}} that is used to create
     the speller that is used to create the document checker.
     This can be omitted and is ignored if the caller specifies the
     checker or the speller.}
}
\value{

}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{aspell}}
}
\examples{
     # Create a simple configuration
 conf = createSpellConfig()

     # Add HTML as a filter
 conf$"add-filter" = "html"

     # spell check the document TODO.html
 spellDoc(system.file("TODO.html", package = "Aspell"), function(word, tok, txt, ...) print(word), conf = conf)

}
\keyword{IO}

\concept{text manipulation}

