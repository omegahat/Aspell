\name{CallbackResult-class}
\docType{class}
\alias{CallbackResult-class}
\alias{Callback-class}
\alias{spellDoc,ANY,CallbackResult-method}

\title{Asynchronous callback functions}
\description{
  These classes provide a way to provide structure
  to the concept of a function that is used as
  an asynchronous callback or handler.
  The idea is that one associates a callback function
  with something that generates events.
  When an event occurs, the callback function is
  invoked.
  We use this in the Aspell package for checking the spelling
  of words in a document.  We pass the callback function
  to \code{\link{spellDoc}}.  It iterates over the content
  of the document and when it encounters an mis-spelled word,
  it invokes the callback function, providing it with
  the mis-spelled word, its location, the entire text of the document
  and the speller object responsible for performing the spell-checking.

  The \code{CallbackResult} class is an extension of the basic notion
  of a \code{Callback} function.  This is used to provide a way to
  retrieve the \dQuote{answer} when the entire processing is done.
  In this version, the \code{CallbackResult} provides information
  about which variable in the environment of the callback
  function contains the result.  This allows the iterating function
  to return the result at the end rather than requiring the caller
  to access it separately.

  We imagine that one would create derived/extended classes
  from these that validate the formal arguments of the
  specified function to match the event  type.
  Also, one might provide methods for \code{\link{getResult}}
  to obtain the result from a \code{Callback} in different
  ways than using a named variable. For instance, it
  may involve dynamic computations.
}
\note{
  These  classes are not specific to Aspell, but are here
  for the moment as there is no other good place to house
  them. The concept does arise in many places (e.g. GUIs,
  XML parsing) and we will want to move the code to a general
  package.  
}  
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("CallbackResult", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"function"}. This is the
     callback function that is to be invoked when an event occurs.}
   \item{\code{variable}:}{Object of class \code{"character"}. The name
    of the variable within the }
  }
}
\section{Extends}{
Class \code{"Callback"}, directly.
Class \code{"function"}, by class \code{"Callback"}.
Class \code{"OptionalFunction"}, by class \code{"Callback"}.
Class \code{"PossibleMethod"}, by class \code{"Callback"}.
}
\section{Methods}{
  \describe{
    \item{spellDoc}{\code{signature(doc = "ANY", wordHandler =	"CallbackResult")}: 
       check the spelling of the contents of a document and return the
       results as gathered by the \code{wordHandler} object.
      }
  }
}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}
\seealso{
  \code{\link{spellDoc}}
  \code{\link{DocSpeller}}
}
\examples{

}
\keyword{classes}
