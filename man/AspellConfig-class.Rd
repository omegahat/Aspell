\name{AspellConfig-class}
\docType{class}
\alias{AspellConfig-class}
\alias{AspellSpeller-class}
\alias{AspellDocChecker-class}
\alias{[,AspellConfig-method}
\alias{$,AspellSpeller-method}
\alias{$<-,AspellSpeller-method}

\title{Classes for representing opaque data types in the aspell library.}
\description{

  These classes are used to provide R objects that refer to C-level data
  structures in the native aspell library.  These provide access to the
  configuration facilities for aspell, the word speller itself and the
  higher-level document speller.

  The \code{AspellConfig} class provides syntactic sugar
  for accessing options in the underlying configuration
  object.  One can use the \code{\$} and \code{[} operators
  to retrieve the value of one (\code{\$}) or several (\code{[})
  named elements in a single call. This treats the configuration
  object like a list or vector.
  One can also assign a value to an option using
  \code{ conf$option <- value}.

  The \code{AspellSpeller} also provides syntactic sugar
  for accessing properties and functionality of the speller.
  The \code{\$} operator allows one to setup a call to spell
  check one or more words, e.g.
  \code{sp$spell("duncan")}.
  Similarly, one can also retrieve the \code{AspellConfig}
  object associated with a speller via \code{sp$config}.
  Also, one can get the word lists associated with a speller
  using \code{sp$session}, \code{sp$personal} and \code{sp$main}.
  And finally, one can save the word lists associated with the speller
  using \code{sp$save()}.

  One can also add words to the acceptably spelled words and
  replacement/suggestion words for teaching the speller for the future.
  \code{sp$session = c("duncan", "Aspell")} and \code{sp$personal =
    c("duncan", "SJava")} are examples of assigning new words to
  existing word lists.
  Note that this appends to that list.
  To add to the replacement list, use \code{sp$correct <-
  c(missplledword = "correct version", ....)}
  
}
\section{Objects from the Class}{
  Objects can be constructed via
  \code{\link{createSpellConfig}},
  \code{\link{getSpeller}},
  and   \code{\link{docChecker}}.
}
\section{Slots}{
  \describe{
    \item{\code{ref}:}{Object of class \code{"externalptr"}.
      Each class is merely a reference to a C-level data structure.
      The C-code provides a way to validate the pointer type.}
  }
}
\section{Methods}{
  \describe{
  \item{\$}{these operators provide syntactic convenience for using the
    configuration and speller objects.}
  }
}

\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{createSpellConfig}},
  \code{\link{getSpeller}},
  \code{\link{docChecker}}  
}
\examples{

 sp = getSpeller()

 sp$config

 sp$personal
 sp$session

 sp$session <- c("Omegahat", "AspellConfig")

 sp$save()
 sp$spell("Omegahat")

 sp$correct  <- c("duncn" = "duncan")
}
\keyword{classes}
