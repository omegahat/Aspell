\name{DictInfo-class}
\docType{class}
\alias{DictInfo-class}

\title{Representation of libaspell dictionary information.}
\description{
  This is a class for representing the details of a particular
  dictionary for the aspell library.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("DictInfo", ...)}.
These are typically not used directly, but are generated
via a call to \code{\link{getDictInfo}}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}. The
      human-readable short-hand name of  the dictionary.  }
    \item{\code{code}:}{Object of class \code{"character"}.
      This is the name that is used to specify the dictionary
      for the  configuration options for a speller, i.e. the value for the \code{"lang"} option.
    }
    \item{\code{jargon}:}{Object of class \code{"character"}.
        Typically the suffix for the \code{name}, i.e. the bit that
	differs from the code.}
%XXX
    \item{\code{size}:}{Object of class \code{"integer"}.  ?XXX}
    \item{\code{moduleName}:}{Object of class \code{"character"}. The
      module with which the dictionary is associated.}
  }
}
\section{Methods}{
No methods defined with class "DictInfo" in the signature.
}

\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 \code{\link{getDictInfo}}.
}
\examples{
  d = getDictInfo()
  d[[1]]
}
\keyword{classes}
