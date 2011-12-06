\name{KeyInfo-class}
\docType{class}
\alias{KeyInfo-class}

\title{Representation for option in aspell configuration.}
\description{
  This class is used to represent meta-data about options within
  the aspell library's configuration mechanism.
  
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("KeyInfo", ...)}.
Typically, one does not create objects of this type directly.
Instead, these are created programmatically via a call to
\code{\link{getSpellInfo}}.
}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"} the name of
      the option. }
    \item{\code{type}:}{Object of class \code{"integer"} the type of the
      value of this option. This is a named integer.
      It will be one of the elements of 
      \code{c(string = 0, int = 1, boolean = 2, list = 3)}. 
    }
    \item{\code{def}:}{Object of class \code{"character"}. The default value.}
    \item{\code{desc}:}{Object of class \code{"character"}. A short
      description of the option. This serves as the help.}
    \item{\code{flags}:}{Object of class \code{"integer"}. Flags
      providing information about characteristics of this option.
      This includes things like whether it is hidden, modifiable, UTF8. 
    }
    \item{\code{other_data}:}{Object of class \code{"integer"}. Used
      internally by aspell (see common/config.cpp).}
  }
}
\section{Methods}{
No methods defined with class "KeyInfo" in the signature.
}

\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
 \code{\link{getSpellInfo}}
}
\examples{
 opts = getSpellInfo()

  # Get all the list elements.
 opts[sapply(opts, function(x) names(x@type)) == "list"]
}
\keyword{classes}
