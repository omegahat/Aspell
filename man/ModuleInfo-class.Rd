\name{ModuleInfo-class}
\docType{class}
\alias{ModuleInfo-class}

\title{Representation of information about an aspell library module.}
\description{
  This class is used to provide a description of an aspell module.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("ModuleInfo", ...)}.
Objects of this class are typically created indirectly
by a call to \code{\link{getModuleInfo}}, and directly
by a helper function (\code{createModuleInfo}) in the package

}
\section{Slots}{
  \describe{
    \item{\code{name}:}{Object of class \code{"character"}. A string
      giving the name of the module.}
    \item{\code{order_num}:}{Object of class \code{"numeric"}. ?}
    \item{\code{lib_dir}:}{Object of class \code{"character"}. The
      directory in which the module is located.}
    \item{\code{dict_dirs}:}{Object of class \code{"character"}. A
      vector giving the names o the directories. This is not working for
      modules currently so is left as \code{NA}.}
    \item{\code{dict_exts}:}{Object of class \code{"character"}. A list
      of the extensions for this module.}
  }
}
\section{Methods}{
No methods defined with class "ModuleInfo" in the signature.
}
\references{
  \url{http://aspell.sourceforge.net}
  \url{http://www.omegahat.org}
}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 \code{\link{getModuleInfo}}
}
\examples{

}
\keyword{classes}
