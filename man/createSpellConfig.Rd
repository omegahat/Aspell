\name{createSpellConfig}
\alias{createSpellConfig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a new aspell configuration object.}
\description{
  This  function creates a new apsell configuration object
  and allows the caller to set options within that.
  Options can also be set later during the life
  of the configuration object.
  The configuration object is rarely needed
  directly by users who want to spell
  words.  It is useful if one wants to customize
  how the speller functions.
}
\usage{
createSpellConfig(..., .values = NULL, class = "AspellConfig")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{\dots}{the mechanism for specifying name-value options to se in
     the new configuration object. This is intended for interactive use.}
  \item{.values}{a named list or character vector giving option values for the
    new configuration object. See \code{\link{getSpellConfig}}.}
  \item{class}{the name of the class that will be instantiated and
    returned containing the reference to the C-level configuration
    value.  This argument allows the caller to easily specify
    the type of object that is desired and so this function can
    be used for derived classes of \code{\link{AspellConfig-class}}.
   }
}

\value{
  An object of class \code{class}.
}

\references{  \url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}


\seealso{
  \code{\link{getSpellInfo}}
  \code{\link{getSpellConfig}}  
}
\examples{

 conf = createSpellConfig()

 conf = createSpellConfig(lang = "en")
 conf = createSpellConfig(lang = "en", mode = "email")
}
\keyword{IO}

