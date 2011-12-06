\name{setSpellConfig}
\alias{setSpellConfig}
\alias{getSpellConfig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set options in an aspell configuration object}
\description{
 This function allows one to 
}
\usage{
setSpellConfig(conf, ..., .values = NULL)
getSpellConfig(conf = createSpellConfig(), ..., .values = NULL)
}
\arguments{
  \item{conf}{the configuration object, typically of class \code{\link{AspellConfig-class}}.}
  \item{\dots}{for interactive use, this allows the caller to specify
    \code{name = value} pairs of options that are to be set in the
    configuration object.}
  \item{.values}{the programmatic mechanism for specifiying the
    name-value pairs as a named character vector.}
}

\value{
  \code{getSpellConfig} returns a named list of the current settings for
  the option values in the configuration.

  \code{setSpellConfig} returns an integer vector giving the status of
  the setting each of the specified options.
}
\references{
  \url{http://aspell.sourceforge.net/man-html/The-Options.html#The\%20Options}
  \url{http://aspell.sourceforge.net}
}

\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
  \code{\link{getSpeller}}
  \code{\link{aspell}}  
}
\examples{

  conf = createSpellConfig()

  conf$warn
  conf$warn <- FALSE

  conf$warn

  # To append to a list.
  conf$"add-filter" = "email"

 conf$filter <- c("html", "email")
 conf$filter
 #  [1] "url"   "html"  "email"
 conf$warn

   # Will produce an error since warn doesn't support lists.
 try(conf$warn <- c("true", "false"))
}
\keyword{IO}

