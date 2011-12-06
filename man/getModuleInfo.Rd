\name{getModuleInfo}
\alias{getModuleInfo}
\title{Retrieve information about the aspell library's available modules}
\description{
  This function provides a description of each of the modules
  that are available to the aspell spell checker library.
}
\usage{
getModuleInfo(conf = createSpellConfig(), el = createModuleInfo)
}
\note{
  the \code{dict\_dirs} element will consistently be NA
  as we have been experiencing segmentation faults
  and hence crashes if this was included.  
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{conf}{the modules are potentially specific to a given
    configuration, including paths to the modules, etc.
    This is the configuration object. The default version is used
    if none is provided and tis has no special settings.}
  \item{el}{a function that is is called for each module description
    object at the C-level.  The elements of that object are passed
    as individual arguments to the function.
    These are the name, the order number, library directory name,
     collection of dictionary directories, and a collection of dictionary extensions.}
}
\value{
  A list of objects, each of which is created via a call to the
  function given as \code{el}.
}

\references{\url{http://aspell.sourceforge.net}}
\author{Duncan Temple Lang <duncan@wald.ucdavis.edu>}

\seealso{
 \code{\link{getDictInfo}}
}
\examples{
 m = getModuleInfo()
 names(m)

    # Just get the names of the modules
  getModuleInfo( el = function(name, ...) name)
}
\keyword{IO}

