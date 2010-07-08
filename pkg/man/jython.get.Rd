\name{jython.get}
\alias{jython.get}
\title{ jython.get }
\description{
  Gets a variable from the Jython environment into R
}
\usage{
  jython.get( rJython, py.var )
}
\arguments{
  \item{rJython}{ rJython interpreter as instantiated by function \code{ rJython } }
  \item{py.var}{ Name of a python variable }
}
\details{
    This function tries to get a variable from the Jython environment.
    Exceptions are caught samewhat gracefully.
}
\value{
    An R representation of the python object.
}
\references{ \url{http://code.google.com/p/simplejson} }

\examples{

rJython <- rJython()
rJython$exec( "import math" )
jython.get( rJython, "math.pi" )

}

\keyword{ manip }

