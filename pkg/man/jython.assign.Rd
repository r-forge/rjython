\name{jython.assign}
\alias{jython.assign}
\title{ jython.assign }
\description{
  Assigns to a python variable name the python equivalent of an R object
}
\usage{
  jython.assign( rJython, var.name, value )
}
\arguments{
  \item{rJython}{ rJython interpreter as instantiated by function \code{ rJython } }
  \item{var.name}{ a character string containing a valid python variable name }
  \item{value}{ an R object whose equivalent wants to be assigned to the variable in python }
}
\details{
    TBA
}
\value{
    None.
}
\references{ \url{http://code.google.com/p/simplejson} }

\examples{

rJython <- rJython()

a <- 1:4
b <- 5:8
jython.assign( rJython, "a", a )
jython.exec( rJython, "len( a )" )


}

\keyword{ manip }

