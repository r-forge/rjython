\name{rJython}
\alias{rJython}
\title{ rJython }
\description{
  Interface for using python from R via Jython
}
\usage{
  rJython( jython.jar = NULL, modules = NULL )
}
\arguments{
  \item{jython.jar}{Optional alternative location of the jython.jar file. The package is distributed with its 'jython.jar' file, but the user can invoke its own providing its path as argument to this function.}
  \item{modules}{List containing optinal paths for required modules. Packages utilizing rJython can add their own extra python modules to the 'inst' directory and then add their base path using function \code{system.file} so that they can be later found when issuing 'import module' directives.}
}
\details{
This function is not primarily intended to be run directly by users but to be used by R packages with python dependencies.
Their authors are invited to hide the internals of this package in their code at their convenience.
}
\value{
  An object of class rJava that can exectute python code as per the examples below.
}
\references{ \url{http://code.google.com/p/sympy/}, \url{http://www.jython.org/Project/}, \url{http://www.jython.org/javadoc/org/python/util/PythonInterpreter.html}
}
\examples{

rJython <- rJython()
rJython$exec( "a = 2*2" )
four <- rJython$get("a")
.jstrVal( four )

\dontrun{

# If package 'foo' contains python code that depends on module 'mod',
#   'mod' can be included in the 'inst' directory of the package.
# Then, in order to make it available to Jython, the interpreter can be
#   started as follows:

rJython <- rJython( modules = list( system.file("python-modules", package = "foo") )

}
}
\keyword{ symbolmath }

