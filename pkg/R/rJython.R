#########################################################
# CGB, 20100627
#########################################################

rJython <- function( jython.jar = NULL, modules = NULL ){
	stopifnot(require(rJava))

    # Aux function
    
	system.file. <- function(...) {
		s <- system.file(...)
		if (.Platform$OS == "windows") gsub("/", "\\", s, fixed = TRUE) else s
	}

    # Looking for jython jar 

    if( is.null( jython.jar ) ) 
        jython.jar <- Sys.getenv("RSYMPY_JYTHON")
	if (jython.jar == "")
		jython.jar <- system.file.("jython.jar", package = "rJython")

    # Starting JVM

	.jinit(jython.jar)
    rJython <- .jnew("org.python.util.PythonInterpreter")

    # Adding required python modules to the interpreter

    rJython$exec( "import sys" )
    modules <- lapply( modules, function( module ) paste( "sys.path.append(", module, ");", sep = '"' ) )
    lapply( modules, rJython$exec )
    
    rJython
}
