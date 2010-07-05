#########################################################
# CGB, 20100703
#########################################################

jython.method.call <- function( rJython, py.object, py.method, ... ){

    fix.factors <- function( x ){           # Temporary fix as rjson package cannot parse factors
        if( is.factor( x ) )
            return( as.character( x ) )
        x
    }

    # foo.args <- list( ... )
    foo.args <- fix.factors( list( ... ) )


    if( is.null( names( foo.args ) ) )
        which.dict <- rep( FALSE, length( foo.args ) )
    else
        which.dict <- names( foo.args ) != ""

    n.args.vect <- sum( !which.dict )
    n.args.dict <- sum(  which.dict )

    foo.args.dict <- toJSON( foo.args[  which.dict ] )
    foo.args.vect <- toJSON( foo.args[ !which.dict ] )

    # Passing data to jython

    rJython$exec( paste( "_r_args_dict ='", foo.args.dict, "'", sep = "" ) )
    rJython$exec( paste( "_r_args_vect ='", foo.args.vect, "'", sep = "" ) )
    rJython$exec( "_r_args_dict = json.loads( _r_args_dict )" )
    rJython$exec( "_r_args_vect = json.loads( _r_args_vect )" )

    # Creating the call

    jython.command <- paste( "_r_return = ", py.object, ".", py.method, "(",
                              ifelse( n.args.vect == 1, "_r_args_vect[0]", "*_r_args_vect" ),
                              ifelse( n.args.dict == 0, ")", ", **_r_args_dict)" ), 
                              sep = "" )


    rJython$exec( jython.command )
    rJython$exec( "_r_return = json.dumps( [ _r_return ] )" )
    
    ret <- fromJSON( .jstrVal( rJython$get ( "_r_return" )) )

    if( length( ret ) == 1 ) ret <- ret[[1]]

    ret
}

