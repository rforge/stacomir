

#' Methods for Function connect
#' 
#' see individual .r files for help and examples
#' 
#' 
#' @name connect-methods
#' @aliases connect-methods connect connect,ConnectionODBC-method
#' connect,RequeteODBC-method connect,RequeteODBCwhere-method
#' connect,RequeteODBCwheredate-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(object =
#' \"ConnectionODBC\")")}{connect an 'ODBC' database,and eventually leaves it
#' open for further queries, the connection may send message in the native
#' language if 'stacomiR' package is in use} \item{list("signature(object =
#' \"RequeteODBC\")")}{ connect an 'ODBC' database,performs an sql request}
#' \item{list("signature(object = \"RequeteODBCwhere\")")}{ connect an 'ODBC'
#' database,performs an sql request with where clause}
#' \item{list("signature(object = \"RequeteODBCwheredate\")")}{connect an
#' 'ODBC' database,performs an sql request with where clause for an interval }
#' }
#' @keywords methods
#' @examples
#' 
#'  showMethods("connect")
#' \dontrun{
#' object<-new("RequeteODBCwhere")
#' connect(object)
#' }
#' 
NULL





#' Class "ConnectionODBC"
#' 
#' Mother class for connection, opens the connection but does not shut it
#' 
#' 
#' @name ConnectionODBC-class
#' @aliases ConnectionODBC-class ConnectionODBC
#' @docType class
#' @note Opens the connection but does not close it. This function is intended
#' to be used with 'stacomiR' package, where the error message are collected
#' from the database It has also been programmed to work without the 'stacomiR'
#' package, as it will test for the existence of envir_stacomi environment.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("ConnectionODBC", ...)}.  \describe{
#' \item{list("baseODBC")}{Object of class \code{"vector"} The database
#' }\item{:}{Object of class \code{"vector"} The database }
#' \item{list("silent")}{Object of class \code{"logical"} The mode
#' }\item{:}{Object of class \code{"logical"} The mode }
#' \item{list("etat")}{Object of class \code{"character"} The state
#' }\item{:}{Object of class \code{"character"} The state }
#' \item{list("connection")}{Object of class \code{"ANY"} The connection
#' }\item{:}{Object of class \code{"ANY"} The connection } }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @keywords classes
#' @examples
#' 
#' showClass("ConnectionODBC")
#' \dontrun{
#'  ## this is the mother class, you don't have to use it, 
#'  ## please use requeteODBC and daughter class instead
#'  object<-new("ConnectionODBC")
#'  object@baseODBC<-c("myODBCconnection","myusername","mypassword")
#'  object@silent<-FALSE
#'  object<-connect(object)
#'  odbcClose(object@connection)
#' }
#' 
NULL





#' Class "RequeteODBC"
#' 
#' 'ODBC' Query. This class enables to retrieve data from the database.  This
#' class is inherited by RequeteODBCwhere and RequeteODBCwheredate
#' 
#' 
#' @name RequeteODBC-class
#' @aliases RequeteODBC-class RequeteODBC
#' @docType class
#' @note Inherits from ConnectionODBC
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RequeteODBC", sql=character(), query=data.frame())}.  \describe{
#' \item{list("baseODBC")}{Object of class \code{"vector"} The name, user and
#' password of the database}\item{:}{Object of class \code{"vector"} The name,
#' user and password of the database} \item{list("connection")}{Object of class
#' \code{"ANY"} The connection}\item{:}{Object of class \code{"ANY"} The
#' connection} \item{list("etat")}{Object of class \code{"character"} The state
#' of the query (Connecting, successful,...) }\item{:}{Object of class
#' \code{"character"} The state of the query (Connecting, successful,...) }
#' \item{list("silent")}{Object of class \code{"logical"} True if the query
#' must be executed silently, FALSE}\item{:}{Object of class \code{"logical"}
#' True if the query must be executed silently, FALSE}
#' \item{list("sql")}{Object of class \code{"character"} The
#' query}\item{:}{Object of class \code{"character"} The query}
#' \item{list("query")}{Object of class \code{"data.frame"} The result of the
#' query}\item{:}{Object of class \code{"data.frame"} The result of the query}
#' \item{list("open")}{Object of class \code{"logical"} Should the connection
#' remain open, choosing this ensures more rapid multiple
#' queries}\item{:}{Object of class \code{"logical"} Should the connection
#' remain open, choosing this ensures more rapid multiple queries} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ConnectionODBC}}
#' \code{\linkS4class{RequeteODBCwhere}}
#' \code{\linkS4class{RequeteODBCwheredate}}
#' @keywords classes
#' @examples
#' 
#' showClass("RequeteODBC")
#' \dontrun{
#'  object=new("RequeteODBC")
#'  object@open=TRUE 
#'  ## this will leave the connection open, 
#'  ## by default it closes after the query is sent
#'  ## the following will work only if you have configured and 'ODBC' link
#'  object@baseODBC=c("myODBCconnection","myusername","mypassword")
#'  object@sql= "select * from mytable limit 100"
#'  object<-connect(object)
#'  odbcClose(object@connection)
#'  envir_stacomi=new.env()
#'  ## While testing I like to see the output of sometimes complex queries generated by the program
#'  assign("showmerequest",1,envir_stacomi) 
#'  ## You can assign any values (here 1)
#'  ## just tests the existence of "showmerequest" in envir_stacomi
#'  object=new("RequeteODBC")
#'  object@baseODBC=c("myODBCconnection","myusername","mypassword")
#'  object@sql= "select * from mytable limit 100"
#'  object<-connect(object)
#' ## the connection is already closed, the query is printed
#' }
#' 
NULL





#' Class "RequeteODBCwhere"
#' 
#' SQL Query with WHERE and ORDER BY clauses.
#' 
#' 
#' @name RequeteODBCwhere-class
#' @docType class
#' @note Inherits from RequeteODBC the syntax is where="WHERE ..."  and
#' =vector("AND...","AND...") order_by="ORDER BY.."  The query will syntax will
#' be printed upon failure.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RequeteODBCwhere",
#' where=character(),and=vector(),order_by=character())}.  \describe{
#' \item{list("select")}{Object of class \code{"character"} The "SELECT" part
#' of the query}\item{:}{Object of class \code{"character"} The "SELECT" part
#' of the query} \item{list("where")}{Object of class \code{"character"} The
#' "WHERE" part of the query}\item{:}{Object of class \code{"character"} The
#' "WHERE" part of the query} \item{list("and")}{Object of class
#' \code{"vector"} The "AND" part of the query}\item{:}{Object of class
#' \code{"vector"} The "AND" part of the query} \item{list("order_by")}{Object
#' of class \code{"character"} The "ORDER BY" part of the query}\item{:}{Object
#' of class \code{"character"} The "ORDER BY" part of the query}
#' \item{list("sql")}{Object of class \code{"character"} The query built by
#' aggregating "select","where","and", and "order_by" slots}\item{:}{Object of
#' class \code{"character"} The query built by aggregating
#' "select","where","and", and "order_by" slots} \item{list("query")}{Object of
#' class \code{"data.frame"} The result of the query}\item{:}{Object of class
#' \code{"data.frame"} The result of the query} \item{list("open")}{Object of
#' class \code{"logical"} Should the connection remain open, choosing this
#' ensures more rapid multiple queries}\item{:}{Object of class
#' \code{"logical"} Should the connection remain open, choosing this ensures
#' more rapid multiple queries} \item{list("baseODBC")}{Object of class
#' \code{"vector"} The name, user and password of the database}\item{:}{Object
#' of class \code{"vector"} The name, user and password of the database}
#' \item{list("silent")}{Object of class \code{"logical"} TRUE if the query
#' must be executed silently, FALSE else}\item{:}{Object of class
#' \code{"logical"} TRUE if the query must be executed silently, FALSE else}
#' \item{list("etat")}{Object of class \code{"character"} The state of the
#' query (Connecting, successful,...) }\item{:}{Object of class
#' \code{"character"} The state of the query (Connecting, successful,...) }
#' \item{list("connection")}{Object of class \code{"ANY"} The database
#' connection}\item{:}{Object of class \code{"ANY"} The database connection} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ConnectionODBC}}
#' \code{\linkS4class{RequeteODBC}} \code{\linkS4class{RequeteODBCwheredate}}
#' @keywords classes
#' @examples
#' 
#' showClass("RequeteODBCwhere")
#' \dontrun{
#'  test<-0
#'  object=new("RequeteODBCwhere")
#'  object@baseODBC=c("myodbcconnection","myusername","mypassword")
#'  object@select= "select * from mytable limit 100"
#'  # assuming mycol, mycol1 and mycol2 are numeric
#'  object@where=paste(" where mycol>",test,sep="")
#'  object@and=paste(" and mycol2>",test," and mycol3<",test,sep="")
#'  object@order_by=" order by mycol1" 
#'  object<-connect(object)
#'  ## now object@sql contains the syntax of the query. 
#'  ## By changing the test variable, one can see how the
#'  ## function might be usefull
#'  ##  object@query contains the resulting data.frame
#'  }
#' 
NULL





#' Class "RequeteODBCwheredate"
#' 
#' Query with WHERE condition and overlaping dates clause.
#' 
#' 
#' @name RequeteODBCwheredate-class
#' @docType class
#' @note Inherits from RequeteODBCwhere and uses its connect method with a new
#' SetAs. This function is only usefull in databases supporting the "overlaps"
#' statement.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("RequeteODBCwheredate",
#' datedebut="POSIXlt",datefin="POSIXlt",colonnedebut="character",colonnefin="character")}.
#' \describe{ \item{list("datedebut")}{Object of class \code{"POSIXlt"} ~ The
#' starting date}\item{:}{Object of class \code{"POSIXlt"} ~ The starting date}
#' \item{list("datefin")}{Object of class \code{"POSIXlt"} ~ The ending
#' date}\item{:}{Object of class \code{"POSIXlt"} ~ The ending date}
#' \item{list("colonnedebut")}{Object of class \code{"character"} ~ The name
#' begin column}\item{:}{Object of class \code{"character"} ~ The name begin
#' column} \item{list("colonnefin")}{Object of class \code{"character"} ~ The
#' name end column}\item{:}{Object of class \code{"character"} ~ The name end
#' column} }
#' @author cedric.briand"at"eptb-vilaine.fr
#' @seealso \code{\linkS4class{ConnectionODBC}}
#' \code{\linkS4class{RequeteODBC}} \code{\linkS4class{RequeteODBCwhere}}
#' @examples
#' 
#' showClass("RequeteODBCwheredate")
#' 
NULL





#' RODBC connector class and some utilities
#' 
#' This package contains S4 wrappers for 'ODBC' connection and some utilities
#' 
#' \tabular{ll}{ Package: \tab stacomirtools\cr Type: \tab Package\cr Version:
#' \tab 0.5.2\cr Date: \tab 2017-06-24\cr License: \tab GPL (>= 2)\cr LazyLoad:
#' \tab yes\cr }
#' 
#' @name stacomirtools-package
#' @docType package
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
#' @keywords package
NULL



