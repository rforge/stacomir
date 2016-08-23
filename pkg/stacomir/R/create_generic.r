# creation des fonctions generiques...

# la clasee baseODBC n'a plus besoin d'un baseODBC par defaut
#liste_chemins=chargecsv()
#baseODBC=liste_chemins[["baseODBC"]]
#listes de connection � la base de donnee (programmation S4)
setGeneric("choice",def=function(object,...) standardGeneric("choice"))
setGeneric("choicemult",def=function(object,...) standardGeneric("choicemult"))
# load method to work outside the graphical interface
setGeneric("choice_c",def=function(object,...) standardGeneric("choice_c"))
setGeneric("charge",def=function(object,...) standardGeneric("charge"))
setGeneric("charge_avec_filtre",def=function(object,...) standardGeneric("charge_avec_filtre"))
#setGeneric("connect",def=function(object,...) standardGeneric("connect")) # package stacomirtools
setGeneric("charge_complement",def=function(object,...) standardGeneric("charge_complement"))
setGeneric("calcule",def=function(object,...) standardGeneric("calcule"))
setGeneric("supprime",def=function(object,...) standardGeneric("supprime"))
setGeneric("cumplot",def=function(object,...) standardGeneric("cumplot"))
setGeneric("plot1",def=function(object,...) standardGeneric("plot1"))
setGeneric("getvalue",def=function(object,...) standardGeneric("getvalue"))
setGeneric("out",def=function(object,...) standardGeneric("out"))
setGeneric("createmessage",def=function(object,...) standardGeneric("createmessage"))
# nouvel environnement






#' Environment where most objects from the package are stored and then loaded
#' by the charge method
#' 
#' envir_stacomi \code{envir_stacomi <- new.env(parent = baseenv())} is the
#' environment where most object created by the interface are stored
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
"envir_stacomi"
