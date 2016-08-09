# creation des fonctions generiques...

# la clasee baseODBC n'a plus besoin d'un baseODBC par defaut
#liste_chemins=chargecsv()
#baseODBC=liste_chemins[["baseODBC"]]
#listes de connection � la base de donnee (programmation S4)
setGeneric("choix",def=function(objet,...) standardGeneric("choix"))
setGeneric("choixmult",def=function(objet,...) standardGeneric("choixmult"))
# load method to work outside the graphical interface
setGeneric("load",def=function(objet,...) standardGeneric("load"))
setGeneric("charge",def=function(objet,...) standardGeneric("charge"))
setGeneric("charge_avec_filtre",def=function(objet,...) standardGeneric("charge_avec_filtre"))
#setGeneric("connect",def=function(objet,...) standardGeneric("connect")) # package stacomirtools
setGeneric("chargecomplement",def=function(objet,...) standardGeneric("chargecomplement"))
setGeneric("calcule",def=function(objet,...) standardGeneric("calcule"))
setGeneric("supprime",def=function(objet,...) standardGeneric("supprime"))
setGeneric("graphe",def=function(objet,...) standardGeneric("graphe"))
setGeneric("getvalue",def=function(objet,...) standardGeneric("getvalue"))
setGeneric("out",def=function(objet,...) standardGeneric("out"))
setGeneric("createmessage",def=function(objet,...) standardGeneric("createmessage"))
# nouvel environnement






#' Environment where most objects from the package are stored and then loaded
#' by the charge method
#' 
#' envir_stacomi \code{envir_stacomi <- new.env(parent = baseenv())} is the
#' environment where most object created by the interface are stored
#' 
#' 
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
envir_stacomi <- new.env(parent = emptyenv())
# cree
