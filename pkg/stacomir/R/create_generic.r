# creation des fonctions generiques...

# la clasee baseODBC n'a plus besoin d'un baseODBC par defaut
#liste_chemins=chargexml()
#baseODBC=liste_chemins[["baseODBC"]]
#listes de connection à la base de donnee (programmation S4)
setGeneric("choix",def=function(objet,...) standardGeneric("choix"))
setGeneric("charge",def=function(objet,...) standardGeneric("charge"))
setGeneric("charge_avec_filtre",def=function(objet,...) standardGeneric("charge_avec_filtre"))
#setGeneric("connect",def=function(objet,...) standardGeneric("connect")) # package stacomirtools
setGeneric("chargecomplement",def=function(objet,...) standardGeneric("chargecomplement"))
setGeneric("calcule",def=function(objet,...) standardGeneric("calcule"))
setGeneric("supprime",def=function(objet,...) standardGeneric("supprime"))
setGeneric("graphe",def=function(objet,...) standardGeneric("graphe"))
setGeneric("getvalue",def=function(objet,...) standardGeneric("getvalue"))
setGeneric("createmessage",def=function(objet,...) standardGeneric("createmessage"))
# nouvel environnement
envir_stacomi <- new.env(parent = baseenv())
# cree