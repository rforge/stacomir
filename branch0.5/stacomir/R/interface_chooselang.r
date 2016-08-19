# Nom fichier :        interface_chooselang.R    (interface)

#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_chooselang = function()
{    
	quitte() # vidange de l'interface
    group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    assign("group",group,envir = .GlobalEnv)
	add(ggroupboutons,group)
	listlang<-new("RefListe")
	listlang@listechoice=c("french","english","spanish")
	listlang@label="choose language"
	choice(listlang)
	hassingnlang=function(h,...){		
		lang<-get("refliste",envir_stacomi)@listechoice
		messages(lang)
		close(win)
		interface_graphique()
	}
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			annuler=gWidgets::gaction(handler= hassingnlang,icon = "close",label="quitter")
	)    
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	gWidgets::addSpring(group)
}
