# Nom fichier :        interface_chooselang.R    (interface)

#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_chooselang = function()
{    
	quitte() # vidange de l'interface
    group = ggroup(horizontal=FALSE)   # doit toujours s'appeller group
    assign("group",group,envir = .GlobalEnv)
	add(ggroupboutons,group)
	listlang<-new("RefListe")
	listlang@listechoix=c("french","english","spanish")
	listlang@label="choose language"
	choix(listlang)
	hassingnlang=function(h,...){		
		lang<-get("refliste",envir_stacomi)@listechoix
		messages(lang)
		close(win)
		interface_graphique()
	}
	ggroupboutonsbas = ggroup(horizontal=FALSE)
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)
	add(ggroupboutons,ggroupboutonsbas)
	toolbarlist = list(
			annuler=gaction(handler= hassingnlang,icon = "close",label="quitter")
	)    
	add(ggroupboutonsbas, gtoolbar(toolbarlist))
	addSpring(group)
}
