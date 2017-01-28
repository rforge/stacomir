#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_Bilan_carlot = function()
{  
	quitte() # vidange de l'interface
	bilan_carlot=new("Bilan_carlot")
	assign("bilan_carlot",bilan_carlot,envir = envir_stacomi)
	
	funout(gettext("Loading of the view vue_ope_lot, and choice of the counting device and of the time steps\n"))
	bilan_carlot@dc=charge(bilan_carlot@dc)
	bilan_carlot@taxons=charge(bilan_carlot@taxons)
	bilan_carlot@stades=charge(bilan_carlot@stades)
	bilan_carlot@par=charge(bilan_carlot@par)    
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = .GlobalEnv)
	gWidgets::add(ggroupboutons,group)
	gl=glabel(text=gettext("Batch summary"),container=group)
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_carlot@horodatedebut,label=gettext("Fist timestamp"),
			nomassign="bilan_carlot_date_debut",
			funoutlabel=gettext("The beginning date has been chosen\n"),
			decal=-2)
	choice(bilan_carlot@horodatefin,label=gettext("Last timestamp"),
			nomassign="bilan_carlot_date_fin",
			funoutlabel=gettext("The ending date has been chosen"),
			decal=-1)
	
	choice(bilan_carlot@dc,objectBilan=bilan_carlot,is.enabled=TRUE)
	# Les methodes choice suivantes sont passees en cascade e l'interieur des methodes choice
	#choice(bilan_carlot@taxons,is.enabled=FALSE)
	#choice(bilan_carlot@stades,is.enabled=FALSE)
	#choice(bilan_carlot@par,is.enabled=FALSE)
	#toolbarlist$Calc$handler = connect(bilanFonctionnementDC)
	#toolbarlist$Calc$icon = "dataframe"
	#getStockIcons(toolkit=guiToolkit())
	
	aPoint=gWidgets::gaction(label=gettext("dotplot"),
			icon="gWidgetsRGtk2-cloud",
			handler=funpointBilan_carlot,
			tooltip=gettext("dotplot"))

	aDensity=gWidgets::gaction(label=gettext("density"),
			icon="gWidgetsRGtk2-density",
			handler=fundensityBilan_carlot,
			tooltip=gettext("density"))
	aBoxplot=gWidgets::gaction(label=gettext("boxplot"),
			icon="gWidgetsRGtk2-boxplot",
			handler=funboxplotBilan_carlot,
			tooltip=gettext("boxplot"))
	aTable=gWidgets::gaction(label=gettext("table"),
			icon="dataframe",
			handler=funtableBilan_carlot,
			tooltip=gettext("dataframe"))
	aQuit=gWidgets::gaction(label=gettext("Exit"),
			icon="close",
			handler=quitte,tooltip="Exit")
	
	toolbarlist <- list(    
			plot=aPoint,
			density=aDensity, 
			boxplot= aBoxplot,
			table=aTable,
			Quit = aQuit)
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)	
	gWidgets::addSpring(group)
}
