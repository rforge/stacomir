#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_Bilan_carlot = function()
{  
	quitte() # vidange de l'interface
	bilan_carlot=new("Bilan_carlot")
	assign("bilan_carlot",bilan_carlot,envir = envir_stacomi)
	
	funout(gettext("Loading of the view vue_ope_lot, and choice of the counting device and of the time steps\n",domain="R-stacomiR"))
	bilan_carlot@dc=charge(bilan_carlot@dc)
	bilan_carlot@taxons=charge(bilan_carlot@taxons)
	bilan_carlot@stades=charge(bilan_carlot@stades)
	bilan_carlot@par=charge(bilan_carlot@par)    
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = envir_stacomi)
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	gWidgets::add(ggroupboutons,group)
	gl=glabel(text=gettext("Batch summary",domain="R-stacomiR"),container=group)
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_carlot@horodatedebut,label=gettext("Fist timestamp",domain="R-stacomiR"),
			nomassign="bilan_carlot_date_debut",
			funoutlabel=gettext("The beginning date has been chosen\n",domain="R-stacomiR"),
			decal=-2)
	choice(bilan_carlot@horodatefin,label=gettext("Last timestamp",domain="R-stacomiR"),
			nomassign="bilan_carlot_date_fin",
			funoutlabel=gettext("The ending date has been chosen",domain="R-stacomiR"),
			decal=-1)
	
	choice(bilan_carlot@dc,objectBilan=bilan_carlot,is.enabled=TRUE)
	# Les methodes choice suivantes sont passees en cascade e l'interieur des methodes choice
	#choice(bilan_carlot@taxons,is.enabled=FALSE)
	#choice(bilan_carlot@stades,is.enabled=FALSE)
	#choice(bilan_carlot@par,is.enabled=FALSE)
	#toolbarlist$Calc$handler = connect(bilanFonctionnementDC)
	#toolbarlist$Calc$icon = "dataframe"
	#getStockIcons(toolkit=guiToolkit())
	
	aPoint=gWidgets::gaction(label=gettext("dotplot",domain="R-stacomiR"),
			icon="gWidgetsRGtk2-cloud",
			handler=funpointBilan_carlot,
			tooltip=gettext("dotplot",domain="R-stacomiR"))
	aDensity=gWidgets::gaction(label=gettext("density",domain="R-stacomiR"),
			icon="gWidgetsRGtk2-density",
			handler=fundensityBilan_carlot,
			tooltip=gettext("density",domain="R-stacomiR"))
	aBoxplot=gWidgets::gaction(label=gettext("boxplot",domain="R-stacomiR"),
			icon="gWidgetsRGtk2-boxplot",
			handler=funboxplotBilan_carlot,
			tooltip=gettext("boxplot",domain="R-stacomiR"))
	aTable=gWidgets::gaction(label=gettext("table",domain="R-stacomiR"),
			icon="dataframe",
			handler=funtableBilan_carlot,
			tooltip=gettext("dataframe",domain="R-stacomiR"))
	aQuit=gWidgets::gaction(label=gettext("Exit",domain="R-stacomiR"),
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
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)	
	gWidgets::addSpring(group)
}
