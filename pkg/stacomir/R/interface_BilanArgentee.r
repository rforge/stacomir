#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanArgentee = function()
{  
	quitte() # vidange de l'interface
	bilan_arg=new("BilanArgentee")
	assign("bilan_arg",bilan_arg,envir = envir_stacomi)
	
	funout(get("msg",envir=envir_stacomi)$interface_Bilan_lot.1)
	bilan_arg@dc=charge(bilan_arg@dc)
	bilan_arg@taxons=charge(bilan_arg@taxons)
	bilan_arg@stades=charge(bilan_arg@stades)
	bilan_arg@par=charge(bilan_arg@par)    
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = .GlobalEnv)
	gWidgets::add(ggroupboutons,group)
	gl=glabel(text=get("msg",envir=envir_stacomi)$interface_Bilan_lot.2,container=group)
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_arg@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.3,
			nomassign="bilan_arg_date_debut",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.5,
			decal=-2,
			affichecal=FALSE)
	choice(bilan_arg@horodate,label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.4,
			nomassign="bilan_arg_date_fin",
			funoutlabel=get("msg",envir=envir_stacomi)$interface_Bilan_lot.6,
			decal=-1,
			affichecal=FALSE)
	
	choice(bilan_arg@dc,objectBilan=bilan_arg,is.enabled=TRUE)
	
#  the choice method for RefDC will stop there and the other slots are filled with choicec
	# we only want silver eels in this bilan, and parameters length, eye diameter, pectoral length, contrast...
	choice_c(bilan_arg@taxon,2038)
	choice_c(bilan_arg@std,'AGG')
	choice_c(bilan_arg@par,c('1786','CCCC','BBBB','CONT','LINP','A111','PECT'))
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.7 => dotplot ou graphe de dispersion
	aPoint=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7,
			icon="gWidgetsRGtk2-cloud",
			handler=funpointBilanArgentee,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.7)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.11 => density
	aDensity=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11,
			icon="gWidgetsRGtk2-density",
			handler=fundensityBilanArgentee,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.11)
	#get("msg",envir=envir_stacomi)$interface_Bilan_lot.10 => boxplot
	aBoxplot=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10,
			icon="gWidgetsRGtk2-boxplot",
			handler=funboxplotBilanArgentee,
			tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.10)
	aTable=gWidgets::gaction(label="table",icon="dataframe",handler=funtableBilanArgentee,tooltip=get("msg",envir=envir_stacomi)$interface_BilanArgentee.8)
	aQuit=gWidgets::gaction(label=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9)
	
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
