#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanArgentee = function()
{  
	quitte() # vidange de l'interface
	bilan_arg=new("BilanArgentee")
	assign("bilan_arg",bilan_arg,envir = envir_stacomi)
	
	funout(gettext("Loading view vue_ope_lot, and choice of counting device and time steps\n",domain="R-stacomiR"))
	bilan_arg@dc=charge(bilan_arg@dc)
	bilan_arg@taxons=charge(bilan_arg@taxons)
	bilan_arg@stades=charge(bilan_arg@stades)
	bilan_arg@par=charge(bilan_arg@par)    
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = envir_stacomi)
	ggroupboutons<-get("ggroupboutons",envir=envir_stacomi)
	gWidgets::add(ggroupboutons,group)
	gl=glabel(text=gettext("Silver eel summary",domain="R-stacomiR"),container=group)
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_arg@horodatedebut,label=gettext("First date",domain="R-stacomiR"),
			nomassign="bilan_arg_date_debut",
			funoutlabel=gettext("The beginning date has been chosen\n",domain="R-stacomiR"),
			decal=-2)
	choice(bilan_arg@horodatefin,label=gettext("Last date",domain="R-stacomiR"),
			nomassign="bilan_arg_date_fin",
			funoutlabel=gettext("Ending date has been chosen\n",domain="R-stacomiR"),
			decal=-1)	
	bilan_arg@dc<-choice(bilan_arg@dc,objectBilan=bilan_arg,is.enabled=TRUE)
	
#  the choice method for RefDC will stop there and the other slots are filled with choicec
	# we only want silver eels in this bilan, and parameters length, eye diameter, pectoral length, contrast...
	choice_c(bilan_arg@taxons,2038)
	choice_c(bilan_arg@stades,'AGG')
	choice_c(bilan_arg@par,c('1786','CCCC','BBBB','CONT','LINP','A111','PECT'))
	aplot1=gWidgets::gaction(label="plot-1",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanArgentee,
			action="1",
			tooltip="1")

	aplot2=gWidgets::gaction(label="plot-2",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanArgentee,
			action="2",
			tooltip="2")
	aplot3=gWidgets::gaction(label="plot-3",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanArgentee,
			action="3",
			tooltip="3")
	aplot4=gWidgets::gaction(label="plot-4",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanArgentee,
			action="4",
			tooltip="4")
	asummary=gWidgets::gaction(label=gettext("summary",domain="R-stacomiR"),icon="dataframe",handler=funtableBilanArgentee,tooltip=gettext("summary",domain="R-stacomiR"))
	aquit=gWidgets::gaction(label=gettext("Exit",domain="R-stacomiR"),icon="close", handler=quitte,tooltip=gettext("Exit",domain="R-stacomiR"))
	
	toolbarlist <- list(    
			plot1= aplot1,
			plot2= aplot2, 
			plot3= aplot3,
			plot4= aplot4,
			summary= asummary,
			quit = aquit)
	ggroupboutonsbas = gWidgets::ggroup(horizontal=FALSE)
	gWidgets::add(ggroupboutons,ggroupboutonsbas)
	gWidgets::add(ggroupboutonsbas, gtoolbar(toolbarlist))
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=envir_stacomi)	
	gWidgets::addSpring(group)
}
