#' An interface that calls the object to build the user interface
#' @note always has to be called within a group constructed and deleted using quitte()
#' @author Cedric Briand \email{cedric.briand"at"eptb-vilaine.fr}
interface_BilanAgedemer = function()
{  
	quitte() # vidange de l'interface
	bilan_adm=new("BilanAgedemer")
	assign("bilan_adm",bilan_adm,envir = envir_stacomi)
	
	funout(gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.1))
	bilan_adm@dc=charge(bilan_adm@dc)
	bilan_adm@taxons=charge(bilan_adm@taxons)
	bilan_adm@stades=charge(bilan_adm@stades)
	bilan_adm@par=charge(bilan_adm@par)    
	
	group <- gWidgets::ggroup(horizontal=FALSE)   # doit toujours s'appeller group
	
	assign("group",group,envir = .GlobalEnv)
	gWidgets::add(ggroupboutons,group)
	gl=glabel(text="Bilan age de mer",container=group)
	# dans l'ordre 
	# dans le handler, modifier le contenu de l'object fils si il existe
	# supprimer les widgets fils si ils existent (appel de la methode delete)
	# appeller la methode choice pour l'affichage du fils si il existe
	
	
	choice(bilan_adm@horodatedebut,label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.3),
			nomassign="bilan_adm_date_debut",
			funoutlabel=gettext("Beginning date has been chosen\n"),
			decal=-2)
	choice(bilan_adm@horodatefin,label=gettext("End of timestamp"),
			nomassign="bilan_adm_date_fin",
			funoutlabel=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.6),
			decal=-1)	
	bilan_adm@dc<-choice(bilan_adm@dc,objectBilan=bilan_adm,is.enabled=TRUE)
	bilan_adm@limit1hm<-charge(bilan_adm@limit1hm,title="Limit s1 for 1sw (L(1sw)<=s1), click to edit",label="0")
	bilan_adm@limit2hm<-charge(bilan_adm@limit2hm,title="Limit s2 for 2sw (s1<L(2sw)<=s2) & L(3sw)>s2, click to edit",label="0")
#  the choice method for RefDC will stop there and the other slots are filled with choicec
	# we only want silver eels in this bilan, and parameters length, eye diameter, pectoral length, contrast...
	
	choice(bilan_adm@limit1hm)
	choice(bilan_adm@limit2hm)
	choice_c(bilan_adm@taxons,2220)
	choice_c(bilan_adm@stades,c('5','11','BEC','BER','IND'))
	choice_c(bilan_adm@par,c('1786','1785','C001'))
	#gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.7 => dotplot ou graphe de dispersion)
	aplot1=gWidgets::gaction(label="plot-1",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanAgedemer,
			action="1",
			tooltip="1")

	aplot2=gWidgets::gaction(label="plot-2",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanAgedemer,
			action="2",
			tooltip="2")
	aplot3=gWidgets::gaction(label="plot-3",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanAgedemer,
			action="3",
			tooltip="3")
	aplot4=gWidgets::gaction(label="plot-4",
			icon="gWidgetsRGtk2-cloud",
			handler=funplotBilanAgedemer,
			action="4",
			tooltip="4")
	asummary=gWidgets::gaction(label="summary",icon="dataframe",handler=funtableBilanAgedemer,tooltip="summary")
	aquit=gWidgets::gaction(label=gettext(get("msg",envir=envir_stacomi)$interface_Bilan_lot.9,icon="close", handler=quitte,tooltip=get("msg",envir=envir_stacomi)$interface_Bilan_lot.9))
	
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
	assign("ggroupboutonsbas",ggroupboutonsbas, envir=.GlobalEnv)	
	gWidgets::addSpring(group)
}
